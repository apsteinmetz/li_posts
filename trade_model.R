# ML approach to bond trades
library(tidyverse)
library(lubridate)
library(quantmod)
library(jrvFinance)
library(ggridges)


# functions ---------------------------------------------
# duration
quick_dur <- Vectorize(function(yield,mat = 10){
  bond.duration(settle = Sys.Date(),
                mature = Sys.Date() + (365*mat),
                coupon = yield/100,
                yield = yield/100)

})


# approx period return
approx_return <- function(start_yield,end_yield, duration){
  ret <- (start_yield - end_yield)/100 * duration +
    start_yield/1200
  return(ret)

}

condense_time <- function(dt , pd = "week"){
  # assumes 'date' is name of date field
  dt <- dt %>% mutate(new_dt = ceiling_date(date,unit = pd)) %>%
    group_by(new_dt) %>%
    slice_tail(n=1) %>%
    ungroup() %>%
    select(-new_dt)
  return(dt)
}

# download security yields
sec_list <- c("DFEDTAR","DFEDTARL","INTDSRUSM193N","DGS2","DGS5","DGS10","DGS30","CPIAUCSL")
# getSymbols(sec_list,src="FRED")

rates_raw <- tidyquant::tq_get(sec_list,
                               get="economic.data",
                               from = "1970-01-01")


rates <- rates_raw %>%
  pivot_wider(names_from = "symbol", values_from = "price") %>%
  arrange(date) %>%
  fill(everything(),.direction = "down") %>%
  mutate(FEDTAR = ifelse(is.na(DFEDTARL), DFEDTAR, DFEDTARL)) |>
  mutate(FEDTAR = ifelse(is.na(FEDTAR), INTDSRUSM193N, FEDTAR)) |>
  select(-INTDSRUSM193N,-DFEDTARL,-DFEDTAR) %>%
  condense_time("week") %>%
  drop_na() %>%
  rename_with(~str_replace(.x,"DGS","TSY")) %>%
  rename_with(~"CPI",starts_with("CPI")) %>%
  identity()


# add features -----------------------------------
LAG = 4 # weeks
HOLD_PER = 24 #weeks, 6 months
feature_set <- rates %>%
  mutate(slope = TSY10 - FEDTAR)   %>%
  mutate(across(
    where(is_double),
    .fns = ~ .x - lag(.x, LAG),
    .names = "{.col}_delta_{LAG}"
  )) %>%
  select(-starts_with("date_delta")) %>%
  mutate(dur_30 = quick_dur(TSY30, mat = 30)) %>%
  mutate(pd_ret_30 = approx_return(lag(TSY30),TSY30,dur_30)) %>%
  drop_na() %>%
  mutate(cumret = cumprod(1+pd_ret_30)) %>%
  mutate(trail_ret = cumret/lag(cumret,LAG)-1) %>%
  mutate(fut_ret = lead(cumret,HOLD_PER)/cumret-1) %>%
  select(-pd_ret_30,-cumret)


threshold = 0.10 # we want 10%
outcomes <- feature_set %>%
  transmute(date,fut_ret) %>%
  mutate(winner = fut_ret < threshold)

feature_set <- feature_set %>%
  select(-fut_ret)

