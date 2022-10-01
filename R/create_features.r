# ML approach to bond trades
library(tidyverse)
library(lubridate)
library(quantmod)
library(tidyquant)
library(jrvFinance)
library(timetk)


# MAKE FEATURE SET --------------------------------------

# functions ---------------------------------------------
# Duration. Assumes par coupon.
quick_dur <- Vectorize(function(yield,mat = 10){
  bond.duration(settle = Sys.Date(),
                mature = Sys.Date() + (365*mat),
                coupon = yield/100,
                yield = yield/100)

})


# approx period return
approx_return <- Vectorize(function(start_yield,
                          end_yield,
                          duration,
                          hold_per = 1, #month
                          px_only = FALSE){
  ret <- (start_yield - end_yield)/100 * duration +
    ifelse(px_only,0, (start_yield/1200)*hold_per)
  return(ret)

})


# reduce periodicity
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
sec_list <- c("DFEDTAR","DFEDTARL","INTDSRUSM193N",
              "DGS2","DGS5","DGS10","DGS30","CPALTT01USM659N")
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
  rename_with(~"CPI",starts_with("CPAL")) %>%
  identity()


# add features -----------------------------------
LAG = 4 # weeks trailing data
HOLD_PER = 24 #weeks, 6 months ahead
feature_set <- rates %>%
  mutate(slope = TSY10 - FEDTAR)   %>%
  mutate(across(
    where(is_double),
    .fns = ~ .x - lag(.x, LAG),
    .names = "{.col}_delta_{LAG}"
  )) %>%
  select(-starts_with("date_delta")) %>%
  mutate(dur_30 = quick_dur(TSY30, mat = 30)) %>%
  # calc price only return of 30 year, no coupon
  mutate(pd_ret_30 = approx_return(lag(TSY30),TSY30,dur_30,
                                   hold_per = .25, # 1 week
                                   px_only = TRUE)) %>%
  drop_na() %>%
  identity()


# add things we might want to predict
# create serial leads for 10-year leads
outcome_set <- feature_set %>%
  mutate(cumret = cumprod(1+pd_ret_30)) %>%
  mutate(fut_ret = lead(cumret,HOLD_PER)/cumret-1) %>%
  select(-pd_ret_30,-cumret) %>%
  timetk::tk_augment_leads(TSY10,.lags = -round(c(1:6)*4)) %>%
  select(date,TSY10,last_col(0:6)) %>% # we've added 7 features
  mutate(across(contains("lead"),.fns = function(x) x - TSY10)) %>%
  select(-TSY10) %>%
  identity()


save(feature_set,file="data/feature_set.rdata")
save(outcome_set,file="data/outcome_set.rdata")
