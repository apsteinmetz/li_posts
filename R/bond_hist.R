library(tidyverse)
library(lubridate)
library(tsibble)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(slider)
library(jrvFinance)
library(ggridges)


load(file="data/shiller.rdata")

shiller_xts <- shiller %>%
   select(date,spx,bondret) %>%
   mutate(date = ceiling_date(as.Date(date)+1,
                              unit="month")-1) %>%
   as_tibble() %>%
   column_to_rownames(var="date") %>%
   as.xts() %>%
   identity()


shiller_xts$bondret |>
   charts.RollingPerformance(width = 12)

shiller <- shiller |>
   mutate(year = year(date), month = month(date),.before=date)

shiller$rollret12 <- slider::slide_dbl(
   1 + shiller$bondret,
   prod,
   .before = 12 - 1,
   .complete = TRUE
) - 1

shiller$rollret24 <- slider::slide_dbl(
   1 + shiller$bondret,
   prod,
   .before = 24 - 1,
   .complete = TRUE
) - 1

shiller %>% ggplot(aes(date,rollret12)) + geom_line() +
   geom_hline(yintercept = 0) +
   scale_y_continuous(labels = scales::percent) +
   labs(title = "Worst Year Ever For Treasuries",
        subtitle = "Trailing 12-month Total Return For 10-Year Tsy Note
        Data Through June 2022",
        y= "Total Return, Last 12 Months",
        caption = "Data Source:Robert Shiller")



# download security yields
sec_list <- c("DFEDTAR","DFEDTARL","INTDSRUSM193N","DGS2","DGS5","DGS10","DGS30")
getSymbols(sec_list,src="FRED")
# convert to tibbles

# convert to monthly periodicity
convert_xts <- function(sec){
   as_tibble(eval((parse(text=sec))),rownames="date") |>
      mutate(date = as.Date(date)) |>
      na.omit() |>
      mutate(year_month = yearmonth(date),.before = date) |>
      group_by(year_month) |>
      slice_tail(n=1) |>
      select(-date)
   }


# duration
quick_dur <- Vectorize(function(yield,mat = 10){
   bond.duration(settle = Sys.Date(),
                 mature = Sys.Date() + (365*mat),
                 coupon = yield/100,
                 yield = yield/100)

})

wrangle_rates <- function() {
  temp <- map(sec_list, convert_xts)
  rates <- temp[[1]]
  for (n in 2:length(temp)) {
    rates <- full_join(rates, temp[[n]], by = "year_month")
  }
  rates <- rates |>
    ungroup() %>%
    arrange(year_month) |>
    mutate(FEDTAR = ifelse(is.na(DFEDTARL), DFEDTAR, DFEDTARL)) |>
    mutate(FEDTAR = ifelse(is.na(FEDTAR), INTDSRUSM193N, FEDTAR)) |>
    select(-INTDSRUSM193N,-DFEDTARL,-DFEDTAR) %>%
    filter(!is.na(DGS10)) %>%
    rownames_to_column(var = "Idx") %>%
    mutate(Idx = as.numeric(Idx)) %>%
    mutate(duration = quick_dur(DGS10, mat = 10)) %>%
    mutate(slope = DGS10 - FEDTAR)   %>%
    left_join(select(rename(shiller, year_month = date), year_month, CPI_YOY)) |>
    identity()
  names(rates) <- names(rates) %>% str_replace_all("DGS","TSY")
  return(rates)
}


# source("r/find_extremes.r")
# extremes <- bind_rows(locate_xtrem(rates$TSY10,last = FALSE),
#                       locate_xtrem(rates$TSY10,last = TRUE)) %>%
#    arrange(Idx)
#
# extremes <- rates %>%
#    select(year_month,Idx) %>%
#    right_join(extremes,by = "Idx")


# --------------------------------
# returns after yield curve inverts
est_mo_return <- function(start_yield,end_yield, duration){
   ret <- (start_yield - end_yield)/100 * duration +
      start_yield/1200
   return(ret)

}
# --------------------------------

rates <-wrangle_rates()

bond_ret_shiller <- select(rename(shiller, year_month = date),
                           year_month, bondret)

rates <- rates %>% mutate(bondret_est =
                   est_mo_return(lag(TSY10),TSY10,lag(duration))) %>%
   left_join(bond_ret_shiller)



# iterate over various trade strategies
TRIGGER = 0
HOLD_PER = 6

# --------------------------------
test_trade <- function(TRIGGER = 0, HOLD_PER  = 12,ANNUALIZE = TRUE) {
   results  <- rates %>%
      mutate(trade = if_else(slope < TRIGGER,
                             if_else(lag(slope) > TRIGGER, TRUE, FALSE),
                             FALSE))
   results$holdret <- slider::slide_dbl(1 + rates$bondret_est,
                                        prod,
                                        .after = HOLD_PER - 1,
                                        .complete = TRUE) - 1
   if (ANNUALIZE){
     results$holdret <- (1 + results$holdret) ^ (12/HOLD_PER) - 1
   }
   results$HOLD_PER = HOLD_PER
   results <- results %>%
      filter(trade == TRUE) %>%
      remove_missing()

   h <- hist(results$holdret, plot = FALSE)
   density <- tibble(mids = h$mids, density = h$density)
   return(tibble(
      hold_per = HOLD_PER,
      trigger = TRIGGER,
      mean_ret = mean(results$holdret),
      median_ret = median(results$holdret),
      sd_ret = sd(results$holdret),
      num_trades = nrow(results),
      loss_freq = sum(results$holdret < 0)/num_trades,
      density = list(density),
      result = list(results)
   ))
}
# --------------------------------
temp <- test_trade(TRIGGER=1,HOLD_PER = 6)



grid_trade <- function(ANNUALIZE = TRUE) {
  results = list()
  for (hld in c(6, 12, 18, 24, 36)) {
    for (trg in c(1.00, .50, 0, -.50, -1.00)) {
      results <- bind_rows(results, test_trade(trg, hld, ANNUALIZE = ANNUALIZE))
    }
  }
  return(results)
}


annualize = TRUE
results <- grid_trade(ANNUALIZE = annualize)
is_annl <- if_else(annualize,"Annualized","Unannualized")

results %>% filter(hold_per == 6) %>%
  select(trigger,num_trades) %>%
  ggplot(aes(trigger,num_trades)) + geom_col() +
  labs(title = "Steep Inversions are Rare",
       subtitle = "So we don't have a big sample size.",
       x = "Curve Shape Trigger (BP)",
       y = "Number of Trades Triggered") +
  coord_flip()


# plot median return grid --------------------------------
results %>%
  ggplot(aes(as.factor(hold_per),trigger*100,fill = median_ret)) + geom_tile() +
  scale_fill_viridis_c(option = "C",labels = scales::percent) +
  labs(title = "Trading Strategies",
       subtitle = glue::glue("{is_annl} Returns"),
       fill = "Median\nReturn %",
       y = "Curve Shape Trigger (BP)",
       x = "Holding Period (Months)")

# plot mean return grid --------------------------------
results %>%
  ggplot(aes(as.factor(hold_per),trigger*100,fill = mean_ret)) + geom_tile() +
  scale_fill_viridis_c(option = "C",labels = scales::percent) +
  labs(title = "Trading Strategies",
       subtitle = glue::glue("{is_annl} Returns"),
       fill = "Mean\nReturn %",
       y = "Curve Shape Trigger (BP)",
       x = "Holding Period (Months)")
# plot facet return distribution --------------------------------
results %>%
  unnest(result) %>%
  select(hold_per, trigger,holdret) %>%
  mutate(trigger = as_factor(trigger*100)) %>%
  mutate(hold_per = as_factor(hold_per)) %>%
#  filter(hold_per == 6) %>%
  ggplot(aes(y=trigger,x=holdret,fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~hold_per) +
  scale_fill_viridis_c(name = "Returns", option = "C",labels = scales::percent) +
  labs(title = "Trading Strategies",
       subtitle = glue::glue("{is_annl} Returns"),
       fill = "Trigger (BP)",
       y = "Curve Shape Trigger (BP)",
       x = glue::glue("Distribution of {is_annl} Return Outcomes"))

# plot return distribution --------------------------------
results %>%
  unnest(result) %>%
  select(hold_per, trigger,holdret) %>%
  mutate(trigger = as_factor(trigger*100)) %>%
  mutate(hold_per = as_factor(hold_per)) %>%
  filter(hold_per == 6) %>%
  ggplot(aes(y=trigger,x=holdret,fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5) +
  scale_x_continuous(labels = scales::percent) +
#  facet_wrap(~hold_per,scales = "free") +
  scale_fill_viridis_c(name = "Returns", option = "C",labels = scales::percent) +
  labs(title = "Trading Strategies",
       subtitle = glue::glue("{is_annl} Returns"),
       fill = "Trigger (BP)",
       y = "Curve Shape Trigger (BP)",
       x = glue::glue("Distribution of {is_annl} Return Outcomes"))

