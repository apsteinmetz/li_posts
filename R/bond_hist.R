library(tidyverse)
library(lubridate)
library(tsibble)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(slider)
library(jrvFinance)


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


# plot duration over time
sec_list <- c("DFEDTAR","DFEDTARL","INTDSRUSM193N","DGS10")
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

temp <- map(sec_list,convert_xts)
rates <- temp[[1]]
for (n in 2 : length(temp)){
   rates <- full_join(rates,temp[[n]],by="year_month")}
rates <- rates |>
   ungroup() %>%
   arrange(year_month) |>
   mutate(FEDTAR = ifelse(is.na(DFEDTARL),DFEDTAR,DFEDTARL),
             TSY10 = DGS10) |>
   transmute(year_month,FEDTAR = ifelse(is.na(FEDTAR),INTDSRUSM193N,FEDTAR),
          TSY10) |>
   filter(!is.na(TSY10)) %>%
   rownames_to_column(var="Idx") %>%
   mutate(Idx = as.numeric(Idx)) %>%
   mutate(duration = quick_dur(TSY10,mat = 10)) %>%
   mutate(slope = TSY10 - FEDTAR)   %>%
   left_join(select(rename(shiller,year_month=date),year_month,CPI_YOY)) |>
   identity()


# source("r/find_extremes.r")
# extremes <- bind_rows(locate_xtrem(rates$TSY10,last = FALSE),
#                       locate_xtrem(rates$TSY10,last = TRUE)) %>%
#    arrange(Idx)
#
# extremes <- rates %>%
#    select(year_month,Idx) %>%
#    right_join(extremes,by = "Idx")



rates |> ggplot(aes(year_month,-duration)) + geom_col() +
   ylim(-10,0) +
   labs(title = "Bonds Have Been Getting Riskier for 40 Years",
        subtitle = "Price Movement for a 10Y Tsy
        given a 100 Basis Point Yield Rise",
        y="Potential Price Decline (%)",
        x = "")

rates |> ggplot(aes(year_month,TSY10)) + geom_line() +
   labs(title = "Bonds Have Given US A Great Ride for 40 Years",
        subtitle = "10-Year Treasury Note Yield",
        y="Yield (%)",
        x = "")

rates |> ggplot(aes(year_month,FEDTAR)) + geom_line() +
   labs(title = "Today's Tightening Cycle Is Not Extreme",
        subtitle = "Fed Funds Rate (and Proxies) With CPI",
        caption = "Data Source: fred.stlouisfed.org",
        y="Funds Rate (%) and CPI YOY (In Red)",
        x = "") +
   geom_line(aes(year_month,CPI_YOY),color="red")

rates |> ggplot(aes(year_month,TSY10)) + geom_line() +
   labs(title = "Bond Yields Lead The Fed",
        subtitle = "Treasury 10-Year vs. the Funds Rate",
        caption = "Data Source: fred.stlouisfed.org",
        y="Tsy 10Y Yield and Funds Rate (In Red)",
        x = "") +
   geom_line(aes(year_month,FEDTAR),color="red")

rates |> ggplot(aes(year_month,TSY10-FEDTAR)) +
   geom_hline(yintercept = 0) +
   geom_rect(aes(ymax = 0,
                 ymin = min(TSY10-FEDTAR),
                 xmin = min(year_month),
                 xmax = max(year_month)),
             fill = "lightgreen") +
   geom_line() +
   labs(title = "Ringing The Bell",
        subtitle = "10-Year Minus Fed Funds Rate (and Proxies)",
        y="Slope of the Curve",
        x = "")





# --------------------------------
# returns after yield curve inverts
est_mo_return <- function(start_yield,end_yield, duration){
   ret <- (start_yield - end_yield)/100 * duration +
      start_yield/1200
   return(ret)

}
# --------------------------------


bond_ret_shiller <- select(rename(shiller, year_month = date),
                           year_month, bondret)

rates <- rates %>% mutate(bondret_est =
                   est_mo_return(lag(TSY10),TSY10,lag(duration))) %>%
   left_join(bond_ret_shiller)



# iterate over various trade strategies
TRIGGER = 0
HOLD_PER = 6

# --------------------------------
test_trade <- function(TRIGGER = 0, HOLD_PER  = 12) {
   results  <- rates %>%
      mutate(trade = if_else(slope < TRIGGER,
                             if_else(lag(slope) > TRIGGER, TRUE, FALSE),
                             FALSE))
   results$holdret <- slider::slide_dbl(1 + rates$bondret_est,
                                        prod,
                                        .after = HOLD_PER - 1,
                                        .complete = TRUE) - 1
   results$holdret <- (1 + results$holdret) ^ (12/HOLD_PER) - 1
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
      density = list(density),
      result = list(results)
   ))
}
# --------------------------------
temp <- test_trade(TRIGGER=1,HOLD_PER = 6)



results = list()
for (hld in c(6,12,18,24,36)){
   for (trg in c(1.00,.50,0,-.50,-1.00)){
      results <- bind_rows(results,test_trade(trg,hld))
   }
}

results %>%
   ggplot(aes(as.factor(hold_per),trigger*100,fill = median_ret)) + geom_tile() +
   labs(title = "Trading Strategies",
        y = "Curve Shape Trigger (BP)",
        x = "Holding Period (Months)")
