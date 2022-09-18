# make nominal S&P return from price level and dividend
# http://www.econ.yale.edu/~shiller/data.htm
library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(tsibble)
library(xts)
# cloud
# library(googledrive)
# library(googlesheets4)

# shiller_raw <- read_sheet(drive_get("shiller CAPE"),sheet="Data",skip=7)
# shiller_raw <- readxl::read_xls("data/ie_data.xls",sheet="Data",skip=7)

xl_url <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
GET(xl_url, write_disk(tf <- tempfile(fileext = ".xls")))

shiller_raw <- read_xls(tf,sheet = "Data",skip = 7)
unlink(tf) ; rm(tf)
load("data/shiller_names.rdata")

# want to do smart renaming. not working yet
#renamer <- function(row,df,namedf) {
#   df <- rename_with(df,namedf$new_name[row],.cols = namedf$orig_name[row])
#   return(df)
#}
#1:22 %>% map(renamer,shiller_raw,shiller_names)

shiller <- shiller_raw
names(shiller) <- shiller_names$new_name

shiller <- shiller %>%
   mutate(spx = (P + D / 12) / lag(P) - 1, .before = P) %>%
   mutate(bondret = bondret - 1,) %>%
   filter(!is.na(date)) %>%
   mutate(date = paste0(format(date, digits = 6), ".01")) %>%
   mutate(date = yearmonth(as.Date(date, format = "%Y.%m.%d"))) %>%
   mutate(CPI_YOY = (CPI/lag(CPI,12)-1)*100,.after = CPI)

save(shiller,file="data/shiller.rdata")
load(file="data/shiller.rdata")

shiller_xts <- shiller %>%
   select(date,spx,bondret) %>%
   mutate(date = ceiling_date(as.Date(date)+1,
                               unit="month")-1) %>%
   as_tibble() %>%
   column_to_rownames(var="date") %>%
   as.xts() %>%
   identity()
