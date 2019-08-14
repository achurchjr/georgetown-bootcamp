#Install wbstats package, call libraries
library(wbstats)
library(tidyverse)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)

source("C:\\Users\\WB537822\\Documents\\styles_viz.R")

install.packages(c("countrycode","ggalluvial","ggmosaic","pdftools","png","proj4"  ,"readstata13","rgdal", "OECD"))

devtools::install_github("worldbank/wbgviz", subdir = "wbgdata")
devtools::install_github("worldbank/wbgviz", subdir = "wbgcharts")
devtools::install_github("worldbank/wbgviz", subdir = "wbgmaps")
devtools::install_github("worldbank/wbgviz", subdir = "wbggeo")


fig_sdg1_poor_number_map <- function(years = 2010:2013) {
  indicators = c("SI.POV.DDAY", "SP.POP.TOTL")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    year=years,
    removeNA = TRUE
    # Comment the next two lines to use live API data
    #offline = "only",
    #offline.file = "inputs/cached_api_data/fig_sdg1_poor_number_map.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup
  
  df$count <- df$SI.POV.DDAY/100 * df$SP.POP.TOTL
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      breaks = c(10e+06, 50e+06,200e+06)
      wbg_bubble_map(df, wbgmaps[[quality]], style, "count", 
                     breaks, 
                     max_size = 0.8, 
                     labels = millions())
      
    }, 
    aspect_ratio = 1.5, 
    title = "Populous countries such as China, India, Indonesia, and Bangladesh are home to a significant share of the total number of people living in extreme poverty.", 
    subtitle = wbg_name(indicator="Number of people living on less than $1.90 a day (2011 PPP)",mrv=years,denom="millions"), 
    source_url = "Source: World Bank PovcalNet. World Development Indicators (SI.POV.DDAY; SP.POP.TOTL)"
  )
}


fig_sdg1_poor_number_map()

#Search indicators on Databank
wbsearch(pattern = "average interest on new external debt")

#Create list of Sub-Saharan Africa countries
SSA <- c("AGO", "CIV", "ETH", "GAB", "GHA", "KEN", "MOZ", "NGA", "RWA", "SEN", "TZA", "ZMB")

#Create list of indicators: PPG bond disbursement, Average interest rate private creditors, Average interest official creditors, avg maturity on new external debt com - official,
#and avg maturity on new external debt com - private
indicators <- c("DT.DIS.PBND.CD", "DT.INR.PRVT", "DT.INR.OFFT", "DT.MAT.OFFT", "DT.MAT.PRVT")

#Fetch data from WB API, specifying countries, indicators, start/end dates and put into dataframe
bond_data_SSA <- wb(country = SSA, indicator = indicators, startdate = 2007, enddate = 2017, POSIXct = TRUE)
df <- data.frame(bond_data_SSA)

#Subset ppg bond disbursement into dataframe, format units to thousands, subset
ppg_bond_dis <- subset(df, indicatorID == "DT.DIS.PBND.CD")
ppg_bond_dis$disbursement <- ppg_bond_dis$value/1000
ppg_bond_dis <- subset(ppg_bond_dis, select = c("date", "indicator", "country", "disbursement"))
#View(ppg_bond_dis)

#Subset Average interest rate private creditors into dataframe
avg_int_pri <- subset(df, indicatorID == "DT.INR.PRVT")
avg_int_pri <- subset(avg_int_pri, select = c("date", "indicator", "country", "value"))
#View(avg_int_pri)

#Subset Average interest official creditors into dataframe
avg_int_off <- subset(df, indicatorID == "DT.INR.OFFT")
avg_int_off <- subset(avg_int_off, select = c("date", "indicator", "country", "value"))
#View(avg_int_off)

#Subset avg maturity on new external debt - official
maturity_official <- subset(df, indicatorID == "DT.MAT.OFFT")
maturity_official <- subset(maturity_official, select = c("date", "indicator", "country", "value"))
#View(maturity_official)

#Subset avg maturity on new external debt - private
maturity_private <- subset(df, indicatorID == "DT.MAT.PRVT")
maturity_private <- subset(maturity_private, select = c("date", "indicator", "country", "value"))
#View(maturity_private)

