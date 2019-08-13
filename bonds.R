#Install wbstats package, call libraries
install.packages("wbstats")
library(wbstats)
library(ggplot2)

#Search indicators on Databank
wbsearch(pattern = "average interest on new external debt")

#Create list of Sub-Saharan Africa countries
SSA <- c("AGO", "CIV", "ETH", "GAB", "GHA", "KEN", "MOZ", "NGA", "RWA", "SEN", "TZA", "ZMB")

#Create list of indicators: PPG bond disbursement, Average interest rate private creditors, Average interest official creditors
indicators <- c("DT.DIS.PBND.CD", "DT.INR.PRVT", "DT.INR.OFFT")

#Fetch data from WB API, specifying countries, indicators, start/end dates and put into dataframe
bond_data_SSA <- wb(country = SSA, indicator = indicators, startdate = 2007, enddate = 2017, POSIXct = TRUE)
df <- data.frame(bond_data_SSA)
keeps <- c("date", "value", "indicator", "country")
#df <- df[keeps]


#Subset ppg bond disbursement into dataframe, format units to thousands, subset
ppg_bond_dis <- subset(df, indicatorID == "DT.DIS.PBND.CD")
ppg_bond_dis$disbursement <- ppg_bond_dis$value/1000
ppg_bond_dis <- subset(ppg_bond_dis, select = c("date", "indicator", "country", "disbursement"))
View(ppg_bond_dis)

#Subset Average interest rate private creditors into dataframe
avg_int_pri <- subset(df, indicatorID == "DT.INR.PRVT")
avg_int_pri <- subset(avg_int_pri, select = c("date", "indicator", "country", "value"))
View(avg_int_pri)

#Subset Average interest official creditors into dataframe
avg_int_off <- subset(df, indicatorID == "DT.INR.OFFT")

