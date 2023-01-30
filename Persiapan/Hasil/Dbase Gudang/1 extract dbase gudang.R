rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidyr)

nama_file = "Routing Explore.xlsx"
sht       = excel_sheets(nama_file)

df = read_excel(nama_file,sheet = sht[3]) %>% janitor::clean_names()

write.csv(df,"dbase gudang.csv",row.names = F)
