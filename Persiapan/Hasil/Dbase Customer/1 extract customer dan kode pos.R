rm(list=ls())

# libraries
library(dplyr)
library(readxl)
library(tidyr)

nama_file = "Routing Explore.xlsx"
sht       = excel_sheets(nama_file)
df        = read_excel(nama_file,sheet = sht[1]) %>% janitor::clean_names()

df %>% 
  select(customer,kode_pos) %>% 
  write.csv("dbase customer kode pos.csv",row.names = F)
