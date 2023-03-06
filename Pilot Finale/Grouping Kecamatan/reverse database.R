rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(readxl)

# set working directory
setwd("~/NTOP/Pilot Finale/Grouping Kecamatan")

# kita akan kembalikan data xlsx ke dalam environment R ini
file = "grouping.xlsx"

#import
df   = read_excel(file) %>% janitor::clean_names() %>% 
       filter(provinsi != "provinsi") %>% 
       filter(!is.na(provinsi))

# kita akan pilih yang berguna saja
df   =  df %>% select(provinsi,kota_kab,kecamatan,kode_pos,cluster_final)

# kita save rute nya
save(df,file = "routing dbase.rda")
