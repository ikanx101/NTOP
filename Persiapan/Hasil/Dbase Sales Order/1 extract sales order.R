rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidyr)

# kita ambil file
nama_file = "Routing Explore.xlsx"
sht       = excel_sheets(nama_file)
df        = read_excel(nama_file,sheet = sht[5],skip = 1) %>% janitor::clean_names() 

# kita akan pilih terlebih dahulu
df %>% 
  select(nama_customer,sales_order,sales_order,kubikasi_pemenuhan_m3,berat_pemenuhan_kg,
         tanggal_order,tanggal_po_expired) %>% 
  write.csv("data sales order.csv")
