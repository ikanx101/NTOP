setwd("~/NTOP/Persiapan/Raw Data")

# ambil libraries yang dibutuhkan
library(dplyr)
library(readxl)

# ambil data
file_name = "Routing Explore.xlsx"
sheets    = excel_sheets(file_name)
sh_sel    = sheets[1]

# import data
df        = read_excel(file_name,sheet = sh_sel) %>% janitor::clean_names()
df_cust   = df %>% select(customer,tipe,kode_pos,kapasitas_mobil_max)

# ambil kode pos target
kode_pos_target = df_cust$kode_pos %>% unique() %>% sort()

# load function scraper
source("~/NTOP/Persiapan/Kode Pos Scraper/scraper.R")

hasil = vector("list",length(kode_pos_target))
for(i in 1:length(kode_pos_target)){
  hasil[[i]] = scraper_kodepos(kode_pos_target[i])
  print(i)
}

final = do.call(rbind,hasil)

final
