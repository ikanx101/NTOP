setwd("~/Nutrifood Transporter Optimization/Persiapan/Raw Data")

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
source("~/Nutrifood Transporter Optimization/Persiapan/Kode Pos Scraper/scraper.R")

hasil = vector("list",length(kode_pos_target))
for(i in 1:length(kode_pos_target)){
  hasil[[i]] = scraper_kodepos(kode_pos_target[i])
  print(i)
}

# gabungan semua
final = do.call(rbind,hasil)

# kita save dulu data yang sudah ada
final %>% 
  filter(!is.na(provinsi)) %>% 
  write.csv("kode_pos_done_tahap_1.csv",row.names = F)

# kita ambil yang belum ada sama sekali
kode_pos_belum = final %>% filter(is.na(provinsi))

write.csv(kode_pos_belum,"kode_pos_belum.csv",row.names = F)
