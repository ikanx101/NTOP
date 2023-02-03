rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidyr)

# ambil file
nama_file = "Routing Explore.xlsx"
sheet     = excel_sheets(nama_file)

# kita ambil database mobil
df = read_excel(nama_file, sheet = sheet[3]) %>% janitor::clean_names()
df = df[1:10,] %>% filter(!is.na(jenis_mobil))
df = df[1:7]

# kita akan ganti yang kosong dengan nomor lainnya
df$kubikasi_max[1] = 5

# ternyata ada penambahan data container
df$tonase_max_ton[8] = 30
df$tonase_max_ton[9] = 40

df$index_biaya_per_km_per_mobil[8] = 50
df$index_biaya_per_km_per_mobil[9] = 70

df$kapasitas_kirim_per_1_mobil_brp_drop_point[8] = 2
df$kapasitas_kirim_per_1_mobil_brp_drop_point[9] = 2


# kita save dulu
write.csv(df,"dbase armada.csv",row.names = F)
