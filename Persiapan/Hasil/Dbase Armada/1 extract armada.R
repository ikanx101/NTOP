rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidyr)

# ambil file
nama_file = "Routing Explore.xlsx"
sheet     = excel_sheets(nama_file)

# kita ambil database mobil
df = read_excel(nama_file, sheet = sheet[2]) %>% janitor::clean_names()
df = df[1:7,]
df = df[1:7]

# kita akan ganti yang kosong dengan nomor lainnya
df$kubikasi_max[1] = 5

# kita save dulu
write.csv(df,"dbase armada.csv",row.names = F)
