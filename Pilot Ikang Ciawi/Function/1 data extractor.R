rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(parallel)

# set jumlah cores
numcore = 2

# folder data
path = "~/NTOP/Pilot Ikang Ciawi/Data Mentah"
file = list.files(path,pattern = "*csv",full.names = T)

# kita ambil semua file yang ada
dfs = mclapply(file,read.csv,mc.cores = numcore)

# kita tulis nama dataframe-nya
df_sales_order = dfs[[1]]
df_armada      = dfs[[2]]
df_cust_pos    = dfs[[3]]
df_gudang      = dfs[[4]]
df_kode_pos    = dfs[[5]]

# sekarang kita akan gabung data alamat customer dan data kode pos
df_cust_pos %>% head(1)
df_kode_pos %>% head(1)

# kita gabung
df_cust_complete = merge(df_cust_pos,df_kode_pos,all.x = T)

# ini sebagai informasi, ada beberapa list customer yang tidak memiliki kode pos
# dan atau memiliki kodepos tapi tidak ditemukan padanan provinsi, kota, dan kecamatannya
df_cust_complete %>% filter(is.na(lat)) %>% 
  openxlsx::write.xlsx(file = "list customer tak dimasukkan.xlsx")
