setwd("~/NTOP/input")

rm(list=ls())

library(dplyr)
library(tidyr)
library(expss)
library(openxlsx)

# kita akan buat data yang diperlukan

# data terkait armada
n_armada         = 5
max_cap_kubikasi = sample(10:90,n_armada,replace = F) %>% sort()  # berapa kubikasi yang bisa diangkut
max_cap_tonase   = max_cap_kubikasi * (1 + runif(n_armada))       # berapa kg yang bisa diangkut
cost_per_km      = (runif(n_armada) * 50) %>% sort()              # biaya antar per km
max_route        = # max berapa tempat yang bisa dikunjungi
df_jenis_armada  = data.frame(armada   = 1:n_armada,max_cap_kubikasi,max_cap_tonase,cost_per_km)

df_jenis_armada

# data terkait alamat tujuan
n_toko = 10
nama_toko  = randomNames::randomNames(n_toko,which.names = "first") %>% tolower()
nama_toko  = paste("toko",nama_toko)
long       = runif(n_toko)
lat        = runif(n_toko)
max_armada = sample(n_armada,n_toko,replace = T) # armada yang bisa masuk
df_toko    = data.frame(nama_toko,long,lat,max_armada) 
df_toko

# nah dari data toko ini, ita akan buat dua matriks
# pertama matriks jarak
# kita akan buat matriks jaraknya

# buat rumahnya terlebih dahulu
dist_mat = matrix(0,n_toko,n_toko)
# kita buat euclidean distance terlebih dahulu
hitung_jarak = function(i,j){
  lon_hit = df_toko$long[i] - df_toko$long[j]
  lat_hit = df_toko$lat[i] - df_toko$lat[j]
  sqrt(lon_hit^2 + lat_hit^2)
}
# kita hitung jaraknya sekarang
for(i in 1:n_toko){
  for(j in 1:n_toko){
    dist_mat[i,j] = hitung_jarak(i,j)
  }
}
# hasil finalnya
dist_mat

# kedua matriks armada max
# matriks ini akan menghubungkan antara toko dan armada max yang bisa melintasi
mat_toko_armada = matrix(0,n_toko,n_armada)
# kita buat loopin terlebih dahulu
for(i in 1:n_toko){
  temp = df_toko$max_armada[i]
  for(j in 1:temp){
    mat_toko_armada[i,j] = 1
  }
}
# hasil finalnya
mat_toko_armada

# data terkait pemesanan toko
nama_toko
order_kubikasi = sample(4:30,n_toko,replace = T)
order_tonase   = order_kubikasi * (1 + runif(1)/2)
n_tanggal      = 15   # range tanggal yang mungkin terjadi 

# kita akan buat function untuk generate tanggal min max yang akan muncul
tanggal_min_max = function(n_tanggal,n_toko){
  tanggal_1      = sample(n_tanggal,n_toko,replace = T)
  tanggal_2      = sample(n_tanggal,n_toko,replace = T)
  ifelse(tanggal_1 <= tanggal_2,
         paste(tanggal_1,tanggal_2,sep = ";"),
         paste(tanggal_2,tanggal_1,sep = ";"))
}

tanggal_kirim    = tanggal_min_max(n_tanggal,n_toko)

df_order = data.frame(nama_toko,
                      order_kubikasi,
                      order_tonase,
                      tanggal_kirim) %>% 
  separate(tanggal_kirim,
           into = c("tanggal_kirim_min","tanggal_kirim_max"),
           sep = "\\;")

df_order

# kita akan pisah menjadi dua data
# yakni data order murni
df_order_murni = df_order %>% select(-contains("tanggal"))

# dan data tanggal order dalam bentuk matriks
mat_order_tanggal = matrix(0,n_toko,n_tanggal)
for(i in 1:n_toko){
  temp_1 = df_order$tanggal_kirim_min[i] %>% as.numeric()
  temp_2 = df_order$tanggal_kirim_max[i] %>% as.numeric()
  for(j in temp_1:temp_2){
    mat_order_tanggal[i,j] = 1
  }
}
# hasil finalnya
mat_order_tanggal

# ==============================================================================
# kita save dulu dalam bentuk excel untuk kebutuhan tim digital
wb = createWorkbook()

# bikin sheet
nama_sheet = paste0("jenis armada")
sh = addWorksheet(wb, nama_sheet)
tabel_all = list(df_jenis_armada)
xl_write(tabel_all, wb, sh)

# bikin sheet
nama_sheet = paste0("data alamat toko")
sh = addWorksheet(wb, nama_sheet)
tabel_all = list(df_toko)
xl_write(tabel_all, wb, sh)

# bikin sheet
nama_sheet = paste0("data order toko")
sh = addWorksheet(wb, nama_sheet)
tabel_all = list(df_order)
xl_write(tabel_all, wb, sh)

# bikin sheet
nama_sheet = paste0("summary utk modelling")
sh = addWorksheet(wb, nama_sheet)
tabel_all = list("Ini adalah bentuk dataframe dan matriks yang digunakan untuk membuat modelnya",
                 "Kita dapatkan dari proses ekstraksi dari data-data pada sheets sebelumnya",
                 df_jenis_armada,dist_mat,mat_toko_armada,df_order_murni,mat_order_tanggal)
xl_write(tabel_all, wb, sh)

# export ke Excel
saveWorkbook(wb, "data yang dibutuhkan.xlsx", overwrite = TRUE)


# ==============================================================================
# sementara ini kita save dalam bentuk rda untuk kebutuhan modellingnya
save(df_jenis_armada,dist_mat,mat_toko_armada,df_order_murni,mat_order_tanggal,
     file = "required data.rda")
