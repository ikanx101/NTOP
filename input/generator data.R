setwd("/cloud/project/input")

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
tersedia         = sample(5:10,n_armada,replace = T) # tersedia berapa armada?
max_titik        = sample(2:10,n_armada,replace = T) %>% sort()
loading_time     = runif(n_armada) %>% sort()  # loading time dalam menit
df_jenis_armada  = data.frame(armada   = 1:n_armada,
                              max_cap_kubikasi = round(max_cap_kubikasi,1),
                              max_cap_tonase   = round(max_cap_tonase,1),
                              cost_per_km      = round(cost_per_km,1),
                              tersedia,max_titik,
                              loading_time     = round(loading_time,2))

df_jenis_armada

# dari informasi ini, saya akan buat matriks ketersediaan armada
# jadi nanti binary, misal: a1,10 = 1 jika armada 1 mobil 1 ready, a1,10 = 0  jika tidak ready

mat_armada_tersedia = matrix(0,n_armada,max(df_jenis_armada$tersedia))
for(i in 1:nrow(df_jenis_armada)){
  temp = df_jenis_armada %>% filter(armada == i) %>% .$tersedia
  for(j in 1:temp){
    mat_armada_tersedia[i,j] = 1
  }
}


# data terkait alamat tujuan
n_toko = 20
nama_toko  = randomNames::randomNames(n_toko,which.names = "first") %>% tolower()
nama_toko  = paste("toko",nama_toko)
long       = runif(n_toko)
lat        = runif(n_toko)
max_armada = sample(n_armada,n_toko,replace = T) # armada yang bisa masuk
supplied   = sample(c("ciawi","cibitung"),n_toko,replace = T,prob = c(.7,.3))
df_toko    = data.frame(nama_toko,long,lat,max_armada,supplied) 
df_toko

# nah dari data toko ini, ita akan buat tiga matriks
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

# lalu data toko mana disupply oleh gudang mana
mat_toko_supply = 
  df_toko %>% 
  select(nama_toko,supplied) %>% 
  reshape2::dcast(nama_toko ~ supplied) %>% 
  select(-nama_toko)
mat_toko_supply[is.na(mat_toko_supply)] = 0
mat_toko_supply[mat_toko_supply == "ciawi" | mat_toko_supply == "cibitung"] = 1
mat_toko_supply = mat_toko_supply %>% mutate_if(is.character,as.numeric)
mat_toko_supply = data.matrix(mat_toko_supply)






# data terkait pemesanan toko
nama_toko
order_kubikasi = sample(4:30,n_toko,replace = T)
order_tonase   = order_kubikasi * (1 + runif(1)/2)
n_tanggal      = 7   # range tanggal yang mungkin terjadi 

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
                      order_tonase = round(order_tonase,1),
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

# ada lagi data terkait service yang diberikan gudang
site            = c("ciawi","cibitung")
week_day_hour   = c(13.5,13.5)
week_end_hour   = c(10,10)

df_gudang = data.frame(site,week_day_hour,week_end_hour)



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
nama_sheet = paste0("data service gudang")
sh = addWorksheet(wb, nama_sheet)
tabel_all = list(df_gudang)
xl_write(tabel_all, wb, sh)


# bikin sheet
nama_sheet = paste0("summary utk modelling")
sh = addWorksheet(wb, nama_sheet)
tabel_all = list("Ini adalah bentuk dataframe dan matriks yang digunakan untuk membuat modelnya",
                 "Kita dapatkan dari proses ekstraksi dari data-data pada sheets sebelumnya",
                 "data jenis armada yang ada",df_jenis_armada,
                 "berapa armada yang tersedia untuk masing-masing armada?",mat_armada_tersedia,
                 "matriks jarak antar toko",dist_mat,
                 "matriks toko dan armada yang bisa melewatinya",mat_toko_armada,
                 "matriks toko dan gudang yang mensupply-nya",mat_toko_supply,
                 "data order yang dilakukan toko",df_order_murni,
                 "matriks time window pemenuhan order per toko",mat_order_tanggal,
                 "data service yang diberikan gudang",df_gudang
                 )
xl_write(tabel_all, wb, sh)

# export ke Excel
saveWorkbook(wb, "data yang dibutuhkan.xlsx", overwrite = TRUE)

# ==============================================================================
# sementara ini kita save utk keperluan dokumentasi
save(df_jenis_armada,df_toko,df_order,df_gudang,file = "data dokumentasi.rda")


# ==============================================================================
# sementara ini kita save dalam bentuk rda untuk kebutuhan modellingnya
save(df_jenis_armada,mat_armada_tersedia,dist_mat,mat_toko_armada,
     mat_toko_supply,df_order_murni,mat_order_tanggal,df_gudang,
     file = "modelling data.rda")

# DONE

