# ==============================================================================
# kita mulai ya
rm(list=ls())
setwd("~/NTOP/Pilot Ikang Ciawi/Function")

# kita mulai perubahan yang radikal dari SDOA yang kemarin
# ikanx101.com
# ==============================================================================


# ide dasarnya adalah dengan membuat 3 decision variabel, yakni:
# 1. tanggal kirim
# 2. pake armada jenis apa
# 3. armada ke berapa

# nah pembagian itu harus berdasarkan toko, bukan sales order
# maka database sales order dan toko menjadi hal krusial untuk menentukan decision variables di atas

# ==============================================================================
# libraries
library(dplyr)
library(tidyr)
library(TSP)
library(tictoc)
library(parallel)

numcore = 5
# ==============================================================================

# ==============================================================================
# kita ambil function rotation matriks
source("0 function rotation matrix.R")
# ==============================================================================

# ==============================================================================
# load datasets
load("~/NTOP/Pilot Ikang Ciawi/Dokumentasi/modelling.rda")
# apakah mau Ciawi atau Cibitung terlebih dahulu?
target_gudang = "ciawi"
# ==============================================================================

# ==============================================================================
# kita akan modifikasi si database jenis armada
df_jenis_armada = 
  df_jenis_armada %>% 
  arrange(armada) %>% 
  mutate(id_armada = 1:nrow(df_jenis_armada)) %>% 
  select(-tersedia)
# ==============================================================================

# ==============================================================================
# pre-processing
# ada yang harus kita kerjakan terlebih dahulu, yakni menggabungkan df_order dan df_toko 
# agar jumlah barisnya kelak akan match
temporary = merge(df_order,df_toko)

# kita kembalikan lagi
df_toko  = temporary %>% select(colnames(df_toko))
df_order = temporary %>% select(colnames(df_order))
df_order$id = 1:nrow(df_order)

# kita filtering terlebih dahulu
df_toko   = df_toko %>% filter(supplied == target_gudang)
df_order  = df_order %>% filter(nama_toko %in% df_toko$nama_toko)
df_gudang = df_gudang %>% filter(site == target_gudang) %>% .$week_day_hour

# kita group split dulu per toko
df_order_per_toko = df_order %>% group_split(nama_toko)
# ==============================================================================

# ==============================================================================
# SDOA pertama adalah untuk membagi kelompok per tanggalnya
# ada berapa unique toko?
n_toko = length(df_order_per_toko)

# kita buat template dulu
hasil_df_order_per_toko = vector("list",n_toko)

# kita akan mulai hitung untuks setiap toko
for(ikanx in 1:n_toko){
  # ambil datanya
  temp        = df_order_per_toko[[ikanx]]
  # kalau cuma sebaris, gak usah pake SDOA
  marker_sdoa = nrow(temp)
  # baru kita hitung
  if(marker_sdoa > 1){
    print(paste0("mulai mencari tanggal pengiriman untuk toko ",ikanx))
    # kita panggil function generate tanggal dan perhitungan objective function
    source("0 SDOA untuk tanggal df order.R")
    
    # kita generate calon solusinya terlebih dahulu
    calon_solusi = mclapply(1:210,tanggal_generate,mc.cores = numcore)
    
    # menghitung initial objective function
    f_hit = mcmapply(obj_func,calon_solusi,mc.cores = numcore)
    
    # kita mulai perhitungannya di sini
    for(iter in 1:10){
      # kita cari dulu mana yang akan jadi pusat
      n_bhole = which.min(f_hit)
      
      # kita jadikan center of gravity
      center_1     = calon_solusi[[n_bhole]]
      
      calon_solusi_new = mcmapply(ro_kon,calon_solusi)
      calon_solusi     = lapply(seq_len(ncol(calon_solusi_new)), function(i) calon_solusi_new[,i])
      
      # kita hitung kembali function objective
      f_hit = mcmapply(obj_func,calon_solusi,mc.cores = numcore)
      # kita print terlebih dahulu
      cat(paste0(f_hit[which.min(f_hit)],"..."))
    }
    
    # kita cari dulu mana yang akan jadi pusat
    n_bhole = which.min(f_hit)
    
    # kita jadikan center of gravity
    center_1           = calon_solusi[[n_bhole]]
    temp$tanggal_kirim = center_1
  }
  
  if(marker_sdoa == 1){
    # tanggal kirimnya harus cepat-cepat
    temp$tanggal_kirim = temp$tanggal_kirim_min
  }
  # kita balikin lagi ke data awal
  hasil_df_order_per_toko[[ikanx]] <- temp
  print(paste0("Toko ",ikanx," DONE"))
}

# nah dari hasil yang ada, kita akan buat optimisasinya lagi dengan menggabung 
# yang "sedikit" ke yang "banyakan"
# kita buat template dulu
hasil_df_order_per_toko_tuning = vector("list",n_toko)

for(ikanx in 1:length(hasil_df_order_per_toko)){
  temp = hasil_df_order_per_toko[[ikanx]]
  hasil_df_order_per_toko_tuning[[ikanx]] = utak_atik_tanggal(temp)
  cat(paste0(ikanx))
}

# sudah selesai
