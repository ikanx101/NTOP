# ==============================================================================
# kita mulai ya
rm(list=ls())
setwd("~/NTOP/Pilot Desember/Function")

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

# mulai menghitung runtime
tic("Semua proses ini memakan waktu: ")
# banyak cores
numcore = 4
# ==============================================================================

# ==============================================================================
# kita ambil function rotation matriks
source("0 function rotation matrix.R")
# ==============================================================================

# ==============================================================================
# load datasets
load("~/NTOP/Pilot Desember/Dokumentasi/modelling.rda")
# apakah mau Ciawi atau Cibitung terlebih dahulu?
target_gudang = "ciawi"
# ==============================================================================

# ==============================================================================
# kita akan modifikasi si database jenis armada
df_jenis_armada = 
  df_jenis_armada %>% 
  arrange(armada) %>% 
  mutate(id_armada = 1:nrow(df_jenis_armada))
# ==============================================================================

# ==============================================================================
# pre-processing
# ada yang harus kita kerjakan terlebih dahulu, yakni menggabungkan df_order dan df_toko 
# agar jumlah barisnya kelak akan match
temporary = merge(df_order,df_toko)

# kita kembalikan lagi
df_toko  = temporary %>% select(colnames(df_toko)) %>% mutate(long = as.numeric(long),
                                                              lat  = as.numeric(lat))
df_order = temporary %>% select(colnames(df_order))

# kita filtering terlebih dahulu
df_toko   = df_toko %>% filter(supplied == target_gudang) %>% distinct()
df_order  = df_order %>% filter(nama_toko %in% df_toko$nama_toko) %>% distinct()
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
    calon_solusi = mclapply(1:10,tanggal_generate,mc.cores = numcore) # kalau mau akurat kita perbanyak calon solusi di sini
    
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
# ==============================================================================

# ==============================================================================
# nah dari hasil yang ada, kita akan buat optimisasinya lagi dengan menggabung 
# yang "sedikit" ke yang "banyakan"
# kita buat template dulu
hasil_df_order_per_toko_tuning = vector("list",n_toko)

for(ikanx in 1:length(hasil_df_order_per_toko)){
  temp = hasil_df_order_per_toko[[ikanx]]
  hasil_df_order_per_toko_tuning[[ikanx]] = utak_atik_tanggal(temp)
  print(paste0("fine tuning jadwal toko ",ikanx))
}
# ==============================================================================

# ==============================================================================
# sudah selesai
# kita kembalikan per tanggal kirim
final_jadwal = do.call(rbind,hasil_df_order_per_toko_tuning) %>% group_split(tanggal_kirim)
final_jadwal 
# ==============================================================================

# ==============================================================================
# kita akan cari armada yang pantas di-assign per tanggal
n_tanggal = length(final_jadwal)

# buat template terlebih dahulu
jadwal_tanggal_armada = vector("list",n_tanggal)

# di sini kita akan mulai enrich nama_toko dengan provinsi dan kota_kab
load("~/NTOP/Pilot Desember/Dokumentasi/dbase_toko.rda")
# kita hanya akan pilih yang tertentu saja
df_referensi = df_cust_complete_ready %>% select(nama_toko,provinsi,kota_kab)
# nanti si df_referensi ini akan dijadikan basis utk pengelompokan


# kita mulai pencarian per tanggalnya
for(ikanx in 1:n_tanggal){
  print(paste0("Mencari armada di tanggal ",ikanx))
  source("0 SDOA untuk armada df tanggal.R")
  # kita mulai
  temp = final_jadwal[[ikanx]] %>% merge(df_toko)
  
  # ============================================================================
  # ============================================================================
  # sekarang kita akan hitung
  n_toko_delivery         = length(unique(temp$nama_toko))
  
  # kita hitung dulu total order per toko
  order_total_per_toko = 
    temp %>% 
    group_by(provinsi,kota_kab) %>% 
    summarise(kubik      = sum(order_kubikasi),
              tonase     = sum(order_tonase),
              max_armada = mean(max_armada)) %>% 
    ungroup() %>% 
    mutate(armada_terpilih = 0)
  
  # buat summary per armada
  summary_armada = df_jenis_armada %>% select(armada,max_cap_kubikasi,max_cap_tonase)
  
  # kita akan hitung per baris nama_toko
  for(ix in 1:n_toko_delivery){
    # kita proses untuk toko tersebut
    print(paste0("Mencari armada untuk toko ",ix))
    # kita pilih armadanya
    armada_terpilih = 
      summary_armada %>% 
      filter(armada <= order_total_per_toko$max_armada[ix]) %>% 
      mutate(marker_tonase = max_cap_tonase >= order_total_per_toko$tonase[ix],
             marker_kubik  = max_cap_kubikasi >= order_total_per_toko$kubik[ix]) %>% 
      filter(marker_tonase == T & marker_kubik == T) %>% 
      head(1) %>% 
      .$armada %>% 
      as.numeric()
    # kita assign dulu
    order_total_per_toko$armada_terpilih[ix] = armada_terpilih
  }
  
  # kita akan cek apakah semuanya diassign armada yang berbeda-beda
  # marker pertama adalah jika setiap baris punya armada terpilih beda2
  marker_1 = length(unique(order_total_per_toko$armada_terpilih)) == nrow(order_total_per_toko)
  # marker kedua adalah saat semuanya diassign yang sama dan banyaknya baris <= 5
  marker_2 = nrow(order_total_per_toko) <= 5 & length(unique(order_total_per_toko$armada_terpilih)) == 1
  
  # kita simpulkan
  marker = marker_1 | marker_2
  # jika iya, maka semuanya dikirim menggunakan armada pertama dari armadanya masing-masing
  if(marker == TRUE){order_total_per_toko$armada_ke = 1}
  if(marker == FALSE){
    # kita akan cari berapa armada
    armada_ke_berapa = 
      order_total_per_toko %>% 
      group_by(armada_terpilih) %>% 
      tally() %>% 
      ungroup() %>% 
      mutate(berapa = ceiling(n/4)) %>% 
      select(armada_terpilih,berapa)
    
    # kita balikin ke data awalnya
    order_total_per_toko = merge(order_total_per_toko,armada_ke_berapa)
    
    # ==========================================================================
    # kita akan generate solusinya
    cari_solusi_armada = function(dummy){
      # berapa banyak solusinya
      n_vektor = nrow(order_total_per_toko)
      # kita akan generate solusinya
      solusi = rep(0,n_vektor)
      for(id in 1:n_vektor){
        solusi[id] = sample(order_total_per_toko$berapa[id],1)
      }
      return(solusi)
    }
    # ==========================================================================
    
    # ==============================================================================
    # fungsi untuk rotasi dan kontraksi
    ro_kon = function(list){
      Xt_2 = list
      # kita rotasikan dan konstraksikan
      X2 = mat_rotasi %*% (Xt_2 - center_1)
      X2 = center_1 + (.6 * X2)
      X2 = round(X2,0)
      return(X2)
    }
    # ==============================================================================
    
    
    # kita akan mulai SDOAnya di sini
    # generate solusi
    calon_solusi = vector("list",100)
    for(idy in 1:100){
      calon_solusi[[idy]] = cari_solusi_armada(idy)
    }
    
    # kita buat lagi rotation matriksnya
    mat_rotasi = buat_rot_mat(2*pi / 30,nrow(order_total_per_toko))
    
    # menghitung initial objective function
    f_hit  = mcmapply(obj_func_new,calon_solusi,mc.cores = numcore)
    
    # kita mulai perhitungannya di sini
    for(iter in 1:30){
      # kita cari dulu mana yang akan jadi pusat
      n_bhole = which.min(f_hit)[1]
      
      # kita jadikan center of gravity
      center_1         = calon_solusi[[n_bhole]]
      
      calon_solusi_new = mcmapply(ro_kon,calon_solusi,mc.cores = numcore)
      calon_solusi_new = lapply(1:100, function(i) calon_solusi_new[,i])
      
      calon_solusi = calon_solusi_new
      
      # kita hitung kembali function objective
      f_hit = mcmapply(obj_func_new,calon_solusi,mc.cores = numcore)
      # kita print terlebih dahulu
      cat(paste0(f_hit[which.min(f_hit)[1]],"..."))
    }
    # kita cari dulu mana yang akan jadi pusat
    n_bhole = which.min(f_hit)[1]
    # kita jadikan center of gravity
    center_1     = calon_solusi[[n_bhole]]
    
    # kita akan pilih nama toko dan armada terpilih
    order_total_per_toko = 
      order_total_per_toko %>% 
      mutate(armada_ke = center_1) %>% 
      select(nama_toko,armada_terpilih,armada_ke)
  }
  
  # kita keluarkan output yang diinginkan
  output               = merge(temp,order_total_per_toko) %>% select(-long,-lat)
  
  # kita akan assign per total
  jadwal_tanggal_armada[[ikanx]] = output
  # ============================================================================
  # ============================================================================
  
}

save(jadwal_tanggal_armada,file = "~/NTOP/Pilot Desember/Dokumentasi/ciawi done dengan tweak prov dan kota.rda")

toc()
