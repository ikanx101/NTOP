rm(list=ls())

setwd("~/NTOP/Pilot Desember/Dokumentasi")

library(dplyr)
library(expss)
library(openxlsx)

# ambil data yang sudah dihasilkan oleh optimizationerizer
load("dbase_toko.rda")
load("ciawi done ver")

# hanya ambil data customer yang berguna
df_cust_complete_ready = df_cust_complete_ready %>% 
                         select(nama_toko,provinsi,kota_kab,kecamatan)

# jangan lupa ganti tanggal
# sesuai dengan tanggal awal
tanggal_minimal = as.Date(as.character("2022-12-01"),
                          "%Y-%m-%d")

# kita bikin workbook-nya
wb = createWorkbook()

# sebagai contoh
ikanx = 1

# berapa banyak tanggal pengiriman
n_tanggal_kirim = length(jadwal_tanggal_armada)

for(ikanx in 1:n_tanggal_kirim){
  # ==============================================================================
  # kita mulai deduksinya dari sini
  temp = jadwal_tanggal_armada[[ikanx]] %>% 
    mutate(tanggal_kirim = tanggal_kirim + tanggal_minimal - 1)
  
  # sekarang kita mulai
  tanggal = paste0("TglKrm ",temp$tanggal_kirim[1])
  
  temp = 
    temp %>% 
    merge(df_cust_complete_ready) %>% 
    select(sales_order,nama_toko,provinsi,kota_kab,kecamatan,
           armada_terpilih,armada_ke,
           order_kubikasi,
           order_tonase) %>% 
    arrange(armada_terpilih,armada_ke) %>% 
    rename(jenis_armada = armada_terpilih) %>% 
    mutate(jenis_armada = case_when(
      jenis_armada == 1 ~ "Carry",
      jenis_armada == 2 ~ "CDE",
      jenis_armada == 3 ~ "CDD",
      jenis_armada == 4 ~ "CDD Jumbo",
      jenis_armada == 5 ~ "Fuso",
      jenis_armada == 6 ~ "Tronton",
      jenis_armada == 7 ~ "BU",
      jenis_armada == 8 ~ "Cont 20",
      jenis_armada == 9 ~ "Cont 40"
    )) %>%
    group_split(jenis_armada,armada_ke)
  
  # bikin sheet
  nama_sheet = paste0(tanggal)
  sh = addWorksheet(wb, nama_sheet)
  
  # isi tabelnya
  tabel_all = temp
  
  # masukin semua tabel ke sheet tersebut
  xl_write(tabel_all, wb, sh)
}

# export ke Excel
saveWorkbook(wb, "hasil optimasi jawa.xlsx", overwrite = TRUE)