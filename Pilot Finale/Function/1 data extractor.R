rm(list=ls())

setwd("~/NTOP/Pilot Finale/Function")

# libraries
library(dplyr)
library(tidyr)
library(parallel)
library(readxl)

# perlu diperhatikan bahwa yang mau diambil adalah toko di pulau jawa aja
load("filterus.rda")

# penentuan dia itu masuk ke distributor atau outlet
# maunya apa?
# tipe_jadwal    = "Distributor"
# atau
tipe_jadwal    = "Outlet"

path_raw       = "~/NTOP/Persiapan/Raw Data/Routing Explore.xlsx"
df_dist_outlet = read_excel(path_raw,sheet = "Data Train",skip = 1) %>% 
                 janitor::clean_names() %>% 
                 select(nama_customer,tipe) %>% 
                 distinct() %>% 
                 filter(tipe == tipe_jadwal) %>% 
                 .$nama_customer


# ==============================================================================
# set jumlah cores
numcore = 5

# folder data
path = "~/NTOP/Pilot Finale/Data Mentah"
file = list.files(path,pattern = "*csv",full.names = T)

# kita ambil semua file yang ada
dfs = mclapply(file,read.csv,mc.cores = numcore)

# kita tulis nama dataframe-nya
df_sales_order = dfs[[1]] %>% filter(nama_customer %in% toko_javanicus) %>% 
                              filter(nama_customer %in% df_dist_outlet)
df_armada      = dfs[[2]]
df_cust_pos    = dfs[[3]] %>% filter(customer %in% toko_javanicus) %>% 
                              filter(customer %in% df_dist_outlet)
df_gudang      = dfs[[4]]
df_kode_pos    = dfs[[5]]
# ==============================================================================


# ==============================================================================
# sekarang kita akan gabung data alamat customer dan data kode pos
df_cust_pos %>% head(1)
df_kode_pos %>% head(1)

# kita gabung
df_cust_complete = merge(df_cust_pos,df_kode_pos,all.x = T)

# ini sebagai informasi, ada beberapa list customer yang tidak memiliki kode pos
# dan atau memiliki kodepos tapi tidak ditemukan padanan provinsi, kota, dan kecamatannya
df_cust_uncomplete = 
  df_cust_complete %>% 
  filter(is.na(lat)) 

# sedangkan ini adalah database untuk customer yang complete ada longlatnya
df_cust_complete = df_cust_complete %>% filter(!is.na(lat))
# ==============================================================================


# ==============================================================================
# kita mulai dari armada dulu
df_armada = 
  df_armada %>% 
  mutate(armada           = 1:nrow(df_armada)) %>% 
  rename(max_cap_kubikasi = kubikasi_max,
         max_cap_tonase   = tonase_max_ton,
         cost_per_km      = index_biaya_per_km_per_mobil,
         max_titik        = kapasitas_kirim_per_1_mobil_brp_drop_point,
         loading_time     = loading_runtime_menit) %>% 
  mutate(loading_time     = round((loading_time + 10) / 60,1),
         max_cap_tonase   = max_cap_tonase * 1000) %>% 
  select(armada,max_cap_kubikasi,max_cap_tonase,cost_per_km,max_titik,loading_time)
# ==============================================================================


# ==============================================================================
# kita buat data gudang dulu
df_gudang =
  df_gudang %>% 
  mutate(week_day_hour   = loading_dock * weekday_hour,
         week_end_hour   = loading_dock * weekend_hour,
         site            = tolower(site)) %>% 
  select(site,week_day_hour,week_end_hour)
# ==============================================================================


# ==============================================================================
# data detail toko
df_cust_complete = 
  df_cust_complete %>%
  select(customer,lng,lat) %>% 
  rename(nama_toko  = customer,
         long       = lng) %>% 
  mutate(max_armada = 5,
         supplied   = "ciawi")
# ==============================================================================


# ==============================================================================
# sekarang kita akan rapikan data sales order
df_sales_order =
  df_sales_order %>% 
  mutate(tanggal_order      = as.Date(tanggal_order,"%Y-%m-%d"),
         tanggal_po_expired = as.Date(tanggal_po_expired,"%Y-%m-%d")) %>%
  mutate(tanggal_order      = ifelse(tanggal_order <= as.Date("2022-12-01","%Y-%m-%d"),
                                     as.Date("2022-12-01","%Y-%m-%d"),
                                     tanggal_order),
         tanggal_po_expired = ifelse(tanggal_po_expired <= as.Date("2022-12-01","%Y-%m-%d"),
                                     as.Date("2022-12-04","%Y-%m-%d"),
                                     tanggal_po_expired)) %>%
  mutate(tanggal_po_expired = ifelse(is.na(tanggal_po_expired),
                                     tanggal_order,
                                     tanggal_po_expired),
         tanggal_order      = as.Date(tanggal_order,origin = "1970-01-01"),
         tanggal_po_expired = as.Date(tanggal_po_expired,origin = "1970-01-01")) %>%
  group_by(nama_customer,sales_order) %>% 
  summarise(order_kubikasi    = sum(kubikasi_pemenuhan_m3),
            order_tonase      = sum(berat_pemenuhan_kg),
            tanggal_kirim_min = min(tanggal_order),
            tanggal_kirim_max = max(tanggal_po_expired)) %>%
  ungroup() %>% 
  mutate(order_tonase = ifelse(is.na(order_tonase),0,order_tonase)) %>% 
  arrange(tanggal_kirim_min) %>% 
  filter(!is.na(order_kubikasi)) %>% 
  filter(!is.na(tanggal_kirim_min)) %>% 
  rename(nama_toko = nama_customer)

# kita akan ambil range tanggal tersebut saja
df_sales_order_ready = 
  df_sales_order %>% 
  mutate(tanggal_kirim_max = ifelse(is.na(tanggal_kirim_max),
                                    tanggal_kirim_min,
                                    tanggal_kirim_max)) %>% 
  mutate(tanggal_kirim_min = as.numeric(tanggal_kirim_min),
         tanggal_kirim_max = as.numeric(tanggal_kirim_max))

# ini buat ngecek aja sih
df_sales_order_ready$nama_toko %>% unique() %>% sort()

# kita ubah tanggal min dan tanggal max agar dalam rentang 1 - 20 sekian
min_number      = min(df_sales_order_ready$tanggal_kirim_min) - 1

# kita save dulu tanggal minimal agar nanti reportnya bisa dibalikin
tanggal_minimal = min(df_sales_order_ready$tanggal_kirim_min)
tanggal_minimal = as.Date(tanggal_minimal,origin = "1970-01-01")
  
# kita balikin lagi agar tanggalnya punya range 1 hingga sekian
df_sales_order_ready = 
  df_sales_order_ready %>% 
  mutate(tanggal_kirim_min = tanggal_kirim_min - min_number,
         tanggal_kirim_max = tanggal_kirim_max - min_number,
         # ini kita buat jaring pengaman
         # seandainya ada tanggal max yang kosong
         # maka diisi sama dengan tanggal kirim min
         tanggal_kirim_max = ifelse(tanggal_kirim_max < tanggal_kirim_min,
                                    tanggal_kirim_min,
                                    tanggal_kirim_max))
# ==============================================================================


# ==============================================================================
# berikutnya kita hanya akan ambil database longlat toko yang ada di data sales 
  # order ready saja
df_cust_complete_ready = 
  df_cust_complete %>% 
  filter(nama_toko %in% df_sales_order_ready$nama_toko)
# ==============================================================================



# ==============================================================================
# sedangkan yang ini untuk keperluan modelling
df_jenis_armada = df_armada
df_toko         = df_cust_complete_ready
df_order        = df_sales_order_ready
df_gudang       = df_gudang

# nah karena data max armada nyusul, jadi mau gak mau ditempel di mari
# kita ambil dari sheet armada aja ya
sht         = "Max Armada"
filename    = "~/NTOP/Persiapan/Raw Data/Routing Explore.xlsx"
df_tambahan = read_excel(filename,sheet = sht) %>% 
              janitor::clean_names() %>% 
              filter(!is.na(cust)) %>% 
              rename(jenis_mobil = kapasitas_max_armada) %>% 
              select(cust,jenis_mobil)

# kita hanya ambil kodenya di sini
df_arm_awal = dfs[[2]] %>% select(jenis_mobil) %>% mutate(armada = 1:9)

# kita merge lagi ke sini
df_toko_armada = merge(df_tambahan,df_arm_awal) %>% select(cust,armada) %>% 
                 rename(nama_toko  = cust,
                        max_armada = armada) %>% distinct()

# sekarang kita lihat armada yang bisa liwat di df toko
df_toko$max_armada = NULL
df_toko            = merge(df_toko,df_toko_armada,all.x = T) %>% 
                     mutate(max_armada = ifelse(is.na(max_armada),7,max_armada))

save(df_jenis_armada,df_toko,df_order,df_gudang,
     file = "~/NTOP/Pilot Finale/Dokumentasi/modelling.rda")
# ==============================================================================

# ==============================================================================
# kita save datanya
# untuk keperluan dokumentasi

# kita harus save dbase_toko di sini
df_cust_complete_ready = df_toko

save(df_sales_order,
     df_sales_order_ready,tanggal_minimal,
     df_armada,df_gudang,
     df_cust_complete,
     df_cust_uncomplete,
     df_cust_complete_ready,
     file = "~/NTOP/Pilot Finale/Dokumentasi/dokumentasi.rda")

df_cust_pos            = df_cust_pos %>% rename(nama_toko = customer)
df_kode_pos            = df_kode_pos %>% select(kode_pos,provinsi,kota_kab,kecamatan)
df_cust_complete_ready = merge(df_cust_complete_ready,df_cust_pos) %>% merge(df_kode_pos)

save(df_cust_complete_ready,
     file = "~/NTOP/Pilot Finale/Dokumentasi/dbase_toko.rda")

print("DONE")


# beyonder
# kita akan filter hanya di jawa sahaja
# oleh karena itu ada skrip beyonder seperti ini

toko_javanicus = 
  df_cust_complete_ready %>% 
  filter(grepl("jawa|banten|karta|bogor",provinsi,ignore.case = T)) %>% 
  .$nama_toko

save(toko_javanicus,file = "filterus.rda")







