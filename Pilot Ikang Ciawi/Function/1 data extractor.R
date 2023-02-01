rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(parallel)

# ==============================================================================
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
  mutate(armada           = 1:nrow(df_armada),
         tersedia         = 15) %>% 
  rename(max_cap_kubikasi = kubikasi_max,
         max_cap_tonase   = tonase_max_ton,
         cost_per_km      = index_biaya_per_km_per_mobil,
         max_titik        = kapasitas_kirim_per_1_mobil_brp_drop_point,
         loading_time     = loading_runtime_menit) %>% 
  mutate(loading_time     = round((loading_time + 15) / 60,1)) %>% 
  select(armada,max_cap_kubikasi,max_cap_tonase,cost_per_km,tersedia,max_titik,loading_time)
# ==============================================================================


# ==============================================================================
# kita buat data gudang dulu
df_gudang =
  df_gudang %>% 
  mutate(week_day_hour   = loading_dock * weekday_hour,
         week_end_hour   = loading_dock * weekend_hour) %>% 
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
  group_by(nama_customer,sales_order) %>% 
  summarise(order_kubikasi    = sum(kubikasi_pemenuhan_m3),
            order_tonase      = sum(berat_pemenuhan_kg),
            tanggal_kirim_min = min(tanggal_order),
            tanggal_kirim_max = max(tanggal_po_expired)) %>% 
  ungroup() %>% 
  arrange(tanggal_kirim_min) %>% 
  filter(!is.na(order_kubikasi)) %>% 
  filter(!is.na(tanggal_kirim_min)) %>% 
  rename(nama_toko = nama_customer)

# kita akan ambil range tanggal tertentu saja
range_tanggal  = df_sales_order$tanggal_kirim_min %>% unique() %>% sort() %>% .[1:7]

# kita akan ambil range tanggal tersebut saja
df_sales_order_ready = 
  df_sales_order %>% 
  filter(tanggal_kirim_min %in% range_tanggal) %>% 
  mutate(tanggal_kirim_max = ifelse(is.na(tanggal_kirim_max),
                                    tanggal_kirim_min,
                                    tanggal_kirim_max)) %>% 
  mutate(tanggal_kirim_min = as.numeric(tanggal_kirim_min),
         tanggal_kirim_max = as.numeric(tanggal_kirim_max))

# kita ubah tanggal min dan tanggal max agar dalam rentang 1 - 20 sekian
min_number      = min(df_sales_order_ready$tanggal_kirim_min) - 1

# kita save dulu tanggal minimal agar nanti reportnya bisa dibalikin
tanggal_minimal = min(df_sales_order_ready$tanggal_kirim_min)
tanggal_minimal = as.Date(tanggal_minimal,origin = "1970-01-01")
  
# kita balikin lagi agar tanggalnya punya range 1 hingga sekian
df_sales_order_ready = 
  df_sales_order_ready %>% 
  mutate(tanggal_kirim_min = tanggal_kirim_min - min_number,
         tanggal_kirim_max = tanggal_kirim_max - min_number)
# ==============================================================================

# ==============================================================================
# berikutnya kita hanya akan ambil database longlat toko yang ada di data sales 
  # order ready saja
df_cust_complete_ready = 
  df_cust_complete %>% 
  filter(nama_toko %in% df_sales_order_ready$nama_toko)
# ==============================================================================



# ==============================================================================
# kita save datanya terlebih dahulu
save(df_sales_order,
     df_sales_order_ready,tanggal_minimal,
     df_armada,df_gudang,
     df_cust_complete,
     df_cust_uncomplete,
     df_cust_complete_ready,
     file = "~/NTOP/Pilot Ikang Ciawi/Dokumentasi/ready.rda")
# ==============================================================================