# ==============================================================================
# kita mulai ya
rm(list=ls())
setwd("~/NTOP/Pilot Ikang Ciawi/Function")
# ==============================================================================

# ide dasarnya adalah dengan membuat 3 decision variabel, yakni:
  # 1. tanggal kirim
  # 2. pake armada jenis apa
  # 3. armada ke berapa

# kita mulai ya

# ==============================================================================
# libraries
library(dplyr)
library(tidyr)
library(TSP)
library(tictoc)
# ==============================================================================

# ==============================================================================
# membuat function rotation matrix
buat_rot_mat = function(theta,n){
  # buat template sebuah matriks identitas
  temp_mat = matrix(0,ncol = n,nrow = n)
  diag(temp_mat) = 1
  
  # buat matriks identitas terlebih dahulu
  mat_rot = temp_mat
  # membuat isi matriks rotasi
  for(i in 1:(n-1)){
    for(j in 1:i){
      temp = temp_mat
      idx = n-i
      idy = n+1-j
      # print(paste0("Matriks rotasi untuk ",idx," - ",idy,": DONE"))
      temp[idx,idx] = cos(theta)
      temp[idx,idy] = -sin(theta)
      temp[idy,idx] = sin(theta)
      temp[idy,idy] = cos(theta)
      # assign(paste0("M",idx,idy),temp)
      mat_rot = mat_rot %*% temp
      mat_rot = mat_rot
    }
  }
  # output matriks rotasi
  return(mat_rot)
}
# ==============================================================================
# notes:
# utk ini biarkan saja serial tidak perlu parallel

# ==============================================================================
# load datasets
load("~/NTOP/Pilot Ikang Ciawi/Dokumentasi/modelling.rda")
# ==============================================================================


# ==============================================================================
# apakah mau Ciawi atau Cibitung terlebih dahulu?
target_gudang = "ciawi"
# ==============================================================================


# ==============================================================================
# pre-processing
# ada yang harus kita kerjakan terlebih dahulu, yakni menggabungkan df_order dan df_toko 
  # agar jumlah barisnya kelak akan match
temporary = merge(df_order,df_toko)

# kita kembalikan lagi
df_toko  = temporary %>% select(colnames(df_toko))
df_order = temporary %>% select(colnames(df_order))
# ==============================================================================


# ==============================================================================
# kita akan modifikasi si database jenis armada
df_jenis_armada = 
  df_jenis_armada %>% 
  arrange(armada) %>% 
  mutate(id_armada = 1:nrow(df_jenis_armada)) %>% 
  select(-tersedia)

# lalu kita akan ambil data mana yang harus dikerjakan terlebih dahulu

# kita filtering terlebih dahulu
df_toko   = df_toko %>% filter(supplied == target_gudang)
df_order  = df_order %>% filter(nama_toko %in% df_toko$nama_toko)
df_gudang = df_gudang %>% filter(site == target_gudang) %>% .$week_day_hour
# ==============================================================================


# ==============================================================================
# kita akan buat df_order_per_toko untuk mengetahui sebenarnya ada berapa toko yang harus di-supply
df_order_per_toko = 
  df_order %>% 
  group_by(nama_toko) %>% 
  summarise(kubik_total  = sum(order_kubikasi),
            tonase_total = sum(order_tonase)) %>% 
  ungroup()

# kegunaan fungsi ini adalah untuk memilih armada mana yang sebaiknya digunakan
max_tonase     = df_order_per_toko$tonase_total %>% max()
armada_terbesar = 
  df_jenis_armada %>% 
  filter(max_cap_tonase > max_tonase) %>% 
  .$armada %>% 
  min()

# maka didapatkan, armada terbesar yang harusnya beraksi adalah armada_terbesar


#  #    ##        a      n     n  x       x
#  #   ##        a a     n n   n   x     x
#  #  ##        a   a    n  n  n    x   x
#  # ##        a     a   n   n n     x x
#  ##         aaaaaaaaa  n    nn      x
#  # ##       a       a  n     n     x x
#  #  ##      a       a  n     n    x   x
#  #   ##     a       a  n     n   x     x
#  #    ##    a       a  n     n  x       x
#  #     ##   a       a  n     n  x       x



# ==============================================================================
# berikutnya kita akan buat function generator
# jangan lupa bahwa nanti ada yang harus difilter terlebih dahulu

# generate solusi untuk armada
armada_generate = function(n_toko){
  vec = rep(1,n_toko) 
  for(i in 1:n_toko){
    # kita ambil dulu
    # di toko tersebut armada terbesar yang boleh liwat itu armada yang mana
    jenis_armada_sel = df_toko$max_armada[i]
    # karena semua armada hanya dibatasi punya 5 titik aja, makanya gak ada guna pilih 
    # armada yang besar-besar
    jenis_armada_sel = ifelse(jenis_armada_sel >= armada_terbesar, 
                              armada_terbesar, 
                              jenis_armada_sel)
    vec[i]           = sample(jenis_armada_sel,1)
  }
  return(vec)
}

# generate tanggal kirim sesuai dengan data yang ada pada df_order
tanggal_generate = function(var,df){
  hasil = rep(0,n_toko)
  min   = df[["tanggal_kirim_min"]] %>% as.numeric()
  max   = df[["tanggal_kirim_max"]] %>% as.numeric()
  for(i in 1:n_toko){
    if(min[i] == max[i]){
      hasil[i] = min[i]
    }
    if(min[i] != max[i]){
      # kita paksakan selesai dalam waktu yang sesingkat-singkatnya
      hasil[i] = sample(c(min[i]:(min[i]+3)),1) 
    }
  }
  return(hasil)
}
# ==============================================================================


# ==============================================================================
# function untuk membuat matriks jarak
buat_matriks_jarak = function(df){
  n_toko = nrow(df)
  # kita tambahin untuk long lat CIAWI
  df[n_toko+1,] = list(NA)
  n_toko = nrow(df)
  df$long[n_toko] = -6.649061
  df$lat[n_toko]  = 106.8408808
  # buat rumahnya terlebih dahulu
  dist_mat = matrix(0,n_toko,n_toko)
  # kita buat euclidean distance terlebih dahulu
  hitung_jarak = function(i,j){
    lon_hit = df$long[i] - df$long[j]
    lat_hit = df$lat[i] - df$lat[j]
    jarak = sqrt(lon_hit^2 + lat_hit^2)
    round(jarak,3)
  }
  # kita hitung jaraknya sekarang
  for(i in 1:n_toko){
    for(j in 1:n_toko){
      dist_mat[i,j] = hitung_jarak(i,j)
    }
  }
  # hasil finalnya
  return(dist_mat)
}
# ==============================================================================


# ==============================================================================
# perhitungan rute optimal
# inputnya adalah matriks jarak
tsp_hitung = function(new){
  # jangan lupa new adalah df_toko yang sudah di-slice
  jarse = buat_matriks_jarak(new)
  problem = as.ATSP(jarse)
  hasil = solve_TSP(problem)
  level = row.names(new)
  panjang_rute = tour_length(hasil)
  detail_rute  = paste(level[as.integer(hasil)],collapse = " - ")
  return(panjang_rute)
}
# ==============================================================================


# ==============================================================================
# kita akan buat function untuk objective function
obj_func = function(list_1,list_2,list_3){
  # kita buat dulu ke data frame untuk mengecek semua informasi yang ada
  df_temp_1 = df_toko %>% select(nama_toko,long,lat,max_armada)
  df_temp_2 = df_order %>% select(nama_toko,sales_order,order_kubikasi,order_tonase,
                                  tanggal_kirim_min,tanggal_kirim_max)
  df_temp_3 = merge(df_temp_1,df_temp_2) %>% distinct()
  df_temp_3$id_armada        = round(as.vector(list_1),0) # kita rounding dulu ya
  df_temp_3$tanggal_kirim    = round(as.vector(list_2),0) # kita rounding dulu ya
  df_temp_3$armada_ke        = round(as.vector(list_3),0)
  df_temp_3                  = merge(df_temp_3,df_jenis_armada)
  
  # konstanta penalti
  beta  = 10^6
  alpa  = 10^4
  gamma = 10^3
  
  # kita pecah dulu berdasarkan armada dan tanggal
  pecah      = df_temp_3 %>% group_split(id_armada,tanggal_kirim,armada_ke)
  n_pecah    = length(pecah)
  
  # kita hitung dulu cost per jarak
  jarak_cost = rep(0,n_pecah)
  for(i in 1:n_pecah){
    temp           = pecah[[i]]
    jarak_hit      = tsp_hitung(temp)
    jarak_cost[i]  = jarak_hit * temp$cost_per_km[1]
  }
  jarak_total  = sum(jarak_cost)   # ini yang pertama disave
  
  # constraint 1
  # tidak ada armada yang kelebihan muatan dalam kubikasi
  constraint_1 = rep(0,n_pecah)
  # constraint 2
  # tidak ada armada yang kelebihan muatan dalam tonase
  constraint_2 = rep(0,n_pecah)
  # constraint 3
  # armada yang mengantar tidak boleh melebihi max armada (armada terbesar) yang memungkinkan
  constraint_3 = rep(0,n_pecah)
  # constraint 4
  # rute yang dilalui tidak melebihi max rute
  constraint_4 = rep(0,n_pecah)
  # constraint 5
  # tidak ada yang telat kirim
  constraint_5 = rep(0,n_pecah)
  
  
  # proses menghitung semua constraint
  for(i in 1:n_pecah){
    temp_1 = pecah[[i]]
    # constraint 1
    c_1              = sum(temp_1$order_kubikasi) - mean(temp_1$max_cap_kubikasi)
    c_1              = max(c_1,0)
    constraint_1[i]  = beta * c_1^2
    
    # constraint 2
    c_2              = sum(temp_1$order_tonase) - mean(temp_1$max_cap_tonase)
    c_2              = max(c_2,0)
    constraint_2[i]  = beta * c_2^2
    
    # constraint 3
    c_3              = mean(temp_1$armada) - mean(temp_1$max_armada)
    c_3              = max(c_3,0)
    constraint_3[i]  = beta * c_3^2
    
    # constraint 4
    c_4              = length(unique(temp_1$nama_toko)) - 5    # ini kita paksa agar terisi
    c_4              = max(c_4,0)                              # 5 titik
    constraint_4[i]  = alpa * c_4^2
    
    # constraint 5
    c_5              = temp_1 %>% 
                       mutate(skor = ifelse(tanggal_kirim <= tanggal_kirim_max &
                              tanggal_kirim >= tanggal_kirim_min,
                              0,
                              1)) %>% 
                       summarise(skor = sum(skor))
    c_5              = as.numeric(c_5)
    constraint_5[i]  = gamma * as.numeric(c_5)
    
  }
  
  # ada beberapa constraint yang hanya bisa dilihat per tanggal kirim
  pecah      = df_temp_3 %>% group_split(tanggal_kirim)
  n_pecah    = length(pecah)
  
  # constraint 6
  # total waktu loading
  constraint_6 = rep(0,n_pecah)
  
  # proses menghitung semua constraint
  for(i in 1:n_pecah){
    temp_1 = pecah[[i]]
    # constraint 6
    c_1              = sum(temp_1$loading_time) - df_gudang
    c_1              = max(c_1,0)
    constraint_6[i]  = beta * c_1^2
  }
  
  output = jarak_total + sum(constraint_1) + sum(constraint_2) + 
    sum(constraint_3) + sum(constraint_4) + sum(constraint_5) +
    sum(constraint_6)
  
  return(output)
  }
# ==============================================================================


# ==============================================================================
# jenis mobil
# fungsi untuk rotasi dan kontraksi
ro_kon_1 = function(list,center){
  Xt_1 = list
  # kita rotasikan dan konstraksikan
  X1 = mat_rotasi %*% (Xt_1 - center_1)
  X1 = center_1 + (.4 * X1)
  X1 = ifelse(X1 <= 1,1,X1)  # kita main putar-putar dulu agar basisnya tetap sama
  X1 = ifelse(X1 >= armada_terbesar,armada_terbesar,X1)
  return(X1)
}

# tanggal kirim
# fungsi untuk rotasi dan kontraksi
ro_kon_2 = function(list,center){
  Xt_2 = list
  # kita rotasikan dan konstraksikan
  X2 = mat_rotasi %*% (Xt_2 - center_2)
  X2 = center_2 + (.4 * X2)
  X2 = ifelse(X2 <= 1,1,X2)
  #X2 = ifelse(X2 >= max_tanggal_kirim,max_tanggal_kirim,X2) # seandainya lebih dari max hari pengiriman
  return(X2)
}

# banyak mobil
# fungsi untuk rotasi dan kontraksi
ro_kon_3 = function(list,center){
  Xt_2 = list
  # kita rotasikan dan konstraksikan
  X2 = mat_rotasi %*% (Xt_2 - center_3)
  X2 = center_3 + (.4 * X2)
  X2 = ifelse(X2 <= 1,5,X2)
  X2 = ifelse(X2 >= 5,1,X2) # seandainya lebih dari 5 mobil maka harus diturunkan
  return(X2)
}
# ==============================================================================



# ==============================================================================
# sekarang kita akan mulai bagian yang seru
n_toko   = nrow(df_toko)
n_solusi = 100
n_sdoa   = 15


# karena bakal banyak generatenya, kita akan gunakan prinsip parallel saja
# paralel
library(parallel)
numCores = 10

# list pertama yakni armada
# bikin dummy
df_dummy = data.frame(id = 1:n_solusi,
                      n_toko)

hasil = mcmapply(armada_generate,df_dummy$n_toko,
                 mc.cores = numCores) 
# pecah ke list
solusi_1 = lapply(seq_len(ncol(hasil)), function(i) hasil[,i])

# kita bikin solusi_3 yakni mobil ke berapa
gener_solusi_3 = function(dummy){sample(5,n_toko,replace = T)} # 6 banyak mobil
hasil = mcmapply(gener_solusi_3,
                 df_dummy$n_toko,
                 mc.cores = numCores)

# pecah ke list
solusi_3 = lapply(seq_len(ncol(hasil)), function(i) hasil[,i])

# list pertama yakni tanggal
solusi_2 = vector("list",n_solusi)

# kita generate calon solusi terlebih dahulu
for(i in 1:n_solusi){
  solusi_2[[i]] = tanggal_generate(n_toko,df_order)
  print(i)
}

tic("buat matrot")
# buat matriks rotasi
mat_rotasi = buat_rot_mat(2*pi / 20,n_toko)
toc()

tic("SDOA")
# initial condition
f_hit = c()

# kita hitung dulu initial function objective
f_hit = mcmapply(obj_func,solusi_1,solusi_2,solusi_3,mc.cores = numCores)

# kita mulai perhitungannya di sini
for(iter in 1:n_sdoa){
  # kita cari dulu mana yang akan jadi pusat
  n_bhole = which.min(f_hit)
  
  # kita jadikan center of gravity
  center_1 = solusi_1[[n_bhole]]
  center_2 = solusi_2[[n_bhole]]
  center_3 = solusi_3[[n_bhole]]
  
  solusi_1_new = mcmapply(ro_kon_1,solusi_1,center_1)
  solusi_1     = lapply(seq_len(ncol(solusi_1_new)), function(i) solusi_1_new[,i])
  
  solusi_2_new = mcmapply(ro_kon_2,solusi_2,center_2)
  solusi_2     = lapply(seq_len(ncol(solusi_2_new)), function(i) solusi_2_new[,i])
  
  solusi_3_new = mcmapply(ro_kon_3,solusi_3,center_3)
  solusi_3     = lapply(seq_len(ncol(solusi_3_new)), function(i) solusi_3_new[,i])
  
  # kita hitung kembali function objective
  f_hit = mcmapply(obj_func,solusi_1,solusi_2,solusi_3,mc.cores = numCores)
  
  pesan = paste0("Iterasi ke: ",iter," hasilnya: ",min(f_hit))
  print(pesan)
}

toc()

# kita akan cek solusinya
n_bhole = which.min(f_hit)

# solusinya
center_1 = solusi_1[[n_bhole]]
center_2 = solusi_2[[n_bhole]]
center_3 = solusi_3[[n_bhole]]

# kita buat dulu ke data frame untuk mengecek semua informasi yang ada
df_temp_1 = df_toko %>% select(nama_toko,long,lat,max_armada)
df_temp_2 = df_order %>% select(nama_toko,sales_order,order_kubikasi,order_tonase,
                                tanggal_kirim_min,tanggal_kirim_max)
df_temp_3 = merge(df_temp_1,df_temp_2) %>% distinct()
df_temp_3$id_armada        = round(as.vector(center_1),0) # kita rounding dulu ya
df_temp_3$tanggal_kirim    = round(as.vector(center_2),0) # kita rounding dulu ya
df_temp_3$armada_ke        = round(as.vector(center_3),0)
df_temp_3                  = merge(df_temp_3,df_jenis_armada)

nama_file_rda = paste0(target_gudang," done new 20.rda")

save(df_temp_3,file = nama_file_rda)

# ==============================================================================

# catatan 8 armada jadinya 66


# catatan terbaik saat ini 8 dengan hasil 61.96
# ciawi done ver baru I.rda hasilnya 57
# ciawi versi 4 hasilnya 54

nama_file_rda











