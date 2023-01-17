# bebersih global environment
rm(list=ls())

# ==============================================================================
# libraries yang diperlukan
library(dplyr)
library(tidyr)
library(TSP)

# ==============================================================================
# kita load datanya lagi
load("/cloud/project/input/data dokumentasi.rda")

# ==============================================================================
# kita akan modifikasi si database jenis armada
# yakni dengan mereplikasi baris-baris tergantung dari ketersediaan armada
temp = 
  df_jenis_armada %>% 
  group_split(armada)

i = 1
n_temp = length(temp)
df_hasil = data.frame()
for(i in 1:n_temp){
  df_temp = temp[[i]]
  for(k in 1:df_temp$tersedia){
    df_hasil = rbind(df_temp,df_hasil)
  }
}

# kita kembalikan lagi ke sini
df_jenis_armada = 
  df_hasil %>% 
  arrange(armada) %>% 
  mutate(armada = 1:nrow(df_hasil)) %>% 
  select(-tersedia)

# lalu kita akan ambil data mana yang harus dikerjakan terlebih dahulu
# apakah mau Ciawi atau Cibitung terlebih dahulu?
# misalkan target gudang terlebih dahulu
target_gudang = "ciawi"

# kita filtering terlebih dahulu
df_toko  = df_toko %>% filter(supplied == target_gudang)
df_order = df_order %>% filter(nama_toko %in% df_toko$nama_toko)









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
# berikutnya kita akan buat function generator
# jangan lupa bahwa nanti ada yang harus difilter terlebih dahulu

# generate solusi untuk armada
armada_generate = function(n_toko,n_armada){
  sample(n_armada,n_toko,replace = T)
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
      hasil[i] = sample(c(min[i]:max[i]),1)
    }
  }
  return(hasil)
}


# ==============================================================================
# function untuk membuat matriks jarak
buat_matriks_jarak = function(df){
  n_toko = nrow(df)
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
# kita akan buat function untuk objective function
obj_func = function(list_1,list_2){
  # kita buat dulu ke data frame awalnya
  df_toko$armada_assign <<- list_1
  df_toko$tanggal_kirim <<- list_2
  # kita hitung dulu
  pecah   = df_toko %>% group_split(armada_assign)
  n_pecah = length(pecah)
  jarak = rep(0,n_pecah)
  for(i in 1:n_pecah){
    temp      = pecah[[i]]
    jarak_hit = tsp_hitung(temp)
    jarak[i]  = jarak_hit
  }
  output = 
    list(jarak_detail = jarak,
         jarak_total  = sum(jarak))
  return(output$jarak_total)
}


# ==============================================================================
# sekarang kita akan mulai bagian yang seru
n_toko   = nrow(df_toko)
n_armada = nrow(df_jenis_armada)
n_solusi = 10

# kita buat dulu rumahnya
solusi_1 = vector("list",n_solusi)
solusi_2 = vector("list",n_solusi)

for(i in 1:n_solusi){
  solusi_1[[i]] = armada_generate(n_toko,n_armada)
  solusi_2[[i]] = tanggal_generate(n_toko,df_order)
}

# buat matriks rotasi
mat_rotasi = buat_rot_mat(2*pi / 100,n_toko)

obj_func(solusi_1[[1]],solusi_2[[1]]) 


