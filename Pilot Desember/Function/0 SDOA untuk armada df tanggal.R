# ==============================================================================
# function untuk membuat matriks jarak
# inputnya adalah df yakni df_toko
buat_matriks_jarak = function(df){
  n_toko = nrow(df)
  # kita tambahin untuk long lat CIAWI
  # df[n_toko+1,] = list(NA) -->>> kita take out dulu sementara
  n_toko = nrow(df)
  # df$long[n_toko] = -6.649061
  # df$lat[n_toko]  = 106.8408808
  
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
# kita akan hitung objective function-nya
obj_func_new = function(list){
  # data awal
  df_temp = 
    order_total_per_toko %>% 
    mutate(armada_ke = list) %>% 
    group_split(armada_ke)
  
  # kita mulai
  n_iterasi    = length(df_temp)
  # constraint pertama: jika lebih dari 5 maka hukum
  constraint_1 = rep(0,n_iterasi)
  # constraint kedua: jika lebih dari tonase maka hukum
  constraint_2 = rep(0,n_iterasi)
  # constraint ketiga: jika lebih dari kubikasi maka hukum
  constraint_3 = rep(0,n_iterasi)
  # hitung jarak
  jarak        = rep(0,n_iterasi)
  for(ix in 1:n_iterasi){
    # ambil lagi
    temp_lagi        = df_temp[[ix]]
    # constraint pertama
    constraint_1[ix] = ifelse(nrow(temp_lagi) > 5,1000,0)
    # constraint kedua
    tes_1 = df_jenis_armada %>% filter(armada == temp_lagi$armada_terpilih[1]) %>% .$max_cap_tonase
    constraint_2[ix] = ifelse(sum(temp_lagi$tonase) <= tes_1,100,0)
    # constraint ketiga
    tes_2 = df_jenis_armada %>% filter(armada == temp_lagi$armada_terpilih[1]) %>% .$max_cap_kubikasi
    constraint_3[ix] = ifelse(sum(temp_lagi$kubik) <= tes_2,100,0)
    # cari TSP
    df_toko_temp = df_toko %>% filter(nama_toko %in% temp_lagi$nama_toko)
    jarak[ix]    = tsp_hitung(df_toko_temp)
  }
  sum(constraint_1 + jarak)
}
# ==============================================================================

