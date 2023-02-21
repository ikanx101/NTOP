# ==============================================================================
# function untuk membuat matriks jarak
# inputnya adalah df yakni df_toko
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


# ==============================================================================
# ini adalah function untuk mencari semuanya
cari_armada_donk = function(temp){
  # sekarang kita akan hitung
  n_toko_delivery         = length(unique(temp$nama_toko))
  
  # kita hitung dulu total order per toko
  order_total_per_toko = 
    temp %>% 
    group_by(nama_toko) %>% 
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
      mutate(berapa = ceiling(n/5)) %>% 
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
    for(iter in 1:10){
      # kita cari dulu mana yang akan jadi pusat
      n_bhole = which.min(f_hit)
      
      # kita jadikan center of gravity
      center_1         = calon_solusi[[n_bhole]]
      
      calon_solusi_new = mcmapply(ro_kon,calon_solusi,mc.cores = numcore)
      calon_solusi     = lapply(1:100, function(i) calon_solusi_new[,i])
      
      # kita hitung kembali function objective
      f_hit = mcmapply(obj_func_new,calon_solusi,mc.cores = numcore)
      # kita print terlebih dahulu
      cat(paste0(f_hit[which.min(f_hit)],"..."))
    }
    # kita cari dulu mana yang akan jadi pusat
    n_bhole = which.min(f_hit)
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
  output
  
  return(output)
}
