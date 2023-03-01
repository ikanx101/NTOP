# generate tanggal kirim sesuai dengan data yang ada pada df_order
tanggal_generate = function(dummy){
  hasil = rep(0,nrow(temp))
  # ambil min dan max per baris
  min   = temp[["tanggal_kirim_min"]] %>% as.numeric()
  max   = temp[["tanggal_kirim_max"]] %>% as.numeric()
  
  if(length(unique(min)) == 1){hasil = min}
  if(length(unique(min)) != 1){
    for(ix in 1:nrow(temp)){
      if(min[ix] == max[ix]){
        hasil[ix] = min[ix]
      }
      if(min[ix] != max[ix]){
        # kita paksakan selesai dalam waktu yang sesingkat-singkatnya
        # hasil[ix] = sample(c(min[ix]:(min[ix]+4)),1) awal seperti ini
        hasil[ix] = sample(c(min[ix]:(min[ix] + 3)),1) # kita modif dulu ya
      }
    }
  }
  return(hasil)
}


# menghitung objective function
obj_func = function(list){
  temp = temp %>% 
    mutate(tanggal_kirim = list) %>% 
    mutate(marker = ifelse(tanggal_kirim <= tanggal_kirim_max & tanggal_kirim >= tanggal_kirim_min,
                           0,
                           1))
  # sebisa mungkin yang paling sedikit
  n_tanggal = length(unique(list)) - 2
  n_tanggal = max(n_tanggal,0)^2 * 10
  # dihukum jika lebih dari tanggal yang seharusnya
  punish    = sum(temp$marker) * 10000
  # kita akan hukum jika melebihi max capacity
  punish_lagi = temp %>% 
                group_by(tanggal_kirim) %>% 
                summarise(ton   = sum(order_tonase),
                          kubik = sum(order_kubikasi)) %>% 
                ungroup() %>% 
                mutate(marker_1 = ifelse(ton > 20000,100,0),
                       marker_2 = ifelse(kubik > 45,100,0))
  return(n_tanggal + punish + sum(punish_lagi$marker_1) + sum(punish_lagi$marker_2))
}

# kita buat lagi rotation matriksnya
mat_rotasi = buat_rot_mat(2*pi / 30,nrow(temp))

# fungsi untuk rotasi dan kontraksi
ro_kon = function(list){
  Xt_2 = list
  # kita rotasikan dan konstraksikan
  X2 = mat_rotasi %*% (Xt_2 - center_1)
  X2 = center_1 + (.6 * X2)
  X2 = round(X2,0)
  return(X2)
}

# fungsi untuk fine tuning tanggal pengiriman
utak_atik_tanggal = function(temp){
  # kita ambil 2 tanggal yang muncul paling sedikit
  tgl_terkecil = 
    temp %>% 
    group_by(tanggal_kirim) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(n) %>% 
    .$tanggal_kirim
  
  if(length(tgl_terkecil) > 1){
    # kita ubah dan hitung apakah mungkin?
    # kita tukar
    for(ix in 2:length(tgl_terkecil)){
      # ini adalah proses tukar menukar tanggal
      temp_2 = 
        temp %>% 
        rowwise() %>% 
        mutate(tanggal_kirim_new = ifelse(tanggal_kirim == tgl_terkecil[ix-1],
                                          tgl_terkecil[ix],
                                          tanggal_kirim)) %>%
        ungroup() %>% 
        rowwise() %>% 
        mutate(marker = ifelse(tanggal_kirim_new <= tanggal_kirim_max & 
                                 tanggal_kirim_new >= tanggal_kirim_min,
                               0,
                               1)) %>% 
        mutate(tanggal_kirim_final = ifelse(marker == 1,tanggal_kirim,tanggal_kirim_new)) %>% 
        ungroup() %>% 
        select(-tanggal_kirim,-marker,-tanggal_kirim_new) %>% 
        rename(tanggal_kirim = tanggal_kirim_final)
      
      # perlu kita perhatikan bahwa tukar menukar boleh saja tapi jangan berlebihan
      temp_3 = 
        temp_2 %>% 
        group_by(tanggal_kirim) %>% 
        summarise(ton   = sum(order_tonase),
                  kubik = sum(order_kubikasi)) %>% 
        ungroup()
      
      # kita buat marker dulu
      temp_4 = 
        temp_3 %>% 
        mutate(marker_1 = ton <= 20000,
               marker_2 = kubik <= 45) %>% 
        mutate(marker_3 = marker_1 + marker_2)
      # seandainya true semua, berarti genap kan
      marker_final = sum(temp_4$marker_3) %% 2
      # kita kembalikan jika melebihi kapasitas
      if(marker_final == 0){temp = temp_2}
      # print dulu
      print("utak atik dulu")
    }
  }
  return(temp)
}


