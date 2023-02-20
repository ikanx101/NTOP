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
        hasil[ix] = sample(c(min[ix]:(min[ix]+4)),1) 
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
  return(n_tanggal + punish)
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
