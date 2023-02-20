# generate tanggal kirim sesuai dengan data yang ada pada df_order
tanggal_generate = function(dummy){
  hasil = rep(0,nrow(temp))
  min   = temp[["tanggal_kirim_min"]] %>% as.numeric()
  max   = temp[["tanggal_kirim_max"]] %>% as.numeric()
  for(i in 1:nrow(temp)){
    if(min[i] == max[i]){
      hasil[i] = min[i]
    }
    if(min[i] != max[i]){
      # kita paksakan selesai dalam waktu yang sesingkat-singkatnya
      hasil[i] = sample(c(min[i]:(min[i] + 3)),1) 
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
  n_tanggal = length(unique(list)) - 3
  n_tanggal = max(n_tanggal,0)^2
  # berapa banyak load pekerjaan
  # seharusnya sehari bisa dapat minimal 25 baris sales order
  load      = nrow(temp) - 10
  load      = max(-load,0) ^ 2
  # dihukum jika lebih dari tanggal yang seharusnya
  punish    = sum(temp$marker) * 100
  return(n_tanggal + load + punish)
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
