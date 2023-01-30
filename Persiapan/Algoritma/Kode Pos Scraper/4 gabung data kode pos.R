rm(list=ls())

library(dplyr)
library(parallel)

# paralel
numcore = 3

ambil = function(nama_file){
  read.csv(nama_file) %>% janitor::clean_names()
}

files = list.files(pattern = "*csv")

ambil_file = mclapply(files,ambil,mc.cores = numcore)

kode_pos = do.call(rbind,ambil_file)


write.csv(kode_pos,"kode_pos_all.csv",row.names = F)


