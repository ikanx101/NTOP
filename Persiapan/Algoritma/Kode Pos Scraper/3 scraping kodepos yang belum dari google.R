rm(list=ls())

# function untuk scraping kode pos yang belum didapatkan dari situs sebelumnya

# libraries
library(dplyr)
library(rvest)
library(RSelenium)
library(tidyr)

# kita ambil kode pos yang belum terpetakan
csv      = "kode_pos_belum.csv"
df       = read.csv(csv)
kode_pos = df$kode_pos %>% unique()

# kita akan set nama keywords-nya
keywords = paste0("https://carikodepos.com/?s=",kode_pos)

# kita akan buat scrapernya
scraper_kodepos = function(url_cek){
  # ambil detail
  df = 
    url_cek %>% 
    read_html() %>% {
      tibble(
        provinsi  = html_nodes(.,"td:nth-child(1) a") %>% html_text(),
        kota_kab  = html_nodes(.,"td:nth-child(2) a") %>% html_text(),
        kecamatan = html_nodes(.,"td:nth-child(3) a") %>% html_text()
      )
    }
  if(nrow(df) < 1){df = data.frame(provinsi = NA,kota_kab = NA, kecamatan = NA)}
  df$kode_pos = gsub("https://carikodepos.com/?s=","",url_cek,fixed = T)
  return(df)
}

# kita akan mulai
hasil = vector("list",length(keywords))
for(i in 1:length(keywords)){
  hasil[[i]] = scraper_kodepos(keywords[i])
  print(i)
}

# kita gabung
final = do.call(rbind,hasil) %>% distinct()

# kita hanya export yang berhasil saja
final %>% 
  filter(!is.na(provinsi)) %>% 
  write.csv("kode_pos_done_tahap_2.csv",row.names = F)

# yang belum
final %>% filter(is.na(provinsi)) %>% write.csv("kode_pos_done_tahap_3.csv",row.names = F)
