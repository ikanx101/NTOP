rm(list=ls())

# scraper pakai komputernya mas kodirin
# perhatikan perubahan setting yang ada

# libraries
library(dplyr)
library(rvest)
library(RSelenium)
library(tidyr)

# baca data
df       = read.csv("kode_pos_all.csv")
df$url   = NA
keyword  = paste(df$kecamatan,df$kota_kab,df$provinsi,sep = ",")


# buka url utama
url = "https://www.google.com/maps"

# version chrome
version = "108.0.5359.71"

# memulai selenium
driver =  RSelenium::rsDriver(browser = "chrome",
                              chromever = version)
remDr  = driver[["client"]]


# function untuk memasukkan keyword
cari_keyword = function(keyword){
  css_cari = "#searchboxinput"
  cari_key = remDr$findElement("css", css_cari)
  Sys.sleep(runif(1,3,5))
  cari_key$sendKeysToElement(list(keyword,key = "enter"))
}

# untuk delete box
delete = function(n){
  css_cari = "#searchboxinput"
  cari_key = remDr$findElement("css", css_cari)
  Sys.sleep(runif(1,3,5))
  for(ix in 1:n){
    cari_key$sendKeysToElement(list(key = "backspace"))
  }
}

# kita mulai looping
hasil = vector("list",length(keyword))
for(i in 1:length(keyword)){
  remDr$navigate(url)
  Sys.sleep(runif(1,2,3))
  cari_keyword(keyword[i])
  Sys.sleep(runif(1,3,5))
  df$url[i]  = remDr$getCurrentUrl()[[1]]
  hasil[[i]] = df[i,]
  print(i)
  Sys.sleep(runif(1,1,2))
}

hasil_final = do.call(rbind,hasil)
write.csv(hasil_final,"kode_pos_longlat_done.csv",row.names = T)

save(hasil_final,file = "hasil kodepos longlat.rda")