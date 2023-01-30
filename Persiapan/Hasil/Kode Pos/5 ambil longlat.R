rm(list=ls())

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
url = "https://www.google.co.id/maps/@-6.1834007,106.9240557,15z"

# nyalakan jika diperlukan
# system('docker run -d -p 4445:4444 selenium/standalone-firefox')
remDr = remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
remDr$open()

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
  Sys.sleep(runif(1,3,5))
  cari_keyword(keyword[i])
  Sys.sleep(runif(1,3,5))
  df$url[i]  = remDr$getCurrentUrl()[[1]]
  hasil[[i]] = df[i,]
  Sys.sleep(runif(1,3,5))
}


