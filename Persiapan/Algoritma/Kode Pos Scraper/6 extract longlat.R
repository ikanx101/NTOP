rm(list=ls())

library(dplyr)
library(tidyr)

load("hasil kodepos longlat.rda")

df = hasil_final
ccc = NA
df$lat = NA

extract_long = function(i){
  temp = df$url[i]
  temp = strsplit(temp,split = "/@") %>% unlist() %>% .[2]
  temp = strsplit(temp,split = "\\,") %>% unlist() %>% .[1:2]
  return(temp[1])
}

extract_lat = function(i){
  temp = df$url[i]
  temp = strsplit(temp,split = "/@") %>% unlist() %>% .[2]
  temp = strsplit(temp,split = "\\,") %>% unlist() %>% .[1:2]
  return(temp[2])
}

i = 1:nrow(df)

library(parallel)
numcore = 10

df$lat = mcmapply(extract_lat,i,mc.cores = numcore)
df$lng = mcmapply(extract_long,i,mc.cores = numcore)

write.csv(df,"dbase kodepos longlat.csv")
