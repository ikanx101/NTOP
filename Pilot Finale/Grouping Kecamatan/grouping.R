rm(list=ls())

setwd("~/NTOP/Pilot Finale/Grouping Kecamatan")

library(dplyr)
library(tidyr)
library(cluster)
library(caret)
library(factoextra)
library(ggplot2)


# mengambil data
df      = read.csv("~/NTOP/Pilot Finale/Data Mentah/dbase kodepos longlat.csv") %>% 
          select(-X) %>% 
          distinct() %>% 
          arrange(provinsi,kota_kab,kecamatan) %>% 
          filter(grepl("jawa|banten|karta|bogor",provinsi,ignore.case = T))

# reserve lat dan long
data_n2 = df %>% select(lat,lng)

# Finding distance matrix
distance_mat = dist(data_n2, method = 'euclidean')

# Fitting Hierarchical clustering Model
set.seed(240)  # Setting seed
Hierar_cl = hclust(distance_mat, method = "average")
Hierar_cl

# Pemecahan menjadi k cluster
# banyak cluster
n_cluster = 9

# kita fit ke modelnya
fit = cutree(Hierar_cl, k = n_cluster)
plot(Hierar_cl)
rect.hclust(Hierar_cl, k = n_cluster, border = "red")
table(fit)

# save hasil cluster ke data awal
df$cluster_1 = fit

# kita arrange dulu
df = df %>% arrange(cluster_1)

# =============================================================================
# ini iterasi kedua
data_n2 = df %>% filter(cluster_1 == 1) %>% select(lat,lng)

# Finding distance matrix
distance_mat = dist(data_n2, method = 'euclidean')

# Fitting Hierarchical clustering Model
set.seed(240)  # Setting seed
Hierar_cl = hclust(distance_mat, method = "average")
Hierar_cl

# Pemecahan menjadi k cluster
# banyak cluster
n_cluster = 11

# kita fit ke modelnya
fit = cutree(Hierar_cl, k = n_cluster)
plot(Hierar_cl)
rect.hclust(Hierar_cl, k = n_cluster, border = "red")
table(fit)

df$cluster_2 = NA
df$cluster_2[1:119] = fit

df = df %>% mutate(cluster_final = ifelse(is.na(cluster_2),
                                          cluster_1 + 11,
                                          cluster_2))

df %>% 
  ggplot(aes(x     = lng,
             y     = lat,
             color = as.factor(cluster_final))) +
  geom_point()

tabel_all = df %>% select(-cluster_1,-cluster_2) %>% arrange(cluster_final) %>% group_split(cluster_final)

library(expss)

wb = createWorkbook()

nama_sheet = "Grouping"
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

saveWorkbook(wb, "grouping.xlsx", overwrite = TRUE)

