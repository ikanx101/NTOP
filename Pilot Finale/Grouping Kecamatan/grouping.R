rm(list=ls())

setwd("~/NTOP/Pilot Finale/Grouping Kecamatan")

library(dplyr)
library(tidyr)

# mengambil data
df = read.csv("~/NTOP/Pilot Finale/Data Mentah/dbase kodepos longlat.csv")

