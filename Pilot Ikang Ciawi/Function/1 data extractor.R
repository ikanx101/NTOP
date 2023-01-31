rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(parallel)

# set jumlah cores
numcore = 10

# folder data
path = "~/Nutrifood Transporter Optimization/Pilot Ikang Ciawi/Data Mentah"
file = list.files(path,pattern = "*csv")