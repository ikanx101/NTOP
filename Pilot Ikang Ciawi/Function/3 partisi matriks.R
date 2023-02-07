rm(list=ls())

library(dplyr)

# function matriks rotasi
buat_rot_mat = function(kali,n){
  # buat template sebuah matriks identitas
  temp_mat = matrix(0,ncol = n,nrow = n)
  diag(temp_mat) = 1
  
  theta = kali / 360
  # buat matriks identitas terlebih dahulu
  mat_rot = temp_mat
  
  for(i in 1:(n-1)){
    for(j in 1:i){
      temp = temp_mat
      idx = n-i
      idy = n+1-j
      # print(paste0("Matriks rotasi untuk ",idx," - ",idy,": DONE"))
      temp[idx,idx] = cos(theta)
      temp[idx,idy] = -sin(theta)
      temp[idy,idx] = sin(theta)
      temp[idy,idy] = cos(theta)
      # assign(paste0("M",idx,idy),temp)
      mat_rot = mat_rot %*% temp
      mat_rot = mat_rot 
    }
  }
  return(mat_rot)
}


norm_vec = function(x){
  y = sqrt(sum(x^2))
  return(y)
}

b = runif(132)
A = buat_rot_mat(10,length(b))

b_ = b %*% A

norm_vec(b)
norm_vec(b_)

b1 = b[1:5]
b2 = b[6:10]

A1 = A[1:5,1:5]
A2 = A[6:10,1:5]
A3 = A[1:5,6:10]
A4 = A[6:10,6:10]

c1 = b1 %*% A1 + b2 %*% A2
c2 = b1 %*% A3 + b2 %*% A4

c_ = c(c1,c2)
norm_vec(c_)
