# Ignore the comments. Main code starts from line_num 160

# weights1_vec <- c(0.25,0.75)
# weights2_vec <- c(0.5,0.5)
# weights3_vec <- c(0.75,0.25)

# ret1_vec <- c(5.8,5.5)
# ret2_vec <- c(16.5,12.5)
# ret3_vec <- c(27,20)

# weights1_vec
# weights2_vec
# weights3_vec

# ret1_vec
# ret2_vec
# ret3_vec


#rm(a, b)

# rm(weights1_vec,weights2_vec,weights3_vec)
# rm(ret1_vec,ret2_vec,ret3_vec)

# compute portfolio return
# a %*% b # matrix multiplication
# ret_p1 <- weights1_vec x ret1_vec


# ret_p1 <- t(weights1_vec) %*% ret1_vec
# ret_p1

# n = 3
# Create a matrix
#thismatrix <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
#thismatrix

#thismatrix1 <- matrix(c(5.8,5.5,16.5,12.5,27,20 ), nrow = 3, ncol = 2)

# x_matrix <- matrix(c(5.8,16.5,27,5.5,12.5,20), nrow = 3, ncol = 2)
#  x_matrix

#omega <- (t(x_matrix) %*% x_matrix)/3
#omega


#compute variance of a portfolio
# var_p <- t(weights1_vec) %*% omega1 %*% weights1_vec
# var_p


# spm_1 <- (1-lam1)*ret_p1 - (lam1*var_p)
# spm_1


# ret_p1
# omega2
# var_p
# spm_1

# cat("Accessing the first three rows and the first two columns\n")
# print(A[1:3, 1:2])
#print(state_matrix[2:1, 3:4])

#m[c(1,4), c(2,3)]
#m1[c(1), c(3,4)]


#wt11 <- state_matrix[c(3), c(3,4)]
#ret11 <- state_matrix[c(1), c(1,2)]

# accessing n modifying a specific element in a matrix
#A[3, 3] = 30
# computed_values[27,3] = 107 
# computed_values

# ret_p1 <- t(weights1_vec) %*% ret1_vec
# ret_p1

# ret_p2 <- t(state_matrix[1,3],state_matrix[1,4]) %*% (state_matrix[1,1],state_matrix[1,2])
#ret_p <- t(wt11) %*% ret11
#ret_p

# var_p <- t(weights1_vec) %*% omega1 %*% weights1_vec
#var_p <- t(wt11) %*% omega1 %*% wt11
#var_p



#lam1 <- 0.1

# spm_1 <- (1-lam1)*ret_p1 - (lam1*var_p)

#spm_1 <- (1-lam1)*ret_p - (lam1*var_p)
#spm_1

# inserting computed values of return, variance and spm in computed_values matrix
# for example, updating the first row of computed_values matrix

#computed_values[ 1, ] <- c(ret_p,var_p,spm_1)
#computed_values[ 1, ]

# wt11 <- state_matrix[c(1st row), c(3rd col, 4th col)]
# wt11 <- state_matrix[c(1), c(3,4)]


# ret11 <- state_matrix[c(4), c(1,2)]
# ret11

# wt11 <- state_matrix[c(1), c(3,4)]
# wt11

#colnames(baskets.team) <- c("1st", "2nd", "3th", "4th", "5th", "6th")

#state_matrix

#computed_values

# colnum <- ncol(state_matrix)

#print(i)

#write.csv(my_data, file = "my_data.csv")
# setwd("C:/misc/ABC/2021/")

# combinedmatrix <- cbind(mymatrix1, mymatrix2)

#write.table(row_header, file = "optimal_values.csv", sep = ",", append = TRUE)
#write.table(opt_values2, "abc.txt", append = TRUE, sep = " ", dec = ".",row.names = TRUE, col.names = TRUE)
#write.csv(opt_values2, file = "optimal_values.csv",append=TRUE)
#row_header <- c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm")


#test_str <- "Final computed values of stage 1 for each state: \n------------------------------------------------- \n"
#cat(my_string, my_variable)                        # Applying cat
#cat(test_str)  

#write.csv(combinedmatrix, file = "sdp_test_data1.csv")
#write.csv(opt_values1, file = "optimal_values.csv")


#write.csv(opt_values1, file = "optimal_values.csv")
#write.table(opt_values1, file = "optimal_values.csv", sep = ",", append = TRUE, col.names = FALSE)


#cat(my_string, my_variable)                        # Applying cat
#cat(test_str)                        
#write.csv(combinedmatrix, file = "sdp_test_data2.csv")


# string1 <- "\nFinal optimal values of stage "
# string2 <- " for each state: \n--------------------------------------------------------\n\t"
# t_str1 <- paste(string1, stg_num, string2, sep ="")
# write(t_str1, file = "optimal11_values11.csv", append = TRUE)


#################################################################
#################################################################

# main code starts here

getwd ()
setwd("C:/misc/ABC/2021/nbhm-2021/iimkashipur/term3/cis/cis_topics/cis_word/sdp-testing/sdp_data")

# possible states matrix
state_matrix <- matrix(c(5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,16.5,16.5,16.5,16.5,16.5,16.5,16.5,16.5,16.5,27,27,27,27,27,27,27,27,27,5.5,5.5,5.5,12.5,12.5,12.5,20,20,20,5.5,5.5,5.5,12.5,12.5,12.5,20,20,20,5.5,5.5,5.5,12.5,12.5,12.5,20,20,20,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.25,0.5,0.75,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25,0.75,0.5,0.25), nrow = 27, ncol = 4)
rownames(state_matrix) <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21","s22","s23","s24","s25","s26","s27")
colnames(state_matrix) <- c("ret_of_stock1 ", "ret_of_stock2 ", "wt_of_stock1 ","wt_of_stock2 ")
state_matrix


# omega matrix of variance-covariance between stocks in a portfolio 
# [computed from historical returns]
omega1 <- matrix(c(108.4043,2.8716,2.8716,57.0631), nrow = 2, ncol = 2)
rownames(omega1) <- c("stock1 |", "stock2 |")
colnames(omega1) <- c("stock1 |", "stock2 |")
omega1


# computed values of returns, risk, and spm for lamda = 0.1
# initializing to zero
computed_values1 <- matrix(0,27,3)
rownames(computed_values1) <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21","s22","s23","s24","s25","s26","s27")
colnames(computed_values1) <- c("returns_p", "variance_p", "spm")
computed_values1

rownum <- nrow(state_matrix)

########### START OF STAGE 1 ###########

# stage 1 computations
######################

###################################################################
######### fuzzy decision of fuzzified ret_p and var_p #############
###################################################################
# to modify spm_1 only
# no change in exp_spm
# lam1 not reqd

ret_p_min <- 5.0 # assuming minimal ret_p
ret_p_max <- 30.0 # assuming maximum ret_p 

var_p_min <- 30.0 # assuming minimal var_p
var_p_max <- 70.0 # assuming maximum var_p 




  
###################################################################

for (i in 1:rownum) {
  #lam1 <- 0.1
  ret11 <- state_matrix[c(i), c(1,2)]
  wt11 <- state_matrix[c(i), c(3,4)]
  ret_p <- t(wt11) %*% ret11
  var_p <- t(wt11) %*% omega1 %*% wt11
  
  ############ computing membership functons for ret_p and var_p in stage 1 ########

  # fuzzifying ret_p (as fuzzy set) # defining trapezoidal membership function for ret_p
  # denoting membership value of fuzzy set ret_p as lam_ret_p
  
  if (ret_p <= ret_p_min)
  {
    lam_ret_p <- 0.0
  }
  
  if (ret_p >= ret_p_max)
  {
    lam_ret_p <- 1.0
  }
  
  if (ret_p > ret_p_min && ret_p < ret_p_max)  
  {
    lam_ret_p <- (ret_p-ret_p_min) / (ret_p_max-ret_p_min)
  }
  
  # fuzzifying var_p (as fuzzy set) # defining trapezoidal membership function for var_p
  # denoting membership value of fuzzy set var_p as lam_var_p
  if (var_p <= var_p_min)
  {
    lam_var_p <- 1.0
  }
  
  if (var_p >= var_p_max)
  {
    lam_var_p <- 0.0
  }
  
  if (var_p > var_p_min && var_p < var_p_max)  
  {
    lam_var_p <- (var_p_max - var_p) / (var_p_max-var_p_min)
  }
  
  ############ end of computing membership functons for ret_p and var_p in stage 1 ########
  
  ###################################################################
  ######### spm is fuzzy decision of fuzzified ret_p and var_p ######
  ###################################################################
  
  spm_lam <- c(lam_ret_p,lam_var_p)
  spm_1 <- min(spm_lam)
  
  #spm_1 <- (1-lam1)*ret_p - (lam1*var_p)
  
  computed_values1[i, ] <- c(ret_p,var_p,spm_1)  
  rm(ret11,wt11,ret_p,var_p,spm_1)
  #rm(lam1)
    
}

computed_values1

combinedmatrix <- cbind(state_matrix, computed_values1)
combinedmatrix

# writing computed values of spm for stage 1 to output file 'sdp_test_data.csv'

str0 <- "\nComputed values of stage 1 for each state: \n-----------------------------------------------------\n\t"
write(str0, file = "sdp_test_data.csv", append = TRUE)

#thismatrix <- matrix(c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm"), nrow = 1, ncol = 7)

thismatrix <- matrix(c("ret_of_stock1 ", "ret_of_stock2 ", "wt_of_stock1 ","wt_of_stock2 ","returns_p", "variance_p", "spm"), nrow = 1, ncol = 7)
rownames(thismatrix) <- c("Rows/Columns")
write.table(thismatrix, file = "sdp_test_data.csv", sep = ",", append = TRUE, col.names = FALSE)

write.table(combinedmatrix, file = "sdp_test_data.csv", sep = ",", append = TRUE, col.names = FALSE)
combinedmatrix


# computing the optimal weights of stocks in a portfolio for each given state of stock returns


opt_values_temp <- matrix(0,9,7)
rownames(opt_values_temp) <- c("state1","state2","state3","state4","state5","state6","state7","state8","state9")
colnames(opt_values_temp) <- c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm")
opt_values_temp

ctr1 <- 1
for (i in seq(1, rownum, 3)) 
{
  sub_mat <- combinedmatrix[c(i, i+1, i+2),c(1,2,3,4,5,6,7)]
  ctr <-0
  spm_max <- sub_mat[1,7]
  ctr <- 1
  
  for (j in 1:3)
  {
    
     if (spm_max<=sub_mat[j,7])
       {
       spm_max <- sub_mat[j,7]
       ctr <- j
       }
    #print (spm_max)
  }
  
  
  opt_values_temp[ctr1, ] <- sub_mat[c(ctr),c(1,2,3,4,5,6,7)] 
  ctr1 <- ctr1+1
  
}  
opt_values_temp


# writing optimal values for stage 1 to output file 'optimal_values.csv'

str1 <- "\nFinal optimal values of stage 1 for each state: \n--------------------------------------------------------\n\t"
write(str1, file = "optimal_values.csv", append = TRUE)


thismatrix <- matrix(c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm"), nrow = 1, ncol = 7)
rownames(thismatrix) <- c("Rows/Columns")
write.table(thismatrix, file = "optimal_values.csv", sep = ",", append = TRUE, col.names = FALSE)


# creating an array 'optimal_values' of 9 rows, 7 columns and 6 dimesntions and initializing it to zero

array1 <- c(0,0,0,0,0,0,0)
optimal_values <- array(c(array1), dim = c(9, 7, 6))

rownames(optimal_values) <- c("state1","state2","state3","state4","state5","state6","state7","state8","state9")
colnames(optimal_values) <- c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm")

optimal_values

# copying optimal values of stage 1 (from 'opt_values_temp') in  1st dimention of 'optimal_values' array

optimal_values [,,1]<- opt_values_temp
optimal_values [,,1]

write.table(optimal_values [,,1], file = "optimal_values.csv", sep = ",", append = TRUE, col.names = FALSE)

rm(ctr1,ctr,i,j,spm_max,sub_mat,rownum,combinedmatrix,thismatrix,str0,array1,str1)
#rm(combinedmatrix, opt_values)

########### END OF STAGE 1 ###########

##################################################################
##################################################################

# General recursive computations across stages



optimal_values <- matrix(0,9,7)
rownames(optimal_values) <- c("state1","state2","state3","state4","state5","state6","state7","state8","state9")
colnames(optimal_values) <- c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm")
optimal_values

# 3-dimensional array of optimal values with 9 rows (states), 7 columns (ret1, ret2, wt1, wt2, ret_p, var_p, spm) 
# and 6 stages
# optimal_values <- array(c(array1), dim = c(9, 7, 6))

array1 <- c(0,0,0,0,0,0,0)

optimal_values <- array(c(array1), dim = c(9, 7, 6))
rownames(optimal_values) <- c("state1","state2","state3","state4","state5","state6","state7","state8","state9")
colnames(optimal_values) <- c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm")
optimal_values


##################################################################




# recursive computations from stage 2 onwards

# assuming number of stages 6
### recursive computations from stage 2 onwards #######

num_stages <- 6

for(stg_num in 2:num_stages)
{

# reinitialize 'combinedmatrix' to zero
combinedmatrix <- matrix(0,27,7)

# reinitialize 'computed_values_temp' to zero
computed_values_temp <- matrix(0,27,3)

computed_values1 <- matrix(0,27,3)

rownames(computed_values_temp) <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","s21","s22","s23","s24","s25","s26","s27")
colnames(computed_values_temp) <- c("returns_p", "variance_p", "spm")
computed_values_temp


rownum <- nrow(state_matrix)

for (i in 1:rownum) {
  lam1 <- 0.1
  ret11 <- state_matrix[c(i), c(1,2)]
  wt11 <- state_matrix[c(i), c(3,4)]
  ret_p <- t(wt11) %*% ret11
  var_p <- t(wt11) %*% omega1 %*% wt11
  
  
  
  # assuming transition probability of state 1 in stage 2 to any one of 9 possible states in stage 1 equal to (1/9 = 0.1111)
  # expected value of spm for previous states ==> exp_spm
  
  exp_spm <- 0
  stg_tmp <- stg_num-1
  for (k in 1:9) { exp_spm <- exp_spm + optimal_values[k,7,stg_tmp]}
  exp_spm <- 0.1111 * exp_spm
  
  
  
  ############ computing membership functons for ret_p and var_p in stage 1 ########
  
  # fuzzifying ret_p (as fuzzy set) # defining trapezoidal membership function for ret_p
  # denoting membership value of fuzzy set ret_p as lam_ret_p
  
  if (ret_p <= ret_p_min)
  {
    lam_ret_p <- 0.0
  }
  
  if (ret_p >= ret_p_max)
  {
    lam_ret_p <- 1.0
  }
  
  if (ret_p > ret_p_min && ret_p < ret_p_max)  
  {
    lam_ret_p <- (ret_p-ret_p_min) / (ret_p_max-ret_p_min)
  }
  
  # fuzzifying var_p (as fuzzy set) # defining trapezoidal membership function for var_p
  # denoting membership value of fuzzy set var_p as lam_var_p
  if (var_p <= var_p_min)
  {
    lam_var_p <- 1.0
  }
  
  if (var_p >= var_p_max)
  {
    lam_var_p <- 0.0
  }
  
  if (var_p > var_p_min && var_p < var_p_max)  
  {
    lam_var_p <- (var_p_max - var_p) / (var_p_max-var_p_min)
  }
  
  ############ end of computing membership functons for ret_p and var_p in subsequent stages  ########
  
  ###################################################################
  ######### spm is fuzzy decision of fuzzified ret_p and var_p ######
  ###################################################################
  # to modify spm_1 only
  # no change in exp_spm
  #spm_1 <- (1-lam1)*ret_p - (lam1*var_p) + exp_spm
  
  spm_lam <- c(lam_ret_p,lam_var_p)
  spm_1 <- min(spm_lam) +  exp_spm
  
  
  computed_values_temp[i, ] <- c(ret_p,var_p,spm_1)  
  rm(ret11,wt11,ret_p,var_p,spm_1,lam1)
  
}

computed_values_temp

combinedmatrix <- cbind(state_matrix, computed_values_temp)
combinedmatrix


#str0 <- "\nComputed values of stage 2 for each state: \n-----------------------------------------------------\n\t"
#write(str0, file = "sdp_test_data.csv", append = TRUE)


string1 <- "\nFinal computed values of stage "
string2 <- " for each state: \n--------------------------------------------------------\n\t"
t_str1 <- paste(string1, stg_num, string2, sep ="")
write(t_str1, file = "sdp_test_data.csv", append = TRUE)

#thismatrix <- matrix(c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm"), nrow = 1, ncol = 7)

thismatrix <- matrix(c("ret_of_stock1 ", "ret_of_stock2 ", "wt_of_stock1 ","wt_of_stock2 ","returns_p", "variance_p", "spm"), nrow = 1, ncol = 7)
rownames(thismatrix) <- c("Rows/Columns")
write.table(thismatrix, file = "sdp_test_data.csv", sep = ",", append = TRUE, col.names = FALSE)

write.table(combinedmatrix, file = "sdp_test_data.csv", sep = ",", append = TRUE, col.names = FALSE)





# computing the optimal weights of stocks in a portfolio for each given state of stock returns

combinedmatrix

# reinitializing 'opt_values_temp' values to zero
opt_values_temp <- matrix(0,9,7)
opt_values_temp


ctr1 <- 1
for (i in seq(1, rownum, 3)) 
{
  sub_mat <- combinedmatrix[c(i, i+1, i+2),c(1,2,3,4,5,6,7)]
  ctr <-0
  spm_max <- sub_mat[1,7]
  ctr <- 1
  
  for (j in 1:3)
  {
    
    if (spm_max<=sub_mat[j,7])
    {
      spm_max <- sub_mat[j,7]
      ctr <- j
    }
    #print (spm_max)
  }
  
  
  opt_values_temp[ctr1, ] <- sub_mat[c(ctr),c(1,2,3,4,5,6,7)] 
  ctr1 <- ctr1+1
  
}  
opt_values_temp

# copying optimal values of current stage (from 'opt_values_temp') in 'optimal_values' array

optimal_values [,,stg_num]<- opt_values_temp
optimal_values [,,stg_num]


string3 <- "\nFinal optimal values of stage "
string4 <- " for each state: \n--------------------------------------------------------\n\t"
t_str2 <- paste(string1, stg_num, string2, sep ="")
write(t_str2, file = "optimal_values.csv", append = TRUE)

#test_str <- "\nFinal optimal values of stage 2 for each state: \n--------------------------------------------------------\n\t"
#write(test_str, file = "optimal_values.csv", append = TRUE)


thismatrix <- matrix(c("ret_of_stock1 ", "ret_of_stock2 ", "opt_wt_of_stock1 ","opt_wt_of_stock2 ","opt_returns_p", "opt_variance_p", "opt_spm"), nrow = 1, ncol = 7)
rownames(thismatrix) <- c("Rows/Columns")
write.table(thismatrix, file = "optimal_values.csv", sep = ",", append = TRUE, col.names = FALSE)


write.table(optimal_values [,,stg_num], file = "optimal_values.csv", sep = ",", append = TRUE, col.names = FALSE)

# reset initialize 'computed_values_temp' to zero for re-use in next stage computations
computed_values_temp <- matrix(0,27,3)


}

rm(ctr1,ctr,i,j,k,spm_max,sub_mat,rownum,exp_spm,combinedmatrix,thismatrix,stg_num,num_stages,stg_tmp,array1)
rm(t_str1,string1,string2,t_str2,string3,string4)
#rm(test_str,str0)

