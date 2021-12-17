#Final Q1
#Hyeseon Seo 

source("Desktop/Intro_Computing/Final/final_q1.R")

set.seed(1234567890)

#profile calls in a block.
x = matrix(rnorm(1000*500), 1000, 500)
y = rnorm(1000)
r_squared_fun_1(x,y)
r_squared_fun_2(x,y)
r_squared_fun_3(x,y)
r_squared_fun_4(x,y)
r_squared_fun_5(x,y)

Rprof(filename = 'final_q1.out', filter.callframes = T, interval = 0.001)
r_squared_fun_1(x,y)
r_squared_fun_2(x,y)
r_squared_fun_3(x,y)
r_squared_fun_4(x,y)
r_squared_fun_5(x,y)
Rprof(NULL)


w <- summaryRprof('final_q1.out')$by.total
w
v<- rownames(w)
v
# creating 5X20 matrix which is row name is 5 functions 
# and columns representing total time 

profile_result <- matrix(rep(NA, 100), nrow = 5,ncol = 20, 
                         dimnames = list(c("r_squared_fun_1","r_squared_fun_2","r_squared_fun_3", "r_squared_fun_4","r_squared_fun_5"), c(v)))

# call profile for each function

Rprof(filename = 'final_q1_f1.out', filter.callframes = T, interval = 0.001)
r_squared_fun_1(x,y)
Rprof(NULL)
f1 <- summaryRprof('final_q1_f1.out')$by.total[1] 

Rprof(filename = 'final_q1_f2.out', filter.callframes = T, interval = 0.001)
r_squared_fun_2(x,y)
Rprof(NULL)
f2 <- summaryRprof('final_q1_f2.out')$by.total[1] 

Rprof(filename = 'final_q1_f3.out', filter.callframes = T, interval = 0.001)
r_squared_fun_3(x,y)
Rprof(NULL)
f3 <- summaryRprof('final_q1_f3.out')$by.total[1] 

Rprof(filename = 'final_q1_f4.out', filter.callframes = T, interval = 0.001)
r_squared_fun_4(x,y)
Rprof(NULL)
f4 <- summaryRprof('final_q1_f4.out')$by.total[1] 

Rprof(filename = 'final_q1_f5.out', filter.callframes = T, interval = 0.001)
r_squared_fun_5(x,y)
Rprof(NULL)
f5 <- summaryRprof('final_q1_f5.out')$by.total[1] 

f1
f2
f3
f4
f5

## extract summary information to the 5X20 matrix
for( i in 1:length(rownames(f1))) {
  for(j in 1:length(colnames(profile_result))){
    if(rownames(f1)[i] == colnames(profile_result)[j]){
      profile_result[1,j] <- f1[i,1]
    }
  } 
} 

for( i in 1:length(rownames(f2))) {
  for(j in 1:length(colnames(profile_result))){
    if(rownames(f2)[i] == colnames(profile_result)[j]){
      profile_result[2,j] <- f2[i,1]
    }
  } 
}

for( i in 1:length(rownames(f3))) {
  for(j in 1:length(colnames(profile_result))){
    if(rownames(f3)[i] == colnames(profile_result)[j]){
      profile_result[3,j] <- f3[i,1]
    }
  } 
}

for( i in 1:length(rownames(f4))) {
  for(j in 1:length(colnames(profile_result))){
    if(rownames(f4)[i] == colnames(profile_result)[j]){
      profile_result[4,j] <- f4[i,1]
    }
  } 
}

for( i in 1:length(rownames(f5))) {
  for(j in 1:length(colnames(profile_result))){
    if(rownames(f5)[i] == colnames(profile_result)[j]){
      profile_result[5,j] <- f5[i,1]
    }
  } 
}
profile_result

# 5X6 matrix whose row are summaries of the 5 rows form the profiling matrix

as.matrix(profile_result) 
profile_summary <- summary(profile_result[1,])
for(i in 2: nrow(profile_result)){
   profile_summary <- rbind(profile_summary, summary(profile_result[i,]))
}

as.matrix(profile_summary)
row.names(profile_summary) <- rownames(profile_result)
profile_summary <- profile_summary[,-7]
profile_summary

# > profile_summary
#                  Min. 1st Qu. Median       Mean 3rd Qu.  Max.
# r_squared_fun_1 0.003 0.00475 0.0100 0.04383333 0.08425 0.128
# r_squared_fun_2 0.001 0.00125 0.0025 0.03266667 0.06975 0.097
# r_squared_fun_3 0.001 0.00150 0.0280 0.05085714 0.07350 0.177
# r_squared_fun_4 0.001 0.00175 0.0035 0.08600000 0.22300 0.229
# r_squared_fun_5 0.001 0.00100 0.0010 0.06433333 0.18900 0.193

## From the brief descriptions above, 
# function2 is most efficient, because time spending on average is the shortest,
# and max is also smallest among the 5 functions.

