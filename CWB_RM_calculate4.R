
# Seperate rainfall 
count <-0
i <- 0
z <- 1
k <- 1
list.rain <- list()
while (i < (length(data)-360)  ) {
  sum <- 0
  
  if(i%%100000==0){cat("i=",i,"\n")}
  
  for(j in 1:360){
    sum <- sum+data[i+j]
  }
  
  if(sum > 1.27){
    count <- count+1
    i <- i+1
    next
    
  }else{
    
    if(count == 0 ){
      k <- i+1
      i <- i+1
      next
    }
    
    list.rain[[z]] <- data[(k):(i+360)]
    
    z <- z+1
    i <- i+361
    k <- i
    datarain <- NULL
    count <- 0
  }
}


list.effrain <- list()
j <- 1
k <- 0
for(a in 1:length(list.rain) ){
  print(a)
  i <- list.rain[[a]]
  
  if(sum(i) >= 12.7){
    list.effrain[[j]] <- i
    j <- j+1
  }else{
    
    for(k in 0: (length(i)-15) ){
        if( sum(i[(k+1):(k+15)]) > 6.35 ) {
          list.effrain[[j]] <- i
          j <- j+1
          break;
        }else{
          k <- k+1
        }
          
    }
 
    
  }
  
}

## RM
## 
list.em <- list()
list.I30 <- list()
 Rm <- vector()
   m <- 1
for(i in 1:length(list.effrain)){
  em.unit <- vector()
  em <- vector()

  rain <- list.effrain[[i]]

  for(j in 1 : length(rain)){
    if( (rain[j]*60) <=76 ){
      em.unit[j] <- 0.119 + ( 0.0873 * log10( (rain[j]*60) ) )
      if(em.unit[j] == -Inf){ em.unit[j] <- 0.0 }
      if(is.nan(em.unit[j]) == T ){ em.unit[j] <- 0.0 }
      em[j] <-  em.unit[j] * rain[j]
      
    }else{
      em.unit[j] <- 0.283
      if(em.unit[j] == -Inf){ em.unit[j] <- 0.0 }
      if(is.nan(em.unit[j]) == T ){ em.unit[j] <- 0.0 }
      em[j] <-  em.unit[j] * rain[j]
    }

  }
  
  list.em[[m]] <-  em
  
  I30 <- 0
  rain30 <- 0  

  for(k in 0: (length(rain)-30) ){
    rain30 <-  sum(rain[(k+1):(k+30)] )
    if(rain30> I30){ I30 <- rain30 }
    k <- k+1
  }
  
  I30 <- I30*2
  
  list.I30[[m]] <-  I30
  m=m+1
  Rm[i] <-  sum(em) * I30
  print(i)
  
  
}

print ( sum(Rm)/5 ) 



# function ----------------------------------------------------------------

A0W030_1minute <- read.table(file=("./Data-goverment/A0W030-1minutes-rainfalldata-modify.txt"),header=T,sep=",")
C0W140_1minute <- read.table(file=("./Data-goverment/C0W140-1minutes-rainfalldata.txt"),header=T,sep=",")
C0W150_1minute <- read.table(file=("./Data-goverment/C0W150-1minutes-rainfalldata.txt"),header=T,sep=",")
k467110_1minute <- read.table(file=("./Data-goverment/467110-1minutes-rainfalldata.txt"),header=T,sep=",")



data1 <- C0W140_1minute$minrain
data2 <- C0W150_1minute$minrain
data3 <- k467110_1minute$minrain
data4 <- A0W030_1minute$A0W030_rain

list.data <- list()

list.data[[1]] <- data1
list.data[[2]] <- data2
list.data[[3]] <- data3
list.data[[4]] <- data4


Rm_calculate <- function(data,year=5){
  
  # seperate rainfall 
  count <-0
  i <- 0
  z <- 1
  k <- 1
  list.rain <- list()
  while (i < (length(data)-360)  ) {
    sum <- 0
    
    if(i%%100000==0){cat("i=",i,"\n")}
    
    for(j in 1:360){
      sum <- sum+data[i+j]
    }
    
    if(sum > 1.27){
      count <- count+1
      i <- i+1
      next
      
    }else{
      
      if(count == 0 ){
        k <- i+1
        i <- i+1
        next
      }
      
      list.rain[[z]] <- data[(k):(i+360)]
      
      z <- z+1
      i <- i+361
      k <- i
      datarain <- NULL
      count <- 0
    }
  }
  
  
  list.effrain <- list()
  j <- 1
  k <- 0
  for(a in 1:length(list.rain) ){
    print(a)
    i <- list.rain[[a]]
    
    if(sum(i) >= 12.7){
      list.effrain[[j]] <- i
      j <- j+1
    }else{
      
      for(k in 0: (length(i)-15) ){
        if( sum(i[(k+1):(k+15)]) > 6.35 ) {
          list.effrain[[j]] <- i
          j <- j+1
          break;
        }else{
          k <- k+1
        }
        
      }
      
      
    }
    
  }
  
  ## RM
  ## 
  list.em <- list()
  list.I30 <- list()
  Rm <- vector()
  m <- 1
  for(i in 1:length(list.effrain)){
    em.unit <- vector()
    em <- vector()
    
    rain <- list.effrain[[i]]
    
    for(j in 1 : length(rain)){
      if( (rain[j]*60) <=76 ){
        em.unit[j] <- 0.119 + ( 0.0873 * log10( (rain[j]*60) ) )
        if(em.unit[j] == -Inf){ em.unit[j] <- 0.0 }
        if(is.nan(em.unit[j]) == T ){ em.unit[j] <- 0.0 }
        em[j] <-  em.unit[j] * rain[j]
        
      }else{
        em.unit[j] <- 0.283
        if(em.unit[j] == -Inf){ em.unit[j] <- 0.0 }
        if(is.nan(em.unit[j]) == T ){ em.unit[j] <- 0.0 }
        em[j] <-  em.unit[j] * rain[j]
      }
      
    }
    
    list.em[[m]] <-  em
    
    I30 <- 0
    rain30 <- 0  
    
    for(k in 0: (length(rain)-30) ){
      rain30 <-  sum(rain[(k+1):(k+30)] )
      if(rain30> I30){ I30 <- rain30 }
      k <- k+1
    }
    
    I30 <- I30*2
    
    list.I30[[m]] <-  I30
    m=m+1
    Rm[i] <-  sum(em) * I30
    print(i)
    
    
  }
  
  print ( sum(Rm)/year ) 
  
  return(sum(Rm)/year)
  
}

kinmenrm <- lapply(list.data, FUN = Rm_calculate)






