

#testboard <- c(0,1,2,3,
#               1,3,0,2,
#               3,2,1,0,
#               2,0,3,1)

onest2 <- c(1,7,12,14)
twost2 <- c(2,5,11,16)
threest2 <- c(3,8,10,13)
fourst2 <- c(4,6,9,15)

onest <- list()
onest[[1]] <- sample(onest2)
for(i in 2:24){
  onest[[i]] <- sample(onest2)
  while((onest[i] %in% onest[1:(i-1)])){
    onest[[i]] <- sample(onest2)
  }
}

twost <- list()
twost[[1]] <- sample(twost2)
for(i in 2:24){
  twost[[i]] <- sample(twost2)
  while((twost[i] %in% twost[1:(i-1)])){
    twost[[i]] <- sample(twost2)
  }
}

threest <- list()
threest[[1]] <- sample(threest2)
for(i in 2:24){
  threest[[i]] <- sample(threest2)
  while((threest[i] %in% threest[1:(i-1)])){
    threest[[i]] <- sample(threest2)
  }
}

fourst <- list()
fourst[[1]] <- sample(fourst2)
for(i in 2:24){
  fourst[[i]] <- sample(fourst2)
  while((fourst[i] %in% fourst[1:(i-1)])){
    fourst[[i]] <- sample(fourst2)
  }
}

checkboardoptions <- list()
iterations <- 0
stop = FALSE
colors <- c("y", "o", "g", "r")
for(i in 1:24){
  checkt1 <- setNames(onest[[i]], colors)
  print(paste0("ones: ",i," - ", onest[i]))
  
  for(j in 1:24){
    checkt2 <- setNames(twost[[j]], colors)
    #print(paste0("twos; ",j," - ", twost[[j]]))

    
    for(k in 1:24){
      checkt3 <- setNames(threest[[k]], colors)
      #print(paste0("threes; ",k," - ", threest[[j]]))
      
      for(l in 1:24){
        checkt4 <- setNames(fourst[[l]], colors)
        checkboard <- sort(c(checkt1,checkt2,checkt3, checkt4))
        
        iterations <- iterations + 1
        checkboardoptions[[iterations]] <- paste0(names(checkboard), collapse="")
        
        #if(iterations == 2000){
        #  print(matrix(names(checkboard),byrow = TRUE, nrow = 4))
        #}
        
        dups1 <- colors %in% names(checkboard[1:4])
        dups2 <- colors %in% names(checkboard[5:8])
        dups3 <- colors %in% names(checkboard[9:12])
        dups4 <- colors %in% names(checkboard[13:16])
        
        dups5 <- colors %in% names(checkboard[c(1,5,9,13)])
        dups6 <- colors %in% names(checkboard[c(2,6,10,14)])
        dups7 <- colors %in% names(checkboard[c(3,7,11,15)])
        dups8 <- colors %in% names(checkboard[c(4,8,12,16)])
      
        if(!(FALSE %in% c(dups1,dups2,dups3,dups4))){
          #print(matrix(names(checkboard),byrow = TRUE, nrow = 4))
          if(!(FALSE %in% c(dups5,dups6,dups7,dups8))){
            print(matrix(names(checkboard),byrow = TRUE, nrow = 4))
            stop = TRUE
            break
          }
        }
      }
      if(stop){break}
    }
    if(stop){break}
  }
  if(stop){break}
}



duplicatedFours <- c(duplicated(checkboardoptions))
TRUE %in% dupclicatedFours

uniqueFours <- c(unique(checkboardoptions))
TRUE %in% uniqueFours

uniqueFours[[1]]
uniqueFours[[331753]]


df <- as.tibble(duplicatedFours)





















