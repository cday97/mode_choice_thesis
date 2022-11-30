
onest2 <- c(1,5,9)#c(2,4,9)
twost2 <- c(2,6,7)#c(3,5,7)
threest2 <- c(3,4,8)#c(1,6,8)

#3x3 example. Works!!
onest <- list()
onest[[1]] <- sample(onest2)
for(i in 2:6){
  onest[[i]] <- sample(onest2)
  while((onest[i] %in% onest[1:(i-1)])){
    onest[[i]] <- sample(onest2)
  }
}

twost <- list()
twost[[1]] <- sample(twost2)
for(i in 2:6){
  twost[[i]] <- sample(twost2)
  while((twost[i] %in% twost[1:(i-1)])){
    twost[[i]] <- sample(twost2)
  }
}

threest <- list()
threest[[1]] <- sample(threest2)
for(i in 2:6){
  threest[[i]] <- sample(threest2)
  while((threest[i] %in% threest[1:(i-1)])){
    threest[[i]] <- sample(threest2)
  }
}

checkboardoptions <- list()

iterations <- 0
stop = FALSE
colors <- c("y", "o", "g")
for(i in 1:6){
  checkt1 <- setNames(onest[[i]], colors)
  print(paste0("ones: ",i," - ", onest[i]))
  
  for(j in 1:6){
    checkt2 <- setNames(twost[[j]], colors)
    #print(paste0("twos; ",j," - ", twost[[j]]))
    
    for(k in 1:6){
      checkt3 <- setNames(threest[[k]], colors)
      checkboard <- sort(c(checkt1,checkt2,checkt3))

      iterations <- iterations + 1
      checkboardoptions[[iterations]] <- paste0(names(checkboard), collapse="")
      
      #if(iterations == 32){
      #  print(matrix(names(checkboard),byrow = TRUE, nrow = 3))
      #}

      dups1 <- colors %in% names(checkboard[1:3])
      dups2 <- colors %in% names(checkboard[4:6])
      dups3 <- colors %in% names(checkboard[7:9])
      
      dups4 <- colors %in% names(checkboard[c(1,4,7)])
      dups5 <- colors %in% names(checkboard[c(2,5,8)])
      dups6 <- colors %in% names(checkboard[c(3,6,9)])
      
      if(!(FALSE %in% c(dups1,dups2,dups3))){
        #print(matrix(names(checkboard),byrow = TRUE, nrow = 3))
        if(!(FALSE %in% c(dups4,dups5,dups6))){
          print(matrix(names(checkboard),byrow = TRUE, nrow = 3))
          print(iterations)
          stop = TRUE
          break
        }
      
     }
    }
    if(stop){break}
  }
  if(stop){break}
  
}

duplicated(checkboardoptions)
