onest2 <- c(3,7,15,19,21)
twost2 <- c(2,9,11,20,23)
threest2 <- c(4,10,13,16,22)
fourst2 <- c(1,8,14,17,25)
fivest2 <- c(5,6,12,18,24)

onest <- list()
onest[[1]] <- sample(onest2)
for(i in 2:120){
  onest[[i]] <- sample(onest2)
  while((onest[i] %in% onest[1:(i-1)])){
    onest[[i]] <- sample(onest2)
  }
}

twost <- list()
twost[[1]] <- sample(twost2)
for(i in 2:120){
  twost[[i]] <- sample(twost2)
  while((twost[i] %in% twost[1:(i-1)])){
    twost[[i]] <- sample(twost2)
  }
}

threest <- list()
threest[[1]] <- sample(threest2)
for(i in 2:120){
  threest[[i]] <- sample(threest2)
  while((threest[i] %in% threest[1:(i-1)])){
    threest[[i]] <- sample(threest2)
  }
}

fourst <- list()
fourst[[1]] <- sample(fourst2)
for(i in 2:120){
  fourst[[i]] <- sample(fourst2)
  while((fourst[i] %in% fourst[1:(i-1)])){
    fourst[[i]] <- sample(fourst2)
  }
}

fivest <- list()
fivest[[1]] <- sample(fivest2)
for(i in 2:120){
  fivest[[i]] <- sample(fivest2)
  while((fivest[i] %in% fivest[1:(i-1)])){
    fivest[[i]] <- sample(fivest2)
  }
}

checkboardoptions <- list()
iterations <- 0
stop = FALSE
colors <- c("y", "o", "g", "r", "b")
for(i in 1:120){
  checkt1 <- setNames(onest[[i]], colors)
  print(paste0("ones: ",i," - ", onest[i]))
  
  for(j in 1:120){
    checkt2 <- setNames(twost[[j]], colors)
    print(paste0("twos; ",j," - ", twost[[j]]))
    cb2 <- sort(c(checkt1,checkt2))
    if(TRUE %in% c(duplicated(c(names(cb2[1:2]))),duplicated(c(names(cb2[3:4]))),duplicated(c(names(cb2[5:6]))),
                   duplicated(c(names(cb2[7:8]))),duplicated(c(names(cb2[9:10]))))){
      next
      
    }

    for(k in 1:120){
      checkt3 <- setNames(threest[[k]], colors)
      cb3 <- sort(c(checkt1,checkt2,checkt3))
      if(TRUE %in% c(duplicated(c(names(cb3[1:3]))),duplicated(c(names(cb3[4:6]))),duplicated(c(names(cb3[7:9]))),
                     duplicated(c(names(cb3[10:12]))),duplicated(c(names(cb3[13:15]))))){
        next
      }
      
      for(l in 1:120){
        checkt4 <- setNames(fourst[[l]], colors)
        cb4 <- sort(c(checkt1,checkt2,checkt3,checkt4))
        if(TRUE %in% c(duplicated(c(names(cb4[1:4]))),duplicated(c(names(cb4[5:8]))),duplicated(c(names(cb4[9:12]))),
                       duplicated(c(names(cb4[13:16]))),duplicated(c(names(cb4[17:20]))))){
          next
        }
        
        for(m in 1:120){
          checkt5 <- setNames(fivest[[m]], colors)
          
          checkboard <- sort(c(checkt1,checkt2,checkt3, checkt4, checkt5))
          
          iterations <- iterations + 1
          checkboardoptions[[iterations]] <- paste0(names(checkboard), collapse="")
          if((iterations %% 10000) == 0){
            print(iterations)
          }

          dups1 <- colors %in% names(checkboard[1:5])
          dups2 <- colors %in% names(checkboard[6:10])
          dups3 <- colors %in% names(checkboard[11:15])
          dups4 <- colors %in% names(checkboard[16:20])
          dups5 <- colors %in% names(checkboard[21:25])
          
          dups6 <- colors %in% names(checkboard [c(1,6, 11,16,21)])
          dups7 <- colors %in% names(checkboard [c(2,7, 12,17,22)])
          dups8 <- colors %in% names(checkboard [c(3,8, 13,18,23)])
          dups9 <- colors %in% names(checkboard [c(4,9, 14,19,24)])
          dups10 <- colors %in% names(checkboard[c(5,10,15,20,25)])
          
          if(!(FALSE %in% c(dups1,dups2,dups3,dups4,dups5))){
            #print(matrix(names(checkboard),byrow = TRUE, nrow = 5))
            if(!(FALSE %in% c(dups6,dups7,dups8,dups9,dups10))){
              print(matrix(names(checkboard),byrow = TRUE, nrow = 5))
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
  if(stop){break}
}
