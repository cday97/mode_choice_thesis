


board <- c(0, 3, 4, 2, 1, 5, 
           2, 1, 5, 0, 3, 4,
           5, 4, 2, 3, 0, 1, 
           4, 0, 3, 1, 5, 2, 
           3, 5, 1, 4, 2, 0,
           1, 2, 0, 5, 4, 3)

red <- c(1:6)
orange <- c(7:12)
yellow <- c(13:18)
green <- c(19:24)
blue <- c(25:30)
purple <- c(31:36)










colors <- c(1:36)
boardf <- c(rep(0,36))
usedcolors <- c()


while(sumfun(usedcolors,1,6) != 111 | sumfun(usedcolors,7,12) != 111 |
      sumfun(usedcolors,13,18) != 111 | sumfun(usedcolors,19,24) != 111 |
      sumfun(usedcolors,25,30) != 111 | sumfun(usedcolors,31,36) != 111){
  if(sumfun(usedcolors,1,6) > 100){print(usedcolors)}
  for (i in 1:36){
    r <- sample(colors)
    while(((colors[r[i]] + board[i]) %% 6 != 0 | (colors[r[i]] %in% usedcolors[1:i]))){
      r <- sample(colors)
    }
    boardf[i] = colors[r[i]] + board[i]
    usedcolors[i] <- boardf[i] - board[i]
    #print(usedcolors)
  }
}





sumfun<-function(x,start,end){
  return(sum(x[start:end]))
}

sumfun(usedcolors,1,6)




for (i in 1:36){
repeat{
  r[i] <-  floor(runif(1, min=1, max=37))
  if(!(r[i] %in% r[1:i])){
    break
  }
}
}











# BOARD
board <- c(0, 3, 4, 2, 1, 5, 
           2, 1, 5, 0, 3, 4,
           5, 4, 2, 3, 0, 1, 
           4, 0, 3, 1, 5, 2, 
           3, 5, 1, 4, 2, 0,
           1, 2, 0, 5, 4, 3)

# BUILD ARRAYS
sixes2 <- c(1,10,17,20,30,33)
sixes <- list()
fives2 <- c(5,8,18,22,27,31)
fives <- list()
fours2 <- c(4,7,15,24,29,32)
fours <- list()
threes2 <- c(2,11,16,21,25,36)
threes <- list()
twos2 <- c(3,12,14,19,28,35)
twos <- list()
ones2 <- c(6,9,13,23,26,34)
ones <- list()

for(i in 1:36){
  sixes[[i]] <- sample(sixes2)
  while((!(sixes[i] %in% sixes[1:i]))){
    sixes[[i]] <- sample(sixes2)
  }
}

for(i in 1:36){
  fives[[i]] <- sample(fives2)
  while((!(fives[i] %in% fives[1:i]))){
    fives[[i]] <- sample(fives2)
  }
}

for(i in 1:36){
  fours[[i]] <- sample(fours2)
  while((!(fours[i] %in% fours[1:i]))){
    fours[[i]] <- sample(fours2)
  }
}

for(i in 1:36){
  threes[[i]] <- sample(threes2)
  while((!(threes[i] %in% threes[1:i]))){
    threes[[i]] <- sample(threes2)
  }
  #setNames(threes[[i]], c(1:6)) doesn't work
}

for(i in 1:36){
  twos[[i]] <- sample(twos2)
  while((!(twos[i] %in% twos[1:i]))){
    twos[[i]] <- sample(twos2)
  }
}

for(i in 1:36){
  ones[[i]] <- sample(ones2)
  while((!(ones[i] %in% ones[1:i]))){
    ones[[i]] <- sample(ones2)
  }
}

# WRITE CHECK
colors <- c("r","o","y","g","b","p")
check1 <- setNames(ones[[4]], colors)
check2 <- setNames(twos[[13]], colors)
check3 <- setNames(threes[[7]], colors)
check4 <- setNames(fours[[1]], colors)
check5 <- setNames(fives[[8]], colors)
check6 <- setNames(sixes[[14]], colors)

checkboard <- sort(c(check1,check2,check3,check4,check5,check6))
checkboard


dups1 <- colors %in% names(checkboard[1:6])
dups2 <- colors %in% names(checkboard[7:12])
dups3 <- colors %in% names(checkboard[13:18])
dups4 <- colors %in% names(checkboard[19:24])
dups5 <- colors %in% names(checkboard[25:30])
dups6 <- colors %in% names(checkboard[31:36])

FALSE %in% c(dups1,dups2,dups3,dups4,dups5,dups6)


# WRITE LOOPS
iterations <- 0
for(i in 1:36){
  print(paste0("ones: ",i, " - ",  ones[i]))
  for(j in 1:36){
    print(paste0("twos: ", j, " - ",  twos[j]))
    for(k in 1:36){
      print(paste0("threes: ", k, " - ",  threes[k]))
      for(l in 1:36){
        #print(paste0("fours: ", l, " - ",  fours[l]))
        for(m in 1:36){
          #print(paste0("fives: ", m, " - ", fives[m]))
          for(n in 1:36){
            #print(paste0("sixes: ", n, " - ", sixes[n]))
            
            colors <- c("r","o","y","g","b","p")
            check1 <- setNames(ones[[i]], colors)
            check2 <- setNames(twos[[j]], colors)
            check3 <- setNames(threes[[k]], colors)
            check4 <- setNames(fours[[l]], colors)
            check5 <- setNames(fives[[m]], colors)
            check6 <- setNames(sixes[[n]], colors)
            checkboard <- sort(c(check1,check2,check3,check4,check5,check6))
            #checkboard
            #print(checkboard)
            
            dups1 <- colors %in% names(checkboard[1:6])
            dups2 <- colors %in% names(checkboard[7:12])
            dups3 <- colors %in% names(checkboard[13:18])
            dups4 <- colors %in% names(checkboard[19:24])
            dups5 <- colors %in% names(checkboard[25:30])
            dups6 <- colors %in% names(checkboard[31:36])
            
           if(!(FALSE %in% c(dups1,dups2,dups3,dups4,dups5,dups6))){
             print(paste0("We did it! Answer: ", checkboard))
             break
           }
           iterations <- iterations + 1
           if((iterations %% 10000) < 1){print(iterations)}
            
          }
        }
      }
    }
  }
}



ones[[4]]






