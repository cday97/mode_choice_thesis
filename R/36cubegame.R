


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











# BOARD
board <- c(0, 3, 4, 2, 1, 5, 
           2, 1, 5, 0, 3, 4,
           5, 4, 2, 3, 0, 1, 
           4, 0, 3, 1, 5, 2, 
           3, 5, 1, 4, 2, 0,
           1, 2, 0, 5, 4, 3)

# BUILD ARRAYS
sixes2 <- c(1,10,17,20,30,33)
fives2 <- c(5,8,18,22,27,31)
fours2 <- c(4,7,15,24,29,32)
threes2 <- c(2,11,16,21,25,36)
twos2 <- c(3,12,14,19,28,35)
ones2 <- c(6,9,13,23,26,34)

sixes <- list()
sixes[[1]] <- sample(sixes2)
for(i in 2:720){
  sixes[[i]] <- sample(sixes2)
  while((sixes[i] %in% sixes[1:(i-1)])){
    #print(sixes[[i]])
    sixes[[i]] <- sample(sixes2)
  }
}

fives <- list()
fives[[1]] <- sample(fives2)
for(i in 2:720){
  fives[[i]] <- sample(fives2)
  while((fives[i] %in% fives[1:(i-1)])){
    fives[[i]] <- sample(fives2)
  }
}

fours <- list()
fours[[1]] <- sample(fours2)
for(i in 2:720){
  fours[[i]] <- sample(fours2)
  while((fours[i] %in% fours[1:(i-1)])){
    fours[[i]] <- sample(fours2)
  }
}

threes <- list()
threes[[1]] <- sample(threes2)
for(i in 2:720){
  threes[[i]] <- sample(threes2)
  while((threes[i] %in% threes[1:(i-1)])){
    threes[[i]] <- sample(threes2)
  }
}

twos <- list()
twos[[1]] <- sample(twos2)
for(i in 2:720){
  twos[[i]] <- sample(twos2)
  while((twos[i] %in% twos[1:(i-1)])){
    twos[[i]] <- sample(twos2)
  }
}

ones <- list()
ones[[1]] <- sample(ones2)
for(i in 2:720){
  ones[[i]] <- sample(ones2)
  while((ones[i] %in% ones[1:(i-1)])){
    ones[[i]] <- sample(ones2)
  }
}

# WRITE LOOPS
iterations <- 0
stop = FALSE
colors <- c("r","o","y","g","b","p")
for(i in 1:720){
  check1 <- setNames(ones[[i]], colors)
  print(paste0("ones: ",i, " - ",  ones[i]))
  
  for(j in 1:720){
    check2 <- setNames(twos[[j]], colors)
    #print(paste0("twos: ", j, " - ",  twos[j]))
    cb2 <- sort(c(check1,check2))
    if(TRUE %in% c(duplicated(c(names(cb2[1:2]))),duplicated(c(names(cb2[3:4]))),duplicated(c(names(cb2[5:6]))),
                   duplicated(c(names(cb2[7:8]))),duplicated(c(names(cb2[9:10]))),duplicated(c(names(cb2[11:12]))))){
     next
      
    }
    if(TRUE %in% c(duplicated(c(names(cb2[c(5,7)]))),duplicated(c(names(cb2[c(1,3)]))),duplicated(c(names(cb2[c(2,4)]))),
                   duplicated(c(names(cb2[c(6,9)]))),duplicated(c(names(cb2[c(8,12)]))),duplicated(c(names(cb2[c(10,11)]))))){
      next
    }
    
    for(k in 1:720){
      check3 <- setNames(threes[[k]], colors)
      #print(paste0("threes: ", k, " - ",  threes[k]))
      cb3 <- sort(c(check1,check2,check3))
      if(TRUE %in% c(duplicated(c(names(cb3[1:3]))),duplicated(c(names(cb3[4:6]))),duplicated(c(names(cb3[7:9]))),
                     duplicated(c(names(cb3[10:12]))),duplicated(c(names(cb3[13:15]))),duplicated(c(names(cb3[16:18]))))){
        next
      } 
      if(TRUE %in% c(duplicated(c(names(cb3[c(7,10,13)]))),duplicated(c(names(cb3[c(2,4,11)]))),duplicated(c(names(cb3[c(3,6,18)]))),
                      duplicated(c(names(cb3[c(1,8,14)]))),duplicated(c(names(cb3[c(5,12,17)]))),duplicated(c(names(cb3[c(9,15,16)]))))){
        next
      }
      
      
      for(l in 1:720){
        check4 <- setNames(fours[[l]], colors)
        #print(paste0("fours: ", l, " - ",  fours[l]))
        cb4 <- sort(c(check1,check2,check3,check4))
        if(TRUE %in% c(duplicated(c(names(cb4[1:4]))),duplicated(c(names(cb4[5:8]))),duplicated(c(names(cb4[9:12]))),
                       duplicated(c(names(cb4[13:16]))),duplicated(c(names(cb4[17:20]))),duplicated(c(names(cb4[21:24]))))){
          next
        } 
        if(TRUE %in% c(duplicated(c(names(cb4[c(5,9,13,17)]))),duplicated(c(names(cb4[c(2,6,11,14)]))),duplicated(c(names(cb4[c(4,8,16,24)]))),
                         duplicated(c(names(cb4[c(1,10,18,21)]))),duplicated(c(names(cb4[c(7,15,20,23)]))),duplicated(c(names(cb4[c(3,12,19,22)]))))){
          next
        } 

        for(m in 1:720){
          check5 <- setNames(fives[[m]], colors)
          #print(paste0("fives: ", m, " - ", fives[m]))
          cb5 <- sort(c(check1,check2,check3, check4, check5))
          if(TRUE %in% c(duplicated(c(names(cb5[1:5]))),duplicated(c(names(cb5[6:10]))),duplicated(c(names(cb5[11:15]))),
                         duplicated(c(names(cb5[16:20]))),duplicated(c(names(cb5[21:25]))),duplicated(c(names(cb5[25:30]))))){
            next
          } 
          if(TRUE %in% c(duplicated(c(names(cb5[c(6,11,16,21,26)]))),duplicated(c(names(cb5[c(2,8,13,17,23)]))),duplicated(c(names(cb5[c(5,10,15,20,30)]))),
                           duplicated(c(names(cb5[c(1,7,12,22,27)]))),duplicated(c(names(cb5[c(4,9,19,25,29)]))),duplicated(c(names(cb5[c(3,14,18,24,28)]))))){
            next
          } 

          for(n in 1:720){
            check6 <- setNames(sixes[[n]], colors)
            #print(paste0("sixes: ", n, " - ", sixes[n]))
            checkboard <- sort(c(check1,check2,check3,check4,check5,check6))

            dups1 <- colors %in% names(checkboard[1:6])
            dups2 <- colors %in% names(checkboard[7:12])
            dups3 <- colors %in% names(checkboard[13:18])
            dups4 <- colors %in% names(checkboard[19:24])
            dups5 <- colors %in% names(checkboard[25:30])
            dups6 <- colors %in% names(checkboard[31:36])
            
            dups7 <- colors %in% names(checkboard[c(1,7,13,19,25,31)])
            dups8 <- colors %in% names(checkboard[c(2,8,14,20,26,32)])
            dups9 <- colors %in% names(checkboard[c(3,9,15,21,27,33)])
            dups10 <- colors %in% names(checkboard[c(4,10,16,22,28,34)])
            dups11 <- colors %in% names(checkboard[c(5,11,17,23,29,35)])
            dups12 <- colors %in% names(checkboard[c(6,12,18,24,30,36)])
            
           if(!(FALSE %in% c(dups1,dups2,dups3,dups4,dups5,dups6))){
             print(matrix(names(checkboard),byrow = TRUE, nrow = 6))
             if(!(FALSE %in% c(dups7,dups8,dups9,dups10,dups11,dups12))){
              print(matrix(names(checkboard),byrow = TRUE, nrow = 6))
              stop = TRUE
              break
             }
           }
           iterations <- iterations + 1
           if((iterations %% 10000) < 1){
             print(iterations)
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
  if(stop){break}
}


