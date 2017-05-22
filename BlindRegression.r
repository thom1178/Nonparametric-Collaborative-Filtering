
beta <- 2
tmatrix <- matrix(c(2,2,NA,4,NA, 5, 2, 1, 1, 4, 1, 1,2, 2, 1, 1, 1, 1, 1, 1), nrow = 4)
print(tmatrix<- data.frame(tmatrix))
unk <- which(is.na(tmatrix[,]), arr.ind = T) #To build model
########################################
# N(u): Input row, recieve col
# N(i): Input col, recieve row
########################################
N.u <- col(tmatrix)
N.i <- row(tmatrix)
for(i in 1:length(tmatrix[,1])){
  for(j in 1:length(tmatrix[1,])){
    if(is.na(tmatrix[i,j])){
      N.u[i,j] <- NA
      N.i[i,j] <- NA
    }
  }
}#Remove Missing value indeces
########################################
# N(u,v)
########################################
N.uv <- rep(list(rep(NA, length(tmatrix[1,]))), sum(length(tmatrix[,1]):1-1) )
label <- data.frame(user1 = rep(NA, sum(length(tmatrix[,1]):1-1)), user2 =rep(NA, sum(length(tmatrix[,1]):1-1)))
k<-1
i <-1
j <-i+1
repeat{
  if(j>length(tmatrix[,1])){break}
  N.uv[[k]] <- intersect(N.u[i,], N.u[j,]);
  label[k,1]<-i
  label[k,2]<-j
  k <- k+1;
  j <- j+1;
    if(j == length(tmatrix[,1])){
     N.uv[[k]] <- intersect(N.u[i,], N.u[j,]);
     label[k,1]<-i
     label[k,2]<-j
     i <- i+1
     j <- i+1
     k <- k+1
    }
  
  if(i ==length(tmatrix[,1]-1)){
    N.uv[[k]] <- intersect(N.u[i,], N.u[j,]);
    label[k,1]<-i
    label[k,2]<-j
    break
  }
}#Determine the intersesctions u,v

########################################
# N(i,j)
########################################
N.ij <- rep(list(rep(NA, length(tmatrix[,1]))), sum(length(tmatrix[1,]):1-1) )
label1 <- data.frame(user1 = rep(NA, sum(length(tmatrix[1,]):1-1)), 
                     user2 =rep(NA, sum(length(tmatrix[1,]):1-1)))
k<-1
i <-1
j <-i+1
repeat{
  if(j>length(tmatrix[1,])){break}
  N.ij[[k]] <- intersect(N.i[,i], N.i[,j]);
  label1[k,1]<-i
  label1[k,2]<-j
  k <- k+1;
  j <- j+1;
  if(j == length(tmatrix[1,])){
    N.ij[[k]] <- intersect(N.i[,i], N.i[,j]);
    label1[k,1]<-i
    label1[k,2]<-j
    i <- i+1
    j <- i+1
    k <- k+1
  }
  
  if(i ==length(tmatrix[1,]-1)){
    N.ij[[k]] <- intersect(N.i[,i], N.i[,j]);
    label1[k,1]<-i
    label1[k,2]<-j
    break
  }
}#Determine the intersesctions u,v

########################################
# S(i)
# S(u)
########################################

S.i<-list(rep(NA, length(tmatrix[1,])))
for(u in 1:length(unk[,1])){
  index <- c(which(label$user1 == unk[u,1]), 
        which(label$user2 == unk[u,1]))
  for(i in length(index)){
    if(length(N.uv[[index[i]]]) - sum(is.na(N.uv[[index[i]]])) >= beta){
      S.i[[u]] <- N.i[,unk[u,2]];
    } #size of N.uv
  }
}

S.u<-list(rep(NA, length(tmatrix[,1])))
for(i in 1:length(unk[,1])){
  index <- c(which(label1$user1 == unk[i,1]), 
             which(label1$user2 == unk[i,1]))
  for(u in length(index)){
    if(length(N.ij[[index[u]]]) - sum(is.na(N.ij[[index[u]]])) >= beta){
      S.u[[i]] <- N.u[unk[i,1],];
    } #size of N.ij
  }
}

########################################
# Empirical Mean and variances
########################################
m.uv <-matrix(rep(NA, length(unk[,1])*(length(tmatrix[,1])-1) ), nrow = length(unk[,1]))
for(u in 1:length(unk[,1])){
  S.i[[u]] <- S.i[[u]][!is.na(S.i[[u]])]
  for(v in 1:length(S.i[[u]])){
  index.uv <- which((label$user1 == unk[u,1] | label$user2 == unk[u,1]) & 
                      (label$user1 == S.i[[u]][v] | label$user2 == S.i[[u]][v])); 
  N.uv[[index.uv]]<-N.uv[[index.uv]][!is.na(N.uv[[index.uv]])]
  m.uv.temp <- rep(NA, length(N.uv[[index.uv]]) )
    weight <- (1/(length(N.uv[[index.uv]])));
     for(j in 1:length(N.uv[[index.uv]])) {
       m.uv.temp[j] <- (tmatrix[unk[u,1], N.uv[[index.uv]][j]] - 
                           tmatrix[S.i[[u]][v], N.uv[[index.uv]][j]]) 
     }
    m.uv[u,v] <- weight*sum(m.uv.temp)
  }
} #Mean uv

s.uv <-matrix(rep(NA, length(unk[,1])*(length(tmatrix[,1])-1) ), nrow = length(unk[,1]))
for(u in 1:length(unk[,1])){
  S.i[[u]] <- S.i[[u]][!is.na(S.i[[u]])]
  for(v in 1:length(S.i[[u]])){
    index.uv <- which((label$user1 == unk[u,1] | label$user2 == unk[u,1]) & 
                        (label$user1 == S.i[[u]][v] | label$user2 == S.i[[u]][v])); 
    N.uv[[index.uv]]<-N.uv[[index.uv]][!is.na(N.uv[[index.uv]])]
    s.uv.temp <- rep(NA, length(N.uv[[index.uv]]) )
    weight <- (1/(length(N.uv[[index.uv]]) - 1));
    for(j in 1:length(N.uv[[index.uv]])) {
      s.uv.temp[j] <- (tmatrix[unk[u,1], N.uv[[index.uv]][j]] - 
                         tmatrix[S.i[[u]][v], N.uv[[index.uv]][j]] - m.uv[u,v])^2 
    }
    s.uv[u,v] <- weight*sum(s.uv.temp)
  }
} #Variance uv


m.ij <-matrix(rep(NA, length(unk[1,])*(length(tmatrix[1,])-1) ), nrow = length(unk[,1]))
for(u in 1:length(unk[,1])){
  S.u[[u]] <- S.u[[u]][!is.na(S.u[[u]])]
  for(v in 1:length(S.u[[u]])){
    index.ij <- which((label1$user1 == unk[u,2] | label1$user2 == unk[u,2]) & 
                        (label1$user1 == S.u[[u]][v] | label1$user2 == S.u[[u]][v])); 
    N.ij[[index.ij]]<-N.ij[[index.ij]][!is.na(N.ij[[index.ij]])]
    m.ij.temp <- rep(NA, length(N.ij[[index.ij]]) )
    weight <- (1/(length(N.ij[[index.ij]])));
    for(j in 1:length(N.ij[[index.ij]])) {
      m.ij.temp[j] <- (tmatrix[N.ij[[index.ij]][j], unk[u,2]] - 
                         tmatrix[N.ij[[index.ij]][j], S.u[[u]][v],]) 
    }
    m.ij[u,v] <- weight*sum(m.ij.temp)
  }
} #Mean ij

s.ij <-matrix(rep(NA, length(unk[1,])*(length(tmatrix[1,])-1) ), nrow = length(unk[,1]))
for(u in 1:length(unk[,1])){
  S.u[[u]] <- S.u[[u]][!is.na(S.u[[u]])]
  for(v in 1:length(S.u[[u]])){
    index.ij <- which((label1$user1 == unk[u,2] | label1$user2 == unk[u,2]) & 
                        (label1$user1 == S.u[[u]][v] | label1$user2 == S.u[[u]][v])); 
    N.ij[[index.ij]]<-N.ij[[index.ij]][!is.na(N.ij[[index.ij]])]
    s.ij.temp <- rep(NA, length(N.ij[[index.ij]]) )
    weight <- (1/(length(N.ij[[index.ij]]) - 1));
    for(j in 1:length(N.ij[[index.ij]])) {
      s.ij.temp[j] <- (tmatrix[N.ij[[index.ij]][j], unk[u,2]] - 
                         tmatrix[N.ij[[index.ij]][j], S.u[[u]][v],] - m.ij[u,v])^2 
    }
    s.ij[u,v] <- weight*sum(s.ij.temp) #18????
  }
} #Mean ij
#Prediction
B.beta <- list(matrix(rep(NA,dim(tmatrix)[1]*dim(tmatrix)[2]), ncol = 2))
temp <- list(matrix(rep(NA,dim(tmatrix)[1]*dim(tmatrix)[2]), ncol = 2))
temp2 <- list(matrix(rep(NA,dim(tmatrix)[1]*dim(tmatrix)[2]), ncol = 2))
remove <- list(NA)
for(i in 1:length(unk[,1])){
  B.beta[[i]] <- expand.grid(S.i[[i]], S.u[[i]])
  for(j in 1:length(unk[,1])){
  temp[[i]]<- B.beta[[i]][,2] == unk[j,2]
  temp2[[i]]<- B.beta[[i]][,1] == unk[j,1]
  if(length(intersect(which(temp[[i]]), 
                      which(temp2[[i]])))>0){
  remove[[i]] <- intersect(which(temp[[i]]), 
                          which(temp2[[i]]))
  B.beta[[i]] <- B.beta[[i]][-remove[[i]][1],]
  }
  }
}
y.hat <- rep(NA, length(unk[,1]))
for(u in 1:length(unk[,1])){
  for(i in length(B.beta[[u]][,1])){
    y.hat[u] <- (tmatrix[unk[u,1], B.beta[[u]][i,2]] +
                        tmatrix[B.beta[[u]][i,1], unk[u,2]] - tmatrix[B.beta[[u]][i,1], B.beta[[u]][i,2]]  )
    y.hat[u] <- y.hat[u] / length(B.beta[[u]])
  }
}
y.hat #Prediction

