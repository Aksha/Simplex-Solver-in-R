Pivot <- function(M, i, j) {
  n = nrow(M) 
  M[i,] <- M[i,]/M[i,j]  # pivot term coefficient is set to 1
  for(r in 1:n) {    
    if (r != i) {  
      M[r,] <- M[r,] - (M[i,] * M[r,j]) # For all other rows, the coefficient of corresponding column is brought down to 0
    }
  }  
  return(M)
}


Simplex <- function(S){
  vector <- 1:ncol(S)
  vector[1:ncol(S)] <- 1
  vector[S[1,] == 0] <- 0
  vector[ncol(S)] <- 0
  
  while(TRUE){  
    
    if(length(S[S[1,vector==1]<0])>0){
      columnindex <- c(1:ncol(S))[S[1,] == min(S[1,vector==1]) & vector==1][1] #columnindex
      
      if(length(S[S[,columnindex]>0,ncol(S)]<0)>0){ #till non negative
        
        ratio <- S[S[,columnindex]>0,ncol(S)]/S[S[,columnindex]>0,columnindex] #ratio
        ratioindex <- c(1:(length(ratio)))[ratio == min(ratio)][1] #ratio index
        ratiob <- S[S[,columnindex]>0,columnindex][ratioindex] #ratio at bottom level
        ratiot <- S[S[,columnindex]>0,ncol(S)][ratioindex] #ratio at top level
        rowindex <- c(1:nrow(S))[S[,columnindex] == ratiob & S[,ncol(S)] == ratiot][1] #row index, which row to swap
        
        S <- pivot(S,rowindex,columnindex)
        
        vector[columnindex] <- 0
        
      } 
      else{
        S[1,ncol(S)] <- Inf
        break
      }
    } 
    else{
      break
    }
  }
  return(S[1,ncol(S)])
}