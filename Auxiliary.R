



# function to permute (rename the columns and rows of) a matrix.

permuteMatrix <- function(mat){
  mat_out <- mat
  perm    <- sample(1:(ncol(mat)),ncol(mat),replace=FALSE) 
  for(i in 1:(ncol(mat))){
    for(j in 1:(ncol(mat))){
      mat_out[i,j] = mat[perm[i],perm[j]] 
      }
  } 
  mat_out
}

# function to permute the rows a matrix.

permuteMatrixRows <- function(mat){ 
  mat_out <- mat
  perm <- sample(1:(ncol(mat)),ncol(mat),replace=FALSE) 
  mat[perm,]
}

# function to sample a preference matrix uniformly at random from all n times n preference matrices with entries in [p_low,p_up].

genRandomPrefMatrix <- function(n, p_low=0.501, p_up = 1){
  
  P <- matrix(runif(n*n,p_low,p_up), ncol=n)
  P[1,1] = 0.5
  for(i in 2:n){
    for(j in 1:(i-1)){
      P[i,j] = 1 - P[j,i]
      perm = sample(c(0,1),1,prob=c(0.5,0.5))
      if(perm==1){
        P[i,j] = P[j,i]
        P[j,i] = 1 - P[i,j]
      }
    }
    P[i,i] = 0.5
  }
  permuteMatrix(P)
}

# function to sample a preference matrix with existing Condorcet winner uniformly at random from all n times n preference matrices with existing Condorcet winne and entries in [p_low,p_up].

genRandomCondWinPrefMatrix <- function(n, p_low=0.501, p_up = 1){
  
  P    <- genRandomPrefMatrix(n,p_low,p_up)
  CW   <- sample(1:n,1,replace=FALSE)
  print(CW)
  for(i in 1:n){
    for(j in 1:n){      
      if(i==CW && i!=j){
        P[i,j] = runif(1,p_low, p_up)
        P[j,i] = 1 - P[i,j]
      }
    }
    P[i,i] = 0.5
  }
  return (P)
}


# function to sample a preference matrix uniformly at random from all n times n transitive preference matrices with entries in [p_low,p_up].

genRandomTransitivePrefMatrix <- function(n, p_low=0.501, p_up = 1){
  
  perm <- sample(1:n,n,replace=FALSE) 
  temp <- matrix(runif(n*n,p_low,p_up), ncol=n)
  P <- temp
  temp[1,1] = 0.5
  for(i in 1:(n-1)){
    for(j in i:n){
      temp[j,i] = 1 - temp[i,j] 
    }
    temp[i,i] = 0.5
  }
  
  for(i in 1:n){
    for(j in 1:n){
      P[i,j] = temp[perm[i],perm[j]]
    } 
  }
 
  P
}
