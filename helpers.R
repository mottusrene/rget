require(MASS)

## The main function that updates personalities

personalityMachine = function(n, k, cycles, v1, v2, weig, nfriends, conv=0.9, rget=T){  
  ## Setup initials
  results = array(dim=c(n,cycles+1,k))
  results[,1,] = rnorm(k*n)
  vsoc = matrix(nrow=n,ncol=k,rnorm(n*k)) 
  ## Loop through cycles and agents and update trait scores
  for(c in 1:cycles){
    for(p in 1:n){  
      v = cbind(v1[p,], v2[p,], rnorm(k), vsoc[p,]) %*% as.numeric(sqrt(weig[c,]))
      w = proj.obl(v, v-results[p,c,], r=conv)   
      results[p,c+1,] = w %*% results[p,c,]
      others = results[-p,c+1,] 
      others[is.na(others)] = results[-p,c,][is.na(others)]
      if(rget) 
        vsoc[p,] = rge(results[p,c+1,], others, n = nfriends) 
      else
        vsoc[p,] = rge(v1[p,], others, n = nfriends) 
    }
  }  
  return(list(results = results, vsoc = vsoc)) 
}

## This function is responsible for obilque projection of person vector (represented as A) towards its target vector (B)
## The eigenvalue substitution processes is responsible for projection (or convergence) being gradual

proj.obl = function(A, B, r=.9) {
  B = Null(B)
  P = A %*% ginv(t(B)%*%A) %*% t(B)  
  e = eigen(P)
  e$values = c(1, rep(r,nrow(A)-1))  
  Re((e$vectors) %*% diag(e$values) %*% solve(e$vectors))
}

## Social interactions with n friends

rge = function(self, others, n){
  influence = order(  apply(others, 1, function(x) mean((x-self))^2), decreasing = F)
  closest = t(others[influence,])[,1:n]
  as.numeric(closest %*% rep(1,n)/n)
}

## This function pulls out lower diagonal of a matrix

lt = function(x) x[lower.tri(x)]




