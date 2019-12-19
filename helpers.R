require(MASS)

## A function to get an item from a trait (t) with a loading (l)

item = function(t, l) t * l + sqrt(1-l^2)*rnorm(length(t))

## The main function that updates personalities

personalityMachine = function(n, k, cycles, v1, v2, weig, nfriends, conv=0.9, ntraits = k){  
  ## Setup initials
  results = array(dim=c(n,cycles,k),rnorm(n*cycles*k))
  vsoc = matrix(nrow=n, rnorm(n*k)) 
  ## Loop through cycles and agents and update trait scores
  for(c in 2:cycles){
    for(p in 1:n){  
      v = cbind(v1[p,], v2[p,], rnorm(k), vsoc[p,]) %*% as.numeric(sqrt(weig))
      w = proj.obl(v, v-results[p,c-1,], r=conv)   
      results[p,c,] = w %*% results[p,c-1,]
      vsoc[p,] = rge(results[p,c,], results[-p,c-1,], n = nfriends, top.traits = ntraits) 
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

## Social interactions with n friends and based on the top.traits most salient traits

rge = function(self, others, n, top.traits){
  tt = order(abs(self), decreasing=T) <= top.traits
  #tt  = order(abs(self), decreasing=T) / (sum(1:length(self)))
  influence = order(  apply(others[,tt], 1, function(x) mean((x-self[tt]))^2), decreasing = F)
  closest = t(others[influence,])[,1:n]
  as.numeric(closest %*% rep(1,n)/n)
}

## This function pulls out lower diagonal of a matrix

lt = function(x) x[lower.tri(x)]

## This functions calculates the similarity of between- and within-family correlation structures
## This means comparing correlations structures of a) mz pair means and b) within mz pair differences

comp.cor = function(x, i1 = tw1, i2 = tw2) cor(lt(cor(x[i1,] + x[i2,])), lt(cor(x[i1,] - x[i2,])))

## This functions calculates mean heritability across many traits (as the mean of mz twin correlations)

tw.cor = function(x, i1 = tw1, i2 = tw2) mean(diag(cor(x[i1,], x[i2,])))
