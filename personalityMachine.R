source("https://github.com/mottusrene/rget/blob/master/helpers.R")

# Allocating the jobs across cores is suggested
require(future)
plan(multisession)

## Behaviour genetic variance components (influences)

## Rows of twins 1 and twins 2 in all dataframes
tw1 = 1:250
tw2 = 251:500

## A random genetic structure or a Big Five genetic structure
# A = matrix(nrow=2000, ncol=100, rnorm(200000))
A = rnorm(2000) %>% list %>% 
   rep(5)  %>% map(~facs(., runif(1, .4, .6))) %>% 
   rep(20) %>% map(~facs(., runif(1, .4, .6))) %>%
   bind_cols %>% as.matrix
A[tw2,] = A[tw1,]  
E = matrix(nrow=2000, ncol=100, rnorm(200000)) 

weights1 = data.frame(genes=rep(1/3, 250), environment=1/3, random=1/3, social=0)
weights2 = data.frame(genes=rep(1/10, 500), environment=1/10, random=1/10, social=7/10)

Sample1 %<-% personalityMachine(n=2000, cycles=250, k=100, v1=A, v2=E, weig=weights1, nfriends=5, rget = F)
Sample2 %<-% personalityMachine(n=2000, cycles=250, k=100, v1=A, v2=E, weig=weights2, nfriends=5, rget = T)

## Inspect the results in the final cycle

lastCycle = Sample2$results[,dim(Sample1$results)[2],]

## Correlations among traits based on twin pair means and averages, respectively

aver.cor = cor(lastCycle[tw1,] + lastCycle[tw2,])
dif.cor = cor(lastCycle[tw1,] - lastCycle[tw2,])

## Correlations between the lower triagonal elements of the two matrices above
## A positive correlation would show that genetic and environmental influences on personality are structred similarly

cor.test(lt(dif.cor), lt(aver.cor))

## Plot how this evolves over time

plot(apply(Sample2$results, 2, function(x) cor(lt(cor(x[tw1,] + x[tw2,])), lt(cor(x[tw1,] - x[tw2,])))), type="l", xlab="Cycles", ylab="Similarity of genetic and environmental structures" )
lines(apply(Sample1$results, 2, function(x) cor(lt(cor(x[tw1,] + x[tw2,])), lt(cor(x[tw1,] - x[tw2,])))), type="l", lty=3 )

## Explore the extent to which genetic and environmental correlations between traits in the last cycle
## match with correlations among genetic and environmental influences agents started off with
## The overlap of genetic and environmental correlations with the phenotypic correlations in twins and others (non-twins) is estimated
## If the twin correlation structures overlap with the correlation structures in others, it shows how twins have influenced others, too

cor( cbind(
   gen.Output =  lt(aver.cor),
   env.Output =  lt(dif.cor),
   gen.Input = lt(cor(A[tw1,])),
   env.Input = lt(cor(E[tw1,])),
   phen.Stct.twins = lt(cor(lastCycle[c(tw1,tw2),])),
   phen.Stct.others = lt(cor(lastCycle[-c(tw1,tw2),])),
   soc.Stct.twins = lt(cor(Sample2$vsoc[c(tw1,tw2),])),
   soc.Stct.others = lt(cor(Sample2$vsoc[-c(tw1,tw2),]))
))

## Plot heritability changes over time

plot(apply(Sample1$results, 2, function(x) mean(diag(cor(x[tw1,], x[tw2,])))), type="l", xlab="Cycles", ylab="Heritability", ylim=c(0,0.6), lty=3)
lines(apply(Sample2$results, 2, function(x) mean(diag(cor(x[tw1,], x[tw2,])))), type="l")


