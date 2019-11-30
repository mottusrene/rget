source("helperFunctions.R")

# Allocating the jobs across cores is suggested
require(future)
plan(multisession)

## Behaviour genetic variance components (influences)

## Rows of twins 1 and twins 2 in all dataframes
tw1 = 1:250
tw2 = 251:500

A = matrix(nrow=2000, ncol=100, rnorm(200000)) 
A[tw2,] = A[tw1,]  
# for(i in 1:1000) A[i,] = mean(A[i,])
# A[,1:50] = A[sample(2000),1:50]
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

plot(apply(Sample1, 2, function(x) mean(diag(cor(x[tw1,], x[tw2,])))), type="l", xlab="Cycles", ylab="Heritability", ylim=c(0,0.6), lty=3)
lines(apply(Sample2, 2, function(x) mean(diag(cor(x[tw1,], x[tw2,])))), type="l")



## Change across all cycles

# id = rep(tw1, times=2) ## id variable for twin pairs
slices = seq(round(0.1*dim(Sample2$results)[2]), round(0.9*dim(Sample2$results)[2]), 10) ## which cycles to take data from

# change1 = change2 = array(dim=c(length(c(tw1,tw2)), ncol(A), length(slices) ))
# count = 0
# for(h in slices) {
#   count = count + 1 
#   for(i in 1:100) {
#     change1[,i,count] = residuals(lme4::lmer(Sample1[c(tw1,tw2),dim(Sample1)[2],i] ~ Sample1[c(tw1,tw2),h,][,i] + (1|id)))
#     change2[,i,count] = residuals(lme4::lmer(Sample2[c(tw1,tw2),dim(Sample2)[2],i] ~ Sample2[c(tw1,tw2),h,][,i] + (1|id)))
#   }
# }

change1 = change2 = array(dim=c(length(c(tw1,tw2)), ncol(A), length(slices) ))
count = 0
for(h in slices) {
  count = count + 1 
    change1[,,count] = Sample1$results[c(tw1,tw2),dim(Sample1$results)[2],] - Sample1$results[c(tw1,tw2),h,] 
    change2[,,count] = Sample2$results[c(tw1,tw2),dim(Sample2$results)[2],] - Sample2$results[c(tw1,tw2),h,]
}

aver.cor1 = cor(Sample1$results[tw1,dim(Sample1$results)[2],] + Sample1$results[tw2,dim(Sample1$results)[2],])
dif.cor1 = cor(Sample1$results[tw1,dim(Sample1$results)[2],] - Sample1$results[tw2,dim(Sample1$results)[2],])
aver.cor2 = cor(Sample2$results[tw1,dim(Sample2$results)[2],] + Sample2$results[tw2,dim(Sample2$results)[2],])
dif.cor2 = cor(Sample2$results[tw1,dim(Sample2$results)[2],] - Sample2$results[tw2,dim(Sample2$results)[2],])

plot(apply(change1, 3, function(x) cor(lt(cor(x)), lt(dif.cor1))), xlab="Cycles", ylab="Change - environmental structure overlap", type="l", lty=3, ylim=c(0,1), axes=F)
axis(2)
lines(apply(change2, 3, function(x) cor(lt(cor(x)), lt(dif.cor2))), type="l")
plot(apply(change1, 3, function(x) cor(lt(cor(x)), lt(aver.cor1))), xlab="Cycles", ylab="Change - genetic structure overlap", type = "l", lty=3, ylim=c(0,1), axes = F )
axis(2)
lines(apply(change2, 3, function(x) cor(lt(cor(x)), lt(aver.cor2))), type="l")



#save.image("tmp.RData")
