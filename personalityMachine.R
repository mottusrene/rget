source("https://raw.githubusercontent.com/mottusrene/rget/master/helpers.R")

# Allocating the jobs across cores is suggested
require(future)
plan(multisession)
require(tidyverse)

## Behaviour genetic variance components (influences)

## Rows of twins 1 and twins 2
tw1 = 1:250
tw2 = 251:500

## A random genetic structure or a Big Five genetic structure
# A = matrix(nrow=2000, ncol=100, rnorm(200000))
A = rnorm(2000) %>% list %>% 
   rep(5)  %>% map(~item(., runif(1, .4, .6))) %>% 
   rep(20) %>% map(~item(., runif(1, .4, .6))) %>%
   bind_cols %>% as.matrix
A[tw2,] = A[tw1,]  
E = matrix(nrow=2000, ncol=100, rnorm(200000)) 

w1 = c(genes=1/3, environment=1/3, random=1/3, social=0)
w2 = c(genes=1/10, environment=1/10, random=1/10, social=7/10)

Sample1 %<-% personalityMachine(n=2000, cycles=150, k=100, v1=A, v2=E, weig=w1, nfriends=5)
Sample2 %<-% personalityMachine(n=2000, cycles=150, k=100, v1=A, v2=E, weig=w2, nfriends=5)

## Correlations between the lower triagonal elements of the between- and within-family correlation matrices
## A positive correlation would show that genetic and environmental influences on personality are structred similarly

Sample1$results %>% .[,dim(.)[2],] %>% comp.cor
Sample2$results %>% .[,dim(.)[2],] %>% comp.cor

## Plot how this evolves over time

Sample2$results %>% apply(2, comp.cor) %>% 
   plot(type="l", xlab="Cycles", ylab="Similarity of genetic and environmental structures" )
Sample1$results %>% apply(2, comp.cor) %>% lines(type="l", lty=3)

## Explore the extent to which genetic and environmental correlations between traits in the last cycle
## match with correlations among genetic and environmental influences agents started off with
## The overlap of genetic and environmental correlations with the phenotypic correlations in twins and others (non-twins) is estimated
## If the twin correlation structures overlap with the correlation structures in others, it shows how twins have influenced others, too

last = Sample2$results %>% .[,dim(.)[2],]

cor(tibble(
   gen.Output =  lt(cor(last[tw1,]+last[tw2,])),
   env.Output =  lt(cor(last[tw1,]-last[tw2,])),
   gen.Input = lt(cor(A[tw1,])),
   env.Input = lt(cor(E[tw1,])),
   phen.Stct.twins = lt(cor(last[c(tw1,tw2),])),
   phen.Stct.others = lt(cor(last[-c(tw1,tw2),])),
   soc.Stct.twins = lt(cor(Sample2$vsoc[c(tw1,tw2),])),
   soc.Stct.others = lt(cor(Sample2$vsoc[-c(tw1,tw2),]))
)) %>% round(2)

## Plot heritability changes over time

Sample1$results %>% apply(2, tw.cor) %>% 
   plot(type="l", xlab="Cycles", ylab="Heritability", ylim=c(0,0.8), lty=3)
Sample2$results %>% apply(2, tw.cor) %>% lines(type="l")

## Correlations of phenotypic scores with genetic and environmental inputs

cor(A, Sample1$results %>% .[,dim(.)[2],]) %>% diag %>% summary
cor(E, Sample1$results %>% .[,dim(.)[2],]) %>% diag %>% summary
cor(A, Sample2$results %>% .[,dim(.)[2],]) %>% diag %>% summary
cor(E, Sample2$results %>% .[,dim(.)[2],]) %>% diag %>% summary

