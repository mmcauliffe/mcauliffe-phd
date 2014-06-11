library(pwr)

cohen.ES(test=c('f2'),size=c("small"))
small = 0.02
cohen.ES(test=c('f2'),size=c("medium"))
med = 0.15
cohen.ES(test=c('f2'),size=c("large"))
large = 0.35

u = 1 + (2-1) + (2-1) + (2*2)
  
#2 Categorical
#Position - S-Initial vs S-Final
#Attention - Attention vs Not attention

#1 Continuous
#Performance on Lexical decision
pwr.f2.test(u = u, v = NULL, f2 = .02, sig.level = 0.05,power = 0.8)
v = 717 #small, N = 725
v = 95 #medium, N = 103
v = 40 #large, N = 48

u + v + 1
