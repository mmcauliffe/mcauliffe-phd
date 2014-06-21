
library(lme4.0)
library(ggplot2)
library(plyr)

cat <- read.delim('C:\\Users\\michael\\Dropbox\\Michael_Dissertation\\Pretest\\sshcat_results.txt')

cat$StepNum <- as.numeric(gsub("[^0-9]","",cat$step))


exp <- subset(cat,which=='one')
test <- subset(cat,which=='both')


ddply(exp, ~ sword * factor(StepNum),summarise,mean(ACC))
ddply(test, ~ sword * factor(StepNum),summarise,mean(ACC))

ggplot(exp, aes(x=StepNum, y=ACC,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Exposure words', y='Proportion <S> responses',x='Step number')+ geom_vline(xintercept = 6) + geom_hline(yintercept=0.5)
ggplot(test, aes(x=StepNum, y=ACC,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Categorization words', y='Proportion <S> responses',x='Step number')+ geom_vline(xintercept = 6)+ geom_vline(xintercept = 6) + geom_hline(yintercept=0.5)
