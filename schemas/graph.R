library(ggplot2)
library(grid)
library(Cairo)
setwd("~/GitHub/mcauliffe-phd/schemas")
votSeq <- seq(-10,21,by=1)
pmean <- 11
bmean <- 1
SD <- 3
classify <- expand.grid(VOT=votSeq,Exposure=c("Norm","Mod"))
classify$Resp <- 0
classify$Resp[classify$Exposure=="Norm"] <- 100/ (1+exp(((classify$VOT[classify$Exposure=="Norm"]-bmean)^2-(classify$VOT[classify$Exposure=="Norm"]-pmean)^2)/(2*SD^2)))
classify$Resp[classify$Exposure=="Mod"] <- 100/ (1+exp(((classify$VOT[classify$Exposure=="Mod"]-(bmean+2))^2-(classify$VOT[classify$Exposure=="Mod"]-pmean)^2)/(2*(SD)^2)))

ggplot(classify,aes(x=VOT,y=Resp,linetype=Exposure))+geom_line(lwd=1.2) + ylab("Percent /s/ response") + xlab("Continuum step")+ scale_y_continuous(limits=c(0,100)) + scale_x_continuous(limits =c(-1,13),breaks=1:11) + scale_linetype_discrete(labels=c("Normal /s/","Modified /s/")) + theme(text=element_text(size=12))
ggsave('../thesis/graphs/class.pdf',width=160,height=50,units='mm',dpi=600)

dists <- expand.grid(VOT=votSeq,Category=c("/s/","/sh/"))
distMod <- expand.grid(VOT=votSeq,Category=c("/s/"))
distMod$Dens <-dnorm(distMod$VOT,mean=bmean+2,sd=SD)

dists$Dens <- 0
dists$Dens[dists$Category=="/s/"] <-dnorm(dists$VOT[dists$Category=="/s/"],mean=bmean,sd=SD)
dists$Dens[dists$Category=="/sh/"] <-dnorm(dists$VOT[dists$Category=="/sh/"],mean=pmean,sd=SD)

ggplot(dists,aes(x=VOT,y=Dens,colour=Category)) + geom_line(lwd=1.5)+geom_line(data=distMod,linetype=2,lwd=1.5) + xlab("Continuum step") + ylab("Density") + theme(text=element_text(size=12),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank())+ scale_colour_hue(l=45)+ scale_x_continuous(limits =c(-5,16),breaks=1:11) + scale_y_continuous(breaks=c())
ggsave('../thesis/graphs/dist.pdf',width=160,height=50,units='mm',dpi=600)

dists <- expand.grid(VOT=votSeq,Category=c("/s/","/sh/"))
distMod <- expand.grid(VOT=votSeq,Category=c("/s/","/sh/"))
distMod$Dens <- 0
distMod$Dens[distMod$Category=="/s/"] <-dnorm(distMod$VOT[distMod$Category=="/s/"],mean=bmean,sd=SD-0.75)
distMod$Dens[distMod$Category=="/sh/"] <-dnorm(distMod$VOT[distMod$Category=="/sh/"],mean=pmean,sd=SD-0.75)

dists$Dens <- 0
dists$Dens[dists$Category=="/s/"] <-dnorm(dists$VOT[dists$Category=="/s/"],mean=bmean,sd=SD)
dists$Dens[dists$Category=="/sh/"] <-dnorm(dists$VOT[dists$Category=="/sh/"],mean=pmean,sd=SD)

ggplot(dists,aes(x=VOT,y=Dens,colour=Category)) + geom_line(lwd=1.5)+geom_line(data=distMod,linetype=2,lwd=1.5) + xlab("Continuum step") + ylab("Density") + theme(text=element_text(size=12),axis.title.y = element_blank(),axis.text.y = element_blank())+ scale_colour_hue(l=45)+ scale_x_continuous(limits =c(-5,16),breaks=1:11) + scale_y_continuous(breaks=c())
ggsave('../thesis/graphs/distAttention.pdf',width=160,height=50,units='mm',dpi=600)

classify <- expand.grid(VOT=votSeq)
classify$Resp <- 0
classify$Resp <- 100/ (1+exp(((classify$VOT-bmean)^2-(classify$VOT-pmean)^2)/(2*SD^2)))
theme_set(theme_bw())
classplot <- ggplot(classify,aes(x=VOT,y=Resp))+geom_line(lwd=1.2) + ylab("Percent identified as voiced stop") + xlab("VOT (ms)")+ scale_y_continuous(limits=c(0,100)) + opts(title="Classification function of VOT continuum\nbetween voiced and voiceless stop",plot.title = theme_text(size=40, face="bold"),axis.text.x=theme_text(size=24),axis.text.y=theme_text(size=24),axis.title.x=theme_text(size=36,vjust=-3),axis.title.y=theme_text(size=36,angle=90,vjust=-0.01),plot.margin = unit(c(0, 0, 1, 1), "cm")) + geom_vline(xintercept=mean(classify[classify$Resp<81&classify$Resp>79,]$VOT),linetype=2,lwd=1.2,colour="red") + geom_vline(xintercept=mean(classify[classify$Resp<21&classify$Resp>19,]$VOT),linetype=2,lwd=1.2,colour="red") + xlim(c(10,60))
print(classplot)
ggsave(filename=paste(graphdir,"ClassRange.pdf",sep=""),plot=classplot)

vots <- seq(25,75,length=50)

trials <- expand.grid(Trial=seq(1,50,by=1),Group=c("Random","Upshift","Stable"))
trials$VOT <- 0
trials$VOT[trials$Group=="Random"] <- sample(vots,50,replace=T)
trials$VOT[trials$Group=="Upshift"] <- vots
trials$VOT[trials$Group=="Stable"] <- 25

trialplot <- ggplot(trials,aes(x=Trial,y=VOT,colour=Group))+geom_line(lwd=1.2) + xlab("Trial number") + ylab("Percent identified as voiced stop") + ylim(c(0,100)) + scale_y_continuous(limits=c(0,100)) + opts(title="Group differences in stimuli presentation\n",plot.title = theme_text(size=40, face="bold"),axis.text.x=theme_text(size=24),axis.text.y=theme_text(size=24),axis.title.x=theme_text(size=36,vjust=-3),axis.title.y=theme_text(size=36,angle=90,vjust=-0.01),legend.text=theme_text(size=28,lineheight=2),legend.title=theme_text(size=30,face="bold"),legend.key.size=unit(2,"cm"),plot.margin = unit(c(0, 0, 1, 1), "cm"))+ scale_colour_hue(l=45,guide=guide_legend(override.aes=list(size=5)))
print(trialplot)
ggsave(filename=paste(graphdir,"Trial.pdf",sep=""),plot=trialplot)



dists <- expand.grid(VOT=votSeq,Category=c("/s/","/ʃ/"),Type=c('Isolation','Predictable'))
dists$Dens <- 0
dists[dists$Category=="/s/" & dists$Type == 'Isolation',]$Dens <-dnorm(dists[dists$Category=="/s/"& dists$Type == 'Isolation',]$VOT,mean=bmean,sd=SD)
dists[dists$Category=="/ʃ/" & dists$Type == 'Isolation',]$Dens <-dnorm(dists[dists$Category=="/ʃ/"& dists$Type == 'Isolation',]$VOT,mean=pmean,sd=SD)
dists[dists$Category=="/s/" & dists$Type == 'Predictable',]$Dens <-dnorm(dists[dists$Category=="/s/" & dists$Type == 'Predictable',]$VOT,mean=bmean,sd=SD+2)
dists[dists$Category=="/ʃ/" & dists$Type == 'Predictable',]$Dens <-dnorm(dists[dists$Category=="/ʃ/" & dists$Type == 'Predictable',]$VOT,mean=pmean,sd=SD+2)

points <- data.frame(x=rep(0,40),y=rep(0,40),Experiment = 'Experiment 1')
points$Experiment <- as.character(points$Experiment)
points[1:20,]$x <- rnorm(20,5,1)
points[21:40,]$x <- rnorm(20,8,1)
points[21:40,]$Experiment <- 'Experiment 2'

CairoPDF('../thesis/graphs/distPred.pdf',width=6.69,height=3.15)
ggplot(dists,aes(x=VOT,y=Dens, colour=Category)) + geom_line(lwd=1.5) + xlab("Continuum step") + ylab("Density") +theme_bw() + theme(text=element_text(size=12),axis.title.y = element_blank(),axis.text.y = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank()) + scale_y_continuous(breaks=c())+geom_point(data=points,aes(x=x,y=y, shape = Experiment), colour='black',position=position_jitter(width=0.005,height=0.002)) + facet_grid(~Type) + geom_vline(x=5) + geom_vline(x=8,linetype=2) + scale_x_continuous(limits=c(-3,15), breaks =c()) + geom_hline(y=0) + scale_colour_discrete(labels = c('/s/', '/ʃ/')) + scale_shape_discrete(name='Modified /s/\ncategory')
dev.off()
ggsave('../thesis/graphs/distAttention.pdf',width=160,height=50,units='mm',dpi=600)
