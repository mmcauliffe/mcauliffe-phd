library(ggplot2)
library(stringr)
library(Cairo)
distdata <- read.delim('similarity_analysis//output.txt')

distdata$Sound <- 's'
distdata[str_detect(distdata$Type,'^SH'),]$Sound <- 'ʃ'

distdata$Sound <- factor(distdata$Sound)

distdata$Position <- 'Initial'
distdata[str_detect(distdata$Type,'Final'),]$Position <- 'Final'

distdata$Position <- factor(distdata$Position)

distdata$ExposureType <- 'Isolation'
distdata[str_detect(distdata$Type,'U$'),]$ExposureType <- 'Unpredictive'
distdata[str_detect(distdata$Type,'P$'),]$ExposureType <- 'Predictive'

distdata$ExposureType <- factor(distdata$ExposureType)

summary(distdata)


plotData <- subset(distdata,Experiment %in% c('original','exp1','exp2') & ExposureType == 'Isolation' & ((Sound == 's' & !Word %in% c('shack', 'shin','shock', 'shy') ) | (Sound == 'ʃ' & Experiment == 'original')))

plotData$Experiment = factor(plotData$Experiment, levels = c('original','exp2','exp1'))

text = data.frame(x=c(-35, -5, 7, 30),y=c(0.055,0.067,0.04,0.065),text = c('/s/','Exp 1', 'Exp 2', '/ʃ/'), Experiment = c('original', 'exp2','exp1','original'), Sound = c('/s/', '/s/', '/s/','/ʃ/' ))

cairo_pdf('../thesis/graphs/salience.pdf', width = 6.69, height = 3.15)
ggplot(plotData,aes(x=X, colour=Experiment, group = interaction(Experiment,Sound))) + geom_density(size = 2, show_guide=F)  + theme_bw()+ theme(text=element_text(size=8),legend.title=element_text(size=7),legend.text=element_text(size=6),axis.title.x = element_blank(),axis.title.y = element_blank())  + geom_text(data = text, aes(x=x,y=y, label = text), size =6, show_guide = F) + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) 
dev.off()
plotData <- subset(distdata,Experiment %in% c('categ','original','exp1') & ExposureType == 'Isolation')

CairoPDF('../thesis/graphs/exp2_mds.pdf',width=6.69,height=4.33)
ggplot(plotData,aes(x=X, y=Y, colour=Experiment, label=Sound, size = Position)) + geom_text() + scale_size_manual(name='Exposure Type',values = c(4,8), labels = c('Word-medial','Word-initial')) + theme_bw() + scale_colour_discrete(labels=c('Categorization', 'Exposure', 'Original')) + xlab('First principal component') + ylab('Second principal component')+ theme(text=element_text(size=8),legend.title=element_text(size=7),legend.text=element_text(size=6))
dev.off()

plotData <- subset(distdata,Experiment %in% c('categ','original', 'exp2') & ExposureType == 'Isolation')
plotData <- rbind(plotData,distdata[distdata$Type %in% c('SH-Final', 'SH-Initial') & distdata$Experiment == 'exp1',])
plotData[plotData$Experiment == 'exp1',]$Experiment <- 'exp2'

CairoPDF('../thesis/graphs/exp1_mds.pdf',width=6.69,height=4.33)
ggplot(plotData,aes(x=X, y=Y, colour=Experiment, label=Sound, size = Position)) + geom_text() + scale_size_manual(name='Exposure Type',values = c(4,8), labels = c('Word-medial','Word-initial'))+ theme_bw() + scale_colour_discrete(labels=c('Categorization', 'Exposure', 'Original')) + xlab('First principal component') + ylab('Second principal component')+ theme(text=element_text(size=8),legend.title=element_text(size=7),legend.text=element_text(size=6))
dev.off()

plotData <- subset(distdata,Experiment %in% c('categ','original','exp3') & ExposureType %in% c('Predictive','Unpredictive'))
plotData$ExposureType <- factor(plotData$ExposureType, levels = c('Isolation','Unpredictive','Predictive'))
plotData <- rbind(plotData,distdata[distdata$Experiment == 'categ',])


CairoPDF('../thesis/graphs/exp3_mds.pdf',width=6.69,height=4.33)
ggplot(plotData,aes(x=X, y=Y, colour=Experiment, label=Sound, size = ExposureType)) + geom_text() + scale_size_manual(name='Exposure Type',values = c(8,8,4))+ theme_bw() + scale_colour_discrete(labels=c('Categorization', 'Exposure', 'Original')) + xlab('First principal component') + ylab('Second principal component')+ theme(text=element_text(size=8),legend.title=element_text(size=7),legend.text=element_text(size=6))
dev.off()

distdataword <- read.delim('similarity_analysis//output_word.txt')

distdataword$Sound <- 's'
distdataword[str_detect(distdataword$Type,'^SH'),]$Sound <- 'ʃ'

distdataword$Sound <- factor(distdataword$Sound)

distdataword$Position <- 'Initial'
distdataword[str_detect(distdataword$Type,'Final'),]$Position <- 'Final'

distdataword$Position <- factor(distdataword$Position)

distdataword$ExposureType <- 'Isolation'
distdataword[str_detect(distdataword$Type,'U$'),]$ExposureType <- 'Unpredictive'
distdataword[str_detect(distdataword$Type,'P$'),]$ExposureType <- 'Predictive'

distdataword$ExposureType <- factor(distdataword$ExposureType)

summary(distdataword)

plotData <- subset(distdataword,Experiment %in% c('categ','original','exp1', 'exp2') & ExposureType == 'Isolation')

ggplot(plotData,aes(x=X, y=Y, colour=Experiment, label=Word, size = Position)) + geom_text() + scale_size_manual(values = c(6,9))

plotData <- subset(distdataword,Experiment %in% c('categ','original','exp3') & Position == 'Final')

ggplot(plotData,aes(x=X, y=Y, colour=Experiment, label=Sound, size = ExposureType)) + geom_text() + scale_size_manual(values = c(6,9,12))
