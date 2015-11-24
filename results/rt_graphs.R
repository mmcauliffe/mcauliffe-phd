

### EXPERIMENT 1 RESULTS

### EXPOSURE
for.plot <- subset(expose.word, Experiment == 'exp2')

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat, levels = c("1-50", "51-100", "101-150", "151-200"), ordered = T)

# RT

plotData <- summarySEwithin(data = for.plot, measurevar = 'RT', betweenvars = c('itemtype2'), withinvars = c('TrialCat'), idvar = 'Subject')

exp1.expose.rt <- ggplot(plotData, aes(x = TrialCat,y = RT, colour = itemtype2, shape = itemtype2, group = itemtype2)) 
exp1.expose.rt <- exp1.expose.rt + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.1)
exp1.expose.rt <- exp1.expose.rt + ylab('Reaction time (ms)') + xlab('Exposure trial block')
exp1.expose.rt <- exp1.expose.rt + theme_bw() 
exp1.expose.rt <- exp1.expose.rt + theme(text = element_text(size = 10),
                                         legend.title = element_text(size = 8),
                                         legend.text = element_text(size = 8))
exp1.expose.rt <- exp1.expose.rt +scale_shape_manual(name='Trial Type',
                                                     values = c(21, 22, 23), 
                                                     labels = c('Filler', '/s/', '/ʃ/')) 
exp1.expose.rt <- exp1.expose.rt + scale_colour_manual(name='Trial Type',
                                                       values = c('#000000', "#0072B2", "#D55E00"),
                                                       labels = c('Filler', '/s/', '/ʃ/'))

cairo_pdf('../thesis/graphs/exp1_exprt.pdf', width = 6.69, height = 3.15)
exp1.expose.rt
dev.off()

### CATEGORIZATION

plotData <- summarySEwithin(data = subset(categ, Experiment == 'exp2'), measurevar = 'RT', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar = 'Subject')

contPlotData <- summarySEwithin(data = cont, measurevar = 'RT', withinvars = c('Step'), idvar = 'Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention = rep('control', 12),
                                              ExposureType = c(rep('final', 6), rep('initial', 6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp1.rt.results <- ggplot(plotData, aes(x = Step,y = RT, colour = Attention, shape = Attention, group = Attention)) 
exp1.rt.results <- exp1.rt.results + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.1)
exp1.rt.results <- exp1.rt.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp1.rt.results <- exp1.rt.results + ylab('Proportion /s/ response') + xlab('Continua step') 
exp1.rt.results <- exp1.rt.results + theme_bw() 
exp1.rt.results <- exp1.rt.results + theme(text = element_text(size = 10),
                                     legend.title = element_text(size = 8),
                                     legend.text = element_text(size = 8),
                                     #legend.justification = c(0, 0), 
                                     #legend.position = c(0, 0),
                                     legend.background = element_blank())
exp1.rt.results <- exp1.rt.results + scale_x_continuous(breaks = 1:6)  
exp1.rt.results <- exp1.rt.results +scale_shape_manual(values = c(21, 22, 23),
                                                 labels = c('No attention', 'Attention', 'Control'))
exp1.rt.results <- exp1.rt.results +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                  labels = c('No attention', 'Attention', 'Control'))
exp1.rt.results
ggsave('../thesis/graphs/exp1_rt_categresults.pdf', width = 170, height = 80, units = 'mm', dpi = 600)

### EXPERIMENT 2

### EXPOSURE


for.plot <- subset(expose.word, Experiment == 'exp1')

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat)

# RT

plotData <- summarySEwithin(data = for.plot, measurevar = 'RT', betweenvars = c('ExposureType'), withinvars = c('TrialCat', 'itemtype2'), idvar = 'Subject')

exp2.expose.rt <- ggplot(plotData,aes(x = TrialCat, y = RT, colour = ExposureType, shape = ExposureType, group = ExposureType)) 
exp2.expose.rt <- exp2.expose.rt + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.1)
exp2.expose.rt <- exp2.expose.rt + facet_grid(~itemtype2, labeller = if_labeller) 
exp2.expose.rt <- exp2.expose.rt + ylab('Reaction time (ms)') + xlab('Exposure trial block')  
exp2.expose.rt <- exp2.expose.rt + theme_bw() 
exp2.expose.rt <- exp2.expose.rt + theme(text = element_text(size = 10),
                                         legend.title = element_text(size = 8),
                                         legend.text = element_text(size = 8),
                                         legend.justification = c(0, 1), 
                                         legend.position = c(0, 1)) 
exp2.expose.rt <- exp2.expose.rt + scale_y_continuous(limits = c(900,1300))
exp2.expose.rt <- exp2.expose.rt + scale_shape_manual(name = 'Exposure Type',
                                                      values = c(21, 22),
                                                      labels = c('Word-initial', 'Word-final'))
exp2.expose.rt <- exp2.expose.rt +scale_colour_manual(name = 'Exposure Type',
                                                      values = c("#0072B2", "#D55E00"),
                                                      labels = c('Word-initial', 'Word-final'))

cairo_pdf('../thesis/graphs/exp2_exprt.pdf', width = 6.69, height = 3.15)
exp2.expose.rt
dev.off()

### CATEGORIZATION

plotData <- summarySEwithin(data = subset(categ, Experiment == 'exp1'), measurevar = 'RT', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar = 'Subject')

contPlotData <- summarySEwithin(data = cont, measurevar = 'RT', withinvars = c('Step'), idvar = 'Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention = rep('control', 12),
                                              ExposureType = c(rep('final', 6), rep('initial', 6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp2.results <- ggplot(plotData, aes(x = Step, y = RT, colour = Attention, shape = Attention, group = Attention)) 
exp2.results <- exp2.results + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.1)
exp2.results <- exp2.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp2.results <- exp2.results + ylab('Proportion /s/ response') + xlab('Continua step')
exp2.results <- exp2.results + theme_bw() 
exp2.results <- exp2.results + theme(text = element_text(size = 10),
                                     legend.title = element_text(size = 8),
                                     legend.text = element_text(size = 8),
                                     #legend.justification = c(0, 0), 
                                     #legend.position = c(0, 0)
                                     legend.background = element_blank())
exp2.results <- exp2.results + scale_x_continuous(breaks = 1:6)  
exp2.results <- exp2.results +scale_shape_manual(values = c(21, 22, 23),
                                                 labels = c('No attention', 'Attention', 'Control'))
exp2.results <- exp2.results +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                  labels = c('No attention', 'Attention', 'Control'))

exp2.results
ggsave('../thesis/graphs/exp2_rt_categresults.pdf', width = 170, height = 80, units = 'mm', dpi = 600)

### EXPERIMENT 3

### EXPOSURE

for.plot <- expose3
for.plot$itemtype2 <- 'Filler'
for.plot[for.plot$Type == 'S-final',]$itemtype2 <- 'S'
for.plot[for.plot$Type == 'SH-final',]$itemtype2 <- 'SH'

for.plot$TrialCat <- "1-25"
for.plot[for.plot$Trial < 51 & for.plot$Trial > 25,]$TrialCat <- "26-50"
for.plot[for.plot$Trial < 76 & for.plot$Trial > 50,]$TrialCat <- "51-75"
for.plot[for.plot$Trial > 75,]$TrialCat <- "75-100"

for.plot$TrialCat <- factor(for.plot$TrialCat)

# RT

plotData <- summarySEwithin(data = for.plot, measurevar = 'RT', betweenvars = c('Attention'), withinvars = c('TrialCat', 'itemtype2'), idvar = 'Subject')

exp3.expose.rt <- ggplot(plotData, aes(x = TrialCat, y = RT, colour = Attention, shape = Attention, group = Attention)) 
exp3.expose.rt <- exp3.expose.rt + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.1)
exp3.expose.rt <- exp3.expose.rt + facet_grid(~itemtype2, labeller = if_labeller) 
exp3.expose.rt <- exp3.expose.rt + ylab('Reaction time (ms)') +xlab('Exposure trial block')  
exp3.expose.rt <- exp3.expose.rt + theme_bw() 
exp3.expose.rt <- exp3.expose.rt + theme(text = element_text(size = 10),
                                         legend.title = element_text(size = 8),
                                         legend.text = element_text(size = 8),
                                         legend.justification = c(1,1), 
                                         legend.position = c(1,1), 
                                         legend.background = element_blank())
exp3.expose.rt <- exp3.expose.rt +scale_shape_manual(values = c(21, 22),labels = c('No attention', 'Attention'))
exp3.expose.rt <- exp3.expose.rt +scale_colour_manual(values = c("#0072B2", "#D55E00"),
                                                      labels = c('No attention', 'Attention'))

CairoPDF('../thesis/graphs/exp3_exprt.pdf', width = 6.69, height = 3.15)
exp3.expose.rt
dev.off()

### CATEGORIZATION

plotData <- summarySEwithin(data = categ3, measurevar = 'RT', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar='Subject')

contPlotData <- summarySEwithin(data = cont, measurevar = 'RT', withinvars = c('Step'), idvar = 'Subject')
contPlotData <- rbind(contPlotData, contPlotData)
contPlotData <- cbind(contPlotData, data.frame(Attention=rep('control', 12),
                                               ExposureType=c(rep('predictive', 6), rep('unpredictive', 6))))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp3.results <- ggplot(plotData,aes(x = Step, y = RT, colour = Attention, shape = Attention, group = Attention)) 
exp3.results <- exp3.results + geom_point(size=1.7)+ geom_line() + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.1)
exp3.results <- exp3.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp3.results <- exp3.results + ylab('Proportion /s/ response') + xlab('Continua step') 
exp3.results <- exp3.results + scale_x_continuous(breaks = 1:6)  
exp3.results <- exp3.results + theme_bw() 
exp3.results <- exp3.results + theme(text = element_text(size = 10),
                                     legend.title = element_text(size = 8),
                                     legend.text = element_text(size = 8)#,
                                     #legend.justification = c(0, 0), 
                                     #legend.position = c(0, 0)
                                     )
exp3.results <- exp3.results + scale_shape_manual(values = c(21, 22, 23),
                                                  labels = c('No attention', 'Attention', 'Control'))
exp3.results <- exp3.results + scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                   labels = c('No attention', 'Attention', 'Control'))

exp3.results
ggsave('../thesis/graphs/exp3_rt_categresults.pdf',width=170,height=80,units='mm',dpi=600)

### EXPERIMENT 5

plotData <- summarySEwithin(data = categ5, measurevar = 'RT', betweenvars = c('ExposureType'), withinvars = c('Step'), idvar = 'Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'RT', withinvars = c('Step'), idvar='Subject')
contPlotData <- cbind(contPlotData,data.frame(ExposureType=rep('control', 6)))
plotData <- rbind(plotData, contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

plotData$ExposureType <- factor(plotData$ExposureType, levels = c('control','unpredictive','predictive'))

### MAIN PLOT

exp5.results <- ggplot(plotData,aes(x = Step, y = RT, colour = ExposureType, shape = ExposureType, group = ExposureType)) 
exp5.results <- exp5.results + geom_point(size = 2.8) + geom_line(size=1.4) + geom_errorbar(aes(ymin = RT - ci, ymax = RT + ci), linetype = 'solid', size = 0.4) 
exp5.results <- exp5.results + ylab('Proportion /s/ response') + xlab('Continua step') 
exp5.results <- exp5.results + theme_bw() 
exp5.results <- exp5.results + theme(text=element_text(size = 22),
                                     legend.title=element_text(size = 18),
                                     legend.text=element_text(size = 18),
                                     #legend.justification = c(0, 0), 
                                     #legend.position = c(0, 0), 
                                     legend.background = element_blank())
exp5.results <- exp5.results + scale_x_continuous(breaks = 1:6)  
exp5.results <- exp5.results + scale_shape_manual(values = c(21, 22, 23),
                                                  labels = c('Control','Unpredictive', 'Predictive' ))
exp5.results <- exp5.results + scale_colour_manual(values = c( "#000000","#0072B2", "#D55E00"), 
                                                   labels = c('Control','Unpredictive', 'Predictive'))

exp5.results
ggsave('../thesis/graphs/exp5_rt_categresults.pdf', width = 10, height = 6, units = 'in', dpi = 600)