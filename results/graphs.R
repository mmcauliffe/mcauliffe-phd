
if_labeller <- function(var, value){
  value <- as.character(value)
  if (var == "ExposureType") { 
    value[value == "initial"] <- "Word-initial"
    value[value == "final"]   <- "Word-medial"
    value[value == "isolation"] <- "Isolation"
    value[value == "unpredictive"]   <- "Unpredictive"
    value[value == "predictive"]   <- "Predictive"
  }
  if (var == "Attention") { 
    value[value == "attend"] <- "Attention"
    value[value == "noattend"]   <- "No Attention"
  }
  if (var == "Experiment") { 
    value[value == "exp1"] <- "Experiment 2"
    value[value == "exp2"]   <- "Experiment 1"
    value[value == "exp3"]   <- "Experiment 3"
  }
  if (var == 'itemtype2'){ 
    value[value == "S"] <- "/s/"
    value[value == "SH"]   <- "/ʃ/"
  }
  return(value)
}

### For grant

plotData <- summarySEwithin(data = subset(categ,Experiment=='exp2' & ExposureType == 'final'), measurevar = 'ACC', betweenvars = c('Attention'), withinvars=c('Step'), idvar= 'Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC', withinvars=c('Step'),idvar='Subject')
contPlotData <- cbind(contPlotData,data.frame(Attention = rep('control', 6)))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

for.grant <- ggplot(plotData, aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
for.grant <- for.grant + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 1)
for.grant <- for.grant + ylab('Proportion /s/ response') + xlab('Continua step') 
for.grant <- for.grant + scale_x_continuous(breaks = 1:6)  
for.grant <- for.grant + theme_bw() 
for.grant <- for.grant + theme(text = element_text(size = 18),
                               legend.justification = c(0, 0), 
                               legend.position = c(0, 0))
for.grant <- for.grant + scale_shape_manual(values = c(21, 22, 23),
                                           labels = c('No attention', 'Attention', 'Control'))
for.grant <- for.grant + scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                            labels = c('No attention', 'Attention', 'Control'))

for.grant
ggsave('exp1_final.pdf', width = 160, height = 160, units = 'mm', dpi = 600)
ggsave('exp1_final.png', width = 160, height = 160, units = 'mm', dpi = 600)

###

### EXPERIMENT 1 RESULTS

### EXPOSURE
for.plot <- subset(expose.word, Experiment == 'exp2')

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat, levels = c("1-50", "51-100", "101-150", "151-200"), ordered = T)

# ACC

plotData <- summarySEwithin(data = for.plot, measurevar = 'ACC', betweenvars = c('Attention', 'itemtype2'), withinvars = c('TrialCat'), idvar = 'Subject')

exp1.expose.acc <- ggplot(plotData, aes(x = TrialCat, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp1.expose.acc <- exp1.expose.acc + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp1.expose.acc <- exp1.expose.acc + facet_grid(~itemtype2, labeller = if_labeller) 
exp1.expose.acc <- exp1.expose.acc + ylab('Word recognition accuracy') + xlab('Exposure trial block')  
exp1.expose.acc <- exp1.expose.acc + theme_bw() 
exp1.expose.acc <- exp1.expose.acc + theme(text = element_text(size = 10),
                                           legend.title = element_text(size = 8),
                                           legend.text = element_text(size = 8),
                                           legend.justification = c(0, 0), 
                                           legend.position = c(0, 0))
exp1.expose.acc <- exp1.expose.acc +scale_shape_manual(values = c(21, 22),
                                                       labels = c('No attention', 'Attention'))
exp1.expose.acc <- exp1.expose.acc +scale_colour_manual(values = c("#0072B2", "#D55E00"),
                                                        labels = c('No attention', 'Attention'))

cairo_pdf('../thesis/graphs/exp1_expacc.pdf', width = 6.69, height = 3.15)
exp1.expose.acc
dev.off()

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

plotData <- summarySEwithin(data = subset(categ, Experiment == 'exp2'), measurevar = 'ACC', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar = 'Subject')

contPlotData <- summarySEwithin(data = cont, measurevar = 'ACC', withinvars = c('Step'), idvar = 'Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention = rep('control', 12),
                                              ExposureType = c(rep('final', 6), rep('initial', 6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp1.results <- ggplot(plotData, aes(x = Step,y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp1.results <- exp1.results + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp1.results <- exp1.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp1.results <- exp1.results + ylab('Proportion /s/ response') + xlab('Continua step') 
exp1.results <- exp1.results + theme_bw() 
exp1.results <- exp1.results + + theme(text = element_text(size = 10),
                                       legend.title = element_text(size = 8),
                                       legend.text = element_text(size = 8),
                                       legend.justification = c(0, 0), 
                                       legend.position = c(0, 0))
exp1.results <- exp1.results + scale_x_continuous(breaks = 1:6)  
exp1.results <- exp1.results +scale_shape_manual(values = c(21, 22, 23),
                                                 labels = c('No attention', 'Attention', 'Control'))
exp1.results <- exp1.results +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                  labels = c('No attention', 'Attention', 'Control'))
exp1.results
ggsave('../thesis/graphs/exp1_categresults_present.pdf', width = 170, height = 110, units = 'mm', dpi = 600)

ggsave('../thesis/graphs/exp1_categresults.pdf', width = 170, height = 80, units = 'mm', dpi = 600)

exp1.results.initial <- ggplot(subset(plotData, ExposureType == 'initial'), aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp1.results.initial <- exp1.results.initial + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp1.results.initial <- exp1.results.initial + facet_grid(~ExposureType, labeller = if_labeller) 
exp1.results.initial <- exp1.results.initial + ylab('Proportion /s/ response') + xlab('Continua step')
exp1.results.initial <- exp1.results.initial + theme_bw() 
exp1.results.initial <- exp1.results.initial + theme(text = element_text(size = 6), 
                                                     legend.title = element_text(size = 4), 
                                                     legend.text = element_text(size = 4), 
                                                     legend.justification = c(0, 0), 
                                                     legend.position = c(-0.05, -0.05), 
                                                     legend.background = element_blank())
exp1.results.initial <- exp1.results.initial + scale_x_continuous(breaks = 1:6)  
exp1.results.initial <- exp1.results.initial +scale_shape_manual(values = c(21, 22, 23), 
                                                                 labels = c('No attention', 'Attention', 'Control'))
exp1.results.initial <- exp1.results.initial +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                                  labels = c('No attention', 'Attention', 'Control'))

exp1.results.initial
ggsave('../thesis/graphs/exp1_categresults_present2-initial.pdf', width = 50, height = 85, units = 'mm', dpi = 600)

exp1.results.final <- ggplot(subset(plotData, ExposureType =='final'), aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp1.results.final <- exp1.results.final + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp1.results.final <- exp1.results.final + facet_grid(~ExposureType, labeller = if_labeller)
exp1.results.final <- exp1.results.final + ylab('Proportion /s/ response') + xlab('Continua step')
exp1.results.final <- exp1.results.final + theme_bw() 
exp1.results.final <- exp1.results.final + theme(text=element_text(size = 6),
                                                legend.title=element_text(size = 4),
                                                legend.text=element_text(size = 4),
                                                legend.justification=c(0, 0), 
                                                legend.position = c(-0.05, -0.05), 
                                                legend.background = element_blank())
exp1.results.final <- exp1.results.final + scale_x_continuous(breaks = 1:6)  
exp1.results.final <- exp1.results.final + scale_shape_manual(values = c(21, 22, 23),
                                                              labels = c('No attention', 'Attention', 'Control'))
exp1.results.final <- exp1.results.final + scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                               labels = c('No attention', 'Attention', 'Control'))

exp1.results.final

ggsave('../thesis/graphs/exp1_categresults_present2-final.pdf', width = 50, height = 85, units = 'mm', dpi = 600)

### END EXPERIMENT 1

### EXPERIMENT 2

### EXPOSURE


for.plot <- subset(expose.word, Experiment == 'exp1')

for.plot$TrialCat <- "1-50"
for.plot[for.plot$Trial < 101 & for.plot$Trial > 50,]$TrialCat <- "51-100"
for.plot[for.plot$Trial < 151 & for.plot$Trial > 100,]$TrialCat <- "101-150"
for.plot[for.plot$Trial > 150,]$TrialCat <- "151-200"

for.plot$TrialCat <- factor(for.plot$TrialCat)

# ACC

plotData <- summarySEwithin(data = for.plot, measurevar = 'ACC', withinvars = c('TrialCat', 'itemtype2'), idvar='Subject')

exp2.expose.acc <- ggplot(plotData, aes(x = TrialCat, y = ACC, colour = itemtype2, shape = itemtype2, group = itemtype2)) 
exp2.expose.acc <- exp2.expose.acc + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp2.expose.acc <- exp2.expose.acc + ylab('Word recognition accuracy') + xlab('Exposure trial block')  
exp2.expose.acc <- exp2.expose.acc + theme_bw() 
exp2.expose.acc <- exp2.expose.acc + theme(text = element_text(size = 10),
                                           legend.title = element_text(size = 8),
                                           legend.text = element_text(size = 8)) 
exp2.expose.acc <- exp2.expose.acc + scale_y_continuous(limits=c(0.25, 1),
                                                        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
exp2.expose.acc <- exp2.expose.acc + scale_shape_manual(name='Trial Type',
                                                        values = c(21, 22, 23), 
                                                        labels = c('Filler', '/s/', '/ʃ/')) 
exp2.expose.acc <- exp2.expose.acc + scale_colour_manual(name='Trial Type',
                                                         values = c('#000000', "#0072B2", "#D55E00"),
                                                         labels = c('Filler', '/s/', '/ʃ/'))

cairo_pdf('../thesis/graphs/exp2_expacc.pdf', width = 6.69, height = 3.15)
exp2.expose.acc
dev.off()

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

plotData <- summarySEwithin(data = subset(categ, Experiment == 'exp1'), measurevar = 'ACC', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar = 'Subject')

contPlotData <- summarySEwithin(data = cont, measurevar = 'ACC', withinvars = c('Step'), idvar = 'Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention = rep('control', 12),
                                              ExposureType = c(rep('final', 6), rep('initial', 6))))
#contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)

#contPlotData$Step <- as.numeric(as.character(contPlotData$Step)) + 3.5
plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp2.results <- ggplot(plotData, aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp2.results <- exp2.results + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp2.results <- exp2.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp2.results <- exp2.results + ylab('Proportion /s/ response') + xlab('Continua step')
exp2.results <- exp2.results + theme_bw() 
exp2.results <- exp2.results + theme(text = element_text(size = 10),
                                     legend.title = element_text(size = 8),
                                     legend.text = element_text(size = 8),
                                     legend.justification = c(0, 0), 
                                     legend.position = c(0, 0))
exp2.results <- exp2.results + scale_x_continuous(breaks = 1:6)  
exp2.results <- exp2.results +scale_shape_manual(values = c(21, 22, 23),
                                                 labels = c('No attention', 'Attention', 'Control'))
exp2.results <- exp2.results +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                  labels = c('No attention', 'Attention', 'Control'))

exp2.results
ggsave('../thesis/graphs/exp2_categresults_present.pdf', width = 170, height = 110, units = 'mm', dpi = 600)

ggsave('../thesis/graphs/exp2_categresults.pdf', width = 170, height = 80, units = 'mm', dpi = 600)

exp2.results.initial <- ggplot(subset(plotData, ExposureType =='initial'), aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp2.results.initial <- exp2.results.initial + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp2.results.initial <- exp2.results.initial + facet_grid(~ExposureType, labeller = if_labeller) 
exp2.results.initial <- exp2.results.initial + ylab('Proportion /s/ response') + xlab('Continua step') 
exp2.results.initial <- exp2.results.initial + theme_bw() 
exp2.results.initial <- exp2.results.initial + theme(text = element_text(size = 6),
                                                     legend.title = element_text(size = 4),
                                                     legend.text = element_text(size = 4),
                                                     legend.justification = c(0, 0), 
                                                     legend.position = c(-0.05, -0.05), 
                                                     legend.background = element_blank())
exp2.results.initial <- exp2.results.initial + scale_x_continuous(breaks = 1:6)  
exp2.results.initial <- exp2.results.initial +scale_shape_manual(values = c(21, 22, 23),
                                                                 labels = c('No attention', 'Attention', 'Control'))
exp2.results.initial <- exp2.results.initial +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                                  labels = c('No attention', 'Attention', 'Control'))

exp2.results.initial
ggsave('../thesis/graphs/exp2_categresults_present2-initial.pdf', width = 50, height = 85, units = 'mm', dpi = 600)

exp2.results.final <- ggplot(subset(plotData, ExposureType =='final'), aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp2.results.final <- exp2.results.final + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp2.results.final <- exp2.results.final + facet_grid(~ExposureType, labeller = if_labeller) 
exp2.results.final <- exp2.results.final + ylab('Proportion /s/ response') + xlab('Continua step') 
exp2.results.final <- exp2.results.final + theme_bw() 
exp2.results.final <- exp2.results.final + theme(text = element_text(size = 6),
                                                 legend.title = element_text(size = 4),
                                                 legend.text = element_text(size = 4),
                                                 legend.justification = c(0, 0), 
                                                 legend.position = c(-0.05, -0.05), 
                                                 legend.background = element_blank())
exp2.results.final <- exp2.results.final + scale_x_continuous(breaks = 1:6)  
exp2.results.final <- exp2.results.final +scale_shape_manual(values = c(21, 22, 23),
                                                             labels = c('No attention', 'Attention', 'Control'))
exp2.results.final <- exp2.results.final +scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                              labels = c('No attention', 'Attention', 'Control'))

exp2.results.final
ggsave('../thesis/graphs/exp2_categresults_present2-final.pdf', width = 50, height = 85, units = 'mm', dpi = 600)

### END EXPERIMENT 2

### GROUPED EXP 1 AND 2

plotData = subset(xovers, T)
plotData$Experiment <- factor(plotData$Experiment, levels = c('exp2', 'exp1'))

plotData$Xover = plotData$Xover + 3.5

xover.corr <- ggplot(plotData, aes(x = WordResp, y = Xover, colour = Experiment)) 
xover.corr <- xover.corr + geom_point(position = position_jitter()) + geom_smooth(method = 'lm', se = F) 
xover.corr <- xover.corr + geom_hline(y = 3.5, linetype='dashed')
xover.corr <- xover.corr + facet_grid(Attention ~ ExposureType, labeller = if_labeller) 
xover.corr <- xover.corr + ylab('Crossover step across continua') + xlab('Proportion "word" response to exposure /s/ items') 
xover.corr <- xover.corr + theme_bw()
xover.corr <- xover.corr + theme(text = element_text(size = 10),
                                 legend.title = element_text(size = 8),
                                 legend.text = element_text(size = 8),
                                 legend.justification = c(0, 1), 
                                 legend.position = c(0, 1), 
                                 legend.background = element_blank()) 
xover.corr <- xover.corr + scale_colour_discrete(labels=c('Experiment 1', 'Experiment 2')) 

xover.corr
ggsave('../thesis/graphs/exp12_xoverwordresp_present.pdf', width = 170, height = 110, units = 'mm', dpi = 600)

ggsave('../thesis/graphs/exp12_xoverwordresp.pdf', width = 170, height = 80, units = 'mm', dpi = 600)


### END GROUPED

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

plotData <- summarySEwithin(data = categ3, measurevar = 'ACC', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar='Subject')

contPlotData <- summarySEwithin(data = cont, measurevar = 'ACC', withinvars = c('Step'), idvar = 'Subject')
contPlotData <- rbind(contPlotData, contPlotData)
contPlotData <- cbind(contPlotData, data.frame(Attention=rep('control', 12),
                                              ExposureType=c(rep('predictive', 6), rep('unpredictive', 6))))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp3.results <- ggplot(plotData,aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp3.results <- exp3.results + geom_point(size=1.7)+ geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1)
exp3.results <- exp3.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp3.results <- exp3.results + ylab('Proportion /s/ response') + xlab('Continua step') 
exp3.results <- exp3.results + scale_x_continuous(breaks = 1:6)  
exp3.results <- exp3.results + theme_bw() 
exp3.results <- exp3.results + theme(text = element_text(size = 10),
                                     legend.title = element_text(size = 8),
                                     legend.text = element_text(size = 8),
                                     legend.justification = c(0, 0), 
                                     legend.position = c(0, 0))
exp3.results <- exp3.results + scale_shape_manual(values = c(21, 22, 23),
                                                  labels = c('No attention', 'Attention', 'Control'))
exp3.results <- exp3.results + scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                                   labels = c('No attention', 'Attention', 'Control'))

exp3.results
ggsave('../thesis/graphs/exp3_categresults_present.pdf', width = 170, height = 110, units = 'mm', dpi = 600)

ggsave('../thesis/graphs/exp3_categresults.pdf',width=170,height=80,units='mm',dpi=600)

## ISOLATION VS SENTENCES

plotData <- summarySEwithin(data = categ23, measurevar = 'ACC', betweenvars = c('Attention', 'ExposureType'), withinvars = c('Step'), idvar = 'Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC', withinvars = c('Step'), idvar='Subject')
contPlotData <- rbind(contPlotData, contPlotData, contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=rep('control', 18),
                                              ExposureType=c(rep('isolation', 6), rep('predictive', 6), rep('unpredictive', 6))))
plotData <- rbind(plotData, contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

exp13.results <- ggplot(plotData,aes(x = Step, y = ACC, colour = Attention, shape = Attention, group = Attention)) 
exp13.results <- exp13.results + geom_point(size = 1.7) + geom_line() + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.1) 
exp13.results <- exp13.results + facet_grid(~ExposureType, labeller = if_labeller) 
exp13.results <- exp13.results + ylab('Proportion /s/ response') + xlab('Continua step') 
exp13.results <- exp13.results + theme_bw() 
exp13.results <- exp13.results + theme(text=element_text(size = 10),
                                       legend.title=element_text(size = 8),
                                       legend.text=element_text(size = 8),
                                       legend.justification = c(0, 0), 
                                       legend.position = c(0, 0))
exp13.results <- exp13.results + scale_x_continuous(breaks = 1:6)  
exp13.results <- exp13.results + scale_shape_manual(values = c(21, 22, 23),
                                                    labels = c('No attention', 'Attention', 'Control'))
exp13.results <- exp13.results + scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"), 
                                                     labels = c('No attention', 'Attention', 'Control'))

exp13.results
ggsave('../thesis/graphs/exp23_categresults.pdf', width = 170, height = 80, units = 'mm', dpi = 600)


### END EXPERIMENT 3

# XOVER DISTRIBUTIONS


plotData <- merge(all.xovers, subj.info23)
plotData$Xover = plotData$Xover + 3.5

contPlotData <- cont.xover
contPlotData$ExposureType = 'Control'
contPlotData$Attention <- 'Control'
contPlotData$Xover = contPlotData$Xover + 3.5

plotData <- rbind(plotData,contPlotData)

plotData$ExposureType <- factor(plotData$ExposureType, levels = c('Control', 'isolation', 'unpredictive', 'predictive'), ordered=T)

exp13.dist <- ggplot(plotData,aes(x = ExposureType, y = Xover, colour = Attention)) 
exp13.dist <- exp13.dist + geom_violin()
exp13.dist <- exp13.dist + geom_hline(y = 3.5, linetype = 2)
exp13.dist <- exp13.dist + ylab('Crossover step across continua') + xlab('Exposure Condition')
exp13.dist <- exp13.dist + scale_colour_manual(values = c("#0072B2", "#D55E00", "#000000"),
                                               labels = c('No attention', 'Attention', 'Control')) 
exp13.dist <- exp13.dist + scale_x_discrete(labels = c('Control', 'Isolation', 'Unpredictive', 'Predictive')) 
exp13.dist <- exp13.dist + theme_bw() 
exp13.dist <- exp13.dist + theme(legend.justification = c(0, 1), 
                                 legend.position = c(0, 1)) 

exp13.dist
ggsave('../thesis/graphs/exp13_xoverdist.pdf', width = 170, height = 80, units = 'mm', dpi = 600)

## MISC CODE - NOT RUN

plotData = xovers3

plotData$Xover = plotData$Xover + 3.5
ggplot(plotData,aes(x=MeanLogRt,y=Xover)) + geom_point(position=position_jitter()) + geom_smooth(method='lm', se=F, colour='black') + ylab('Crossover step across continua') + xlab('Mean response time (log seconds)') + theme_bw()

ggsave('../thesis/graphs/exp3_xoverwordresp.pdf',width=170,height=80,units='mm',dpi=600)

plotData <- merge(all.xovers,all.subj.info)

contPlotData <- cont.xover
contPlotData$Experiment <- 'Control'
contPlotData$ExposureType = 'Control'
contPlotData$Attention <- 'Control'

plotData <- rbind(plotData,contPlotData)
plotData$ExposureType <- as.character(plotData$ExposureType)
plotData[plotData$ExposureType == 'initial',]$ExposureType <- 'Word-initial'
plotData[plotData$ExposureType == 'final',]$ExposureType <- 'Word-medial'
plotData[plotData$ExposureType == 'unpredictive',]$ExposureType <- 'Unpredictive'
plotData[plotData$ExposureType == 'predictive',]$ExposureType <- 'Predictive'
plotData$Xover = plotData$Xover + 3.5

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + facet_grid(~Experiment, scales='free_x', labeller=if_labeller) + geom_violin()+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) +theme_bw()+ theme(text=element_text(size=10),legend.title=element_text(size=6),legend.text=element_text(size=6),legend.position='bottom') + geom_hline(y=3.5, linetype=2)

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + facet_grid(~Experiment, scales='free_x', labeller=if_labeller) + geom_boxplot()+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) +theme_bw()+ theme(text=element_text(size=10),legend.title=element_text(size=6),legend.text=element_text(size=6),legend.position='bottom') + geom_hline(y=3.5, linetype=2)

ggplot(plotData,aes(x=ExposureType,y=Xover,colour=Attention)) + facet_grid(~Experiment, scales='free_x', labeller=if_labeller) + geom_point(size=6,position=position_dodge(.9))+ ylab('Crossover step across continua') +xlab('Exposure Type')+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('No attention','Attention','Control')) +theme_bw()+ theme(text=element_text(size=10),legend.title=element_text(size=6),legend.text=element_text(size=6),legend.position='bottom') + geom_hline(y=3.5, linetype=2)

ggsave('../thesis/graphs/exp123_xoverdist.pdf',width=170,height=110,units='mm',dpi=600)


plotData <- summarySEwithin(data = subset(categ,Experiment %in% c('exp1','exp2')),measurevar = 'ACC',betweenvars = c('Experiment','Attention','ExposureType'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',24),Attention=c(rep('attend',12),rep('noattend',12)),ExposureType=c(rep('final',6),rep('initial',6),rep('final',6),rep('initial',6))))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

### MAIN PLOT

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment, group = Experiment)) + geom_point(size=1.7)+facet_grid(Attention~ExposureType, labeller=if_labeller) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('Experiment 1','Experiment 2','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('Experiment 1','Experiment 2','Control'))

ggsave('../thesis/graphs/exp12_categresults.pdf',width=170,height=140,units='mm',dpi=600)

plotData <- summarySEwithin(data = subset(categ,Experiment %in% c('exp1','exp2')),measurevar = 'ACC',betweenvars = c('Experiment'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- cbind(contPlotData,data.frame(Experiment=rep('control',6)))
plotData <- rbind(plotData,contPlotData)

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment, group = Experiment)) + geom_point(size=1.7) +geom_line() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci),linetype='solid',size=0.1)+ ylab('Proportion /s/ response') +xlab('Continua step') + scale_x_continuous(breaks = 1:6)  + theme_bw() + theme(text=element_text(size=10),legend.title=element_text(size=8),legend.text=element_text(size=8),legend.justification=c(0,0), legend.position=c(0,0))+scale_shape_manual(values = c(21, 22,23),labels = c('Experiment 1','Experiment 2','Control'))+scale_colour_manual(values = c("#0072B2", "#D55E00","#000000"),labels = c('Experiment 1','Experiment 2','Control'))



##EXPERIMENT 3 EXPOSURE
plotData <- summarySEwithin(data=expose3, measurevar = 'LogRT', withinvars = c('Predictability','Type'), idvar = 'Subject')
ggplot(plotData,aes(y=LogRT,x = Predictability)) + facet_grid(~Type) + geom_point() + geom_errorbar(aes(ymin=LogRT - ci, ymax = LogRT + ci))

#BY SUBJECT
plotData <- summarySE(data=expose3, measurevar = 'LogRT', groupvars = c('Predictability','Type','Subject'))
ggplot(plotData,aes(y=LogRT,x = Predictability)) + facet_grid(Type~Subject) + geom_point() + geom_errorbar(aes(ymin=LogRT - ci, ymax = LogRT + ci))


## Exposure


ggplot(subset(expose.word,Experiment == 'exp2'),aes(y=LogRT, x=Trial, colour=itemtype2)) + geom_smooth(method='lm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose.word,Experiment == 'exp1'),aes(y=LogRT, x=Trial, colour=itemtype2)) + geom_smooth(method='lm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose.word,Experiment == 'exp2'),aes(y=ACC, x=Trial, colour=itemtype2)) + geom_smooth(method='glm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose.word,Experiment == 'exp1'),aes(y=ACC, x=Trial, colour=itemtype2)) + geom_smooth(method='glm')+facet_grid(Attention~ExposureType)

ggplot(subset(expose3,ExposureType=='predictive'),aes(y=LogRT, x=Trial, colour=Type)) + geom_smooth(method='lm')+facet_grid(Attention~Predictability)

ggplot(subset(expose3,ExposureType=='unpredictive'),aes(y=LogRT, x=Trial, colour=Type)) + geom_smooth(method='lm')+facet_grid(Attention~Predictability)

## End Exposure
