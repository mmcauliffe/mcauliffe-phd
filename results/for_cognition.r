
for.cognition <- categ5[,c('Subject', 'Item', 'ACC','RT','ExposureType', 'cStep','cLogRT')]
for.cognition <- rbind(for.cognition,subset(cont, Background == 'Native')[,c('Subject','Item','ACC','RT','ExposureType','cStep','cLogRT')])

for.cognition$ExposureType <- factor(for.cognition$ExposureType, levels = c('control','unpredictive','predictive'))


for.cognition.model <- glmer(ACC ~ cStep * ExposureType + (1 + cStep|Subject) + (1 + cStep * ExposureType|Item), 
                             family = 'binomial',
                             data = for.cognition, 
                             control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(for.cognition.model)

for.cognition$ExposureType <- factor(for.cognition$ExposureType, levels = c('unpredictive','control','predictive'))
for.cognition.model <- glmer(ACC ~ cStep * ExposureType + (1 + cStep|Subject) + (1 + cStep * ExposureType|Item), 
                             family = 'binomial',
                             data = for.cognition, 
                             control = glmerControl(optCtrl = list(maxfun = 100000) ))
summary(for.cognition.model)

### PLOT

plotData <- summarySEwithin(data = for.cognition, measurevar = 'ACC', betweenvars = c('ExposureType'), withinvars = c('Step'), idvar = 'Subject')

plotData$Step <- as.numeric(as.character(plotData$Step)) + 3.5

plotData$ExposureType <- factor(plotData$ExposureType, levels = c('control','unpredictive','predictive'))

### MAIN PLOT

for.cognition.results <- ggplot(plotData,aes(x = Step, y = ACC, colour = ExposureType, shape = ExposureType, group = ExposureType)) 
for.cognition.results <- for.cognition.results + geom_point(size = 2.8) + geom_line(size=1.4) + geom_errorbar(aes(ymin = ACC - ci, ymax = ACC + ci), linetype = 'solid', size = 0.4) 
for.cognition.results <- for.cognition.results + ylab('Proportion /s/ response') + xlab('Continua step') 
for.cognition.results <- for.cognition.results + theme_bw() 
for.cognition.results <- for.cognition.results + theme(text=element_text(size = 22),
                                     legend.title=element_text(size = 18),
                                     legend.text=element_text(size = 18),
                                     legend.justification = c(0, 0), 
                                     legend.position = c(0, 0), 
                                     legend.background = element_blank())
for.cognition.results <- for.cognition.results + scale_x_continuous(breaks = 1:6)  
for.cognition.results <- for.cognition.results + scale_shape_manual(values = c(21, 22, 23),
                                                  labels = c('Control','Unpredictive', 'Predictive' ))
for.cognition.results <- for.cognition.results + scale_colour_manual(values = c( "#000000","#0072B2", "#D55E00"), 
                                                   labels = c('Control','Unpredictive', 'Predictive'))

for.cognition.results
ggsave('../thesis/graphs/cognition_categresults.pdf', width = 10, height = 6, units = 'in', dpi = 600)