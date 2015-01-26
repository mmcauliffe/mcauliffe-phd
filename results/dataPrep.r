library(lme4)
library(ggplot2)
library(plyr)
library(stringr)

#s100 initial, attend
#s200 final, noattend
#s300 initial, noattend
#s400 final, attend

#EXPOSURE
expose <- read.delim('exp1_native_expose.txt')
expose$Experiment <- 'exp1'

expose$Attention <- 'attend'

expose[str_detect(expose$Subject,'^ns1-2'),]$Attention <- 'noattend'
expose[str_detect(expose$Subject,'^ns1-3'),]$Attention <- 'noattend'

expose$Attention <- factor(expose$Attention)

expose$ExposureType <- 'initial'

expose[str_detect(expose$Subject,'^ns1-2'),]$ExposureType <- 'final'
expose[str_detect(expose$Subject,'^ns1-4'),]$ExposureType <- 'final'

expose$ExposureType <- factor(expose$ExposureType)

t <- read.delim('exp2_native_expose.txt')
t$Experiment <- 'exp2'
t$CRESP <- NULL

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns2-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns2-3'),]$Attention <- 'noattend'

t$Attention <- factor(t$Attention)

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns2-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns2-4'),]$ExposureType <- 'final'

t$ExposureType <- factor(t$ExposureType)

expose <- rbind(expose,t)
expose$Experiment <- factor(expose$Experiment)

expose <- subset(expose,RT > 200 & RT < 2500)
#expose[expose$RT < 200,]$ACC <- 0
#expose[expose$RT > 2500,]$ACC <- 0
expose <- na.omit(expose)

expose.word <- subset(expose,Lexicality=='Word')
expose.word$Word <- factor(expose.word$Word)

target <- na.omit(subset(expose,itemtype %in% c('S-Initial','S-Final')))

subj.tolerances <- ddply(target,~Subject*itemtype*Attention*Experiment,summarise,WordResp = mean(ACC))

summary(aov(WordResp ~ itemtype*Attention*Experiment,data=subj.tolerances))

#CATEGORIZATION


categ <- read.delim('exp1_native_categ.txt')
categ$Experiment <- 'exp1'

categ$ExposureType <- 'initial'

categ[str_detect(categ$Subject,'^ns1-2'),]$ExposureType <- 'final'
categ[str_detect(categ$Subject,'^ns1-4'),]$ExposureType <- 'final'

categ$ExposureType <- factor(categ$ExposureType)

categ$Attention <- 'attend'

categ[str_detect(categ$Subject,'^ns1-2'),]$Attention <- 'noattend'
categ[str_detect(categ$Subject,'^ns1-3'),]$Attention <- 'noattend'

categ$Attention <- factor(categ$Attention)


t <- read.delim('exp2_native_categ.txt')
t$Experiment <- 'exp2'

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns2-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns2-4'),]$ExposureType <- 'final'

t$ExposureType <- factor(t$ExposureType)

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns2-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns2-3'),]$Attention <- 'noattend'

t$Attention <- factor(t$Attention)

categ <- rbind(categ,t)

cont <- read.delim('control_native_categ.txt')

cont <- na.omit(cont)

cont$ACC <- 0
cont[cont$RESP == cont$SResp,]$ACC <- 1

cont <- subset(cont, RT > 200 & RT < 2500)

t <- paste(cont$Label1,cont$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

cont$Item <- factor(t)
cont$Step <- cont$Step - mean(1:6)
cont$Background <- 'Native'

nncont <- read.delim('control_nonnative_categ.txt')

nncont <- na.omit(nncont)

nncont$ACC <- 0
nncont[nncont$RESP == nncont$SResp,]$ACC <- 1

nncont <- subset(nncont, RT > 200 & RT < 2500)

t <- paste(nncont$Label1,nncont$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

nncont$Item <- factor(t)
nncont$Step <- nncont$Step - mean(1:6)
nncont$Background <- 'Non-native'
cont <- rbind(cont, nncont)
cont$Background <- factor(cont$Background)
#categ <- rbind(categ, cont)
categ$Experiment <- factor(categ$Experiment)

categ <- na.omit(categ)

categ <- subset(categ, RT > 200 & RT < 2500)

#fix for coding error
categ <- subset(categ,Trial < 169)
sub <- subset(categ,Subject %in% c('ns1-113','ns1-114','ns1-115','ns1-118'))
sub$RealAcc = 0
sub[sub$ACC == 0,]$RealAcc = 1

categ[categ$Subject %in% c('ns1-113','ns1-114','ns1-115','ns1-118'),]$ACC = sub$RealAcc

t <- paste(categ$Label1,categ$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ$Item <- factor(t)

categ <- subset(categ,!Subject %in% c('ns1-215','ns1-402','ns2-214', 'ns2-219'))

categ$Step <- categ$Step - mean(1:6)


getCrossOver <- function(data){
  data$p <- -1*data[,'(Intercept)']/data[,'Step']
  data$pRound <- round(data$p)
  
  data <- data.frame(Subject = row.names(data),Xover = data$p)
  return(data)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}