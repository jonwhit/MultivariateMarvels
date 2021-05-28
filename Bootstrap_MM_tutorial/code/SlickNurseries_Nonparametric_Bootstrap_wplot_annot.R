#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Basic non-parameric Bootstrap
# Author: Felipe Carvalho (felipe.carvalho@noaa.gov)
  #Modified by Phil Neubauer and Jonathan Whitney
# Date: 02/11/2019 
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
library(scales) # For plotting
library(tidyverse)
# Setwd
setwd("~/PROJECTS/Workshops/MultivariateMarvels/SlickNurseries-master/")

#read data
dat.raw <- read.csv("SlickNurseries_FunctionalGroups_Densities.csv")

dat.raw <- dat.raw %>% 
  select(-transect) %>%
  arrange(sample, type)

head(dat.raw)
samples = unique(dat.raw$sample)
samples
results = NULL

# choose sample
nboots = 10000
for(i in 1:length(samples)){     #for each sample (e.g., species or group)
  dat = subset(dat.raw,sample==samples[i])  #subset data into samples (groups)
  dat$type <- as.factor(dat$type)
  types = unique(dat$type) 
  
  #-----------------------------
  # Do non-parametric bootsrap
  #-----------------------------
  
  boots  =  NULL # creates empty object
  mean_diff <- vector(,nboots)
  median_diff <- vector(,nboots)
  for(k in 1:nboots){     #for every individual bootstrap
    # Subset type j
    mu.temp = NULL # random data object
    dat.samp = dat
    dat.samp$type <- sample(dat.samp$type,size = length(dat.samp$type),replace = F) #Actual Resampling Step
    mean_diff[k] <- diff(tapply(dat.samp$density,dat.samp$type,mean)) #calculate difference in means
    median_diff[k] <- diff(tapply(dat.samp$density,dat.samp$type,median)) #calculate difference in medians
    
    for(j in 1:length(types)){
      # resample vector by type with replace and take mean
      mu.temp =  c(mu.temp,mean(sample(dat[dat$type==types[j],3],replace=TRUE)))
    }
    boots = rbind(boots,as.numeric(mu.temp)) #writes mean boots output file
  }
  boots= data.frame(boots)
  colnames(boots) = paste(types)
  
  # Ratio Calculation
  boots$Ratio <- (boots$Inside/mean(boots$Outside))
  
  # get stats from all your boots
  median = apply(boots,2,median, na.rm=TRUE)
  mean = apply(boots,2,mean, na.rm=TRUE)
  sd = apply(boots,2,sd, na.rm=TRUE)
  se = sd/sqrt(nboots)
  lci = apply(boots,2,quantile,c(0.025), na.rm=TRUE)
  uci = apply(boots,2,quantile,c(0.975), na.rm=TRUE)
  
  org_diff_mean <- diff(tapply(dat$density,dat$type,mean))
  org_diff_median <- diff(tapply(dat$density,dat$type,median))
  
  #Output your results summary file
  out = data.frame(sample=samples[i],type=c("Inside","Outside","Ratio"), mean,median,sd,se,lci,uci,`p(mean_out<in)` = c(mean(mean_diff > org_diff_mean)))
  
  results = rbind(results,out)
  
  #Distribution Plots (Delete Plot Chunk if bootstrap is stalling)
  cols = c(4,2,3,5,6,7)
  Par = list(mfrow=c(1,1),mar = c(5, 5, 1, 1), mgp =c(3,1,0),mai = c(0.7, 0.7, 0.1, 0.1),mex=0.8, tck = -0.02,cex=0.7)
  png(file = paste0("Bootstrap_",samples[i],".png"), width = 4.5, height = 4, 
      res = 200, units = "in")
  par(Par)
  Ymax = Xlim = NULL
  for(j in 1:2){
    dens = stats:::density(boots[,j],adjust=2)
    Ymax = max(c(Ymax,dens$y))
  }
  xlim = range(boots)
  plot(1,1,type="n",xlab=samples[i],ylab="Density",ylim=c(0,Ymax),xlim=xlim,log='x')
  for(j in 1:length(types)){
    dens = stats:::density(boots[,j],adjust=2)
    polygon(c(dens$x,rev(dens$x)),c(dens$y,rep(0,length(dens$y))),col=alpha(cols[j],0.3))
    lines(rep(mean[j],2),c(0,max(dens$y)),col=cols[j],lwd=2)
  }
  legend("topright",paste(types),pch=22,pt.bg=cols,pt.cex=2,cex=1.1,bty="n")
  dev.off()
  
  write.csv(boots, paste0(getwd(),"/",samples[i],"_boots.csv"),row.names=F)
}

# confirm with R bootstrap package
bootmean <- function(data,index){
  d <- data[index,]
  tapply(d$density,d$type,mean)
}

bootres <- boot::boot(dat,bootmean,R=10000)
boot::boot.ci(bootres,index=c(1,1))
boot::boot.ci(bootres,index=c(2,2))

##############################################
#Save Results
write.csv(results,"boot_results.csv",row.names = F)

#Results Output Metadata
#sample = taxon/group
#type = habitat (Inside=Slick; Outside=Ambient; Ratio=Slick/Ambient)
#mean = mean density (#/m3)
#median = median density (#/m3)
#sd = standard deviation of mean density
#se = standard error
#lci = lower confidence interval
#uci = upper confidence interval
#p.mean_out.in = empirical Probability [P(d ̅slick >d̅ ambient ]) that the mean density (d ̅)
  #inside slicks is greater than in ambient (outside) waters.

#Example Histograms of Boots output
hist(boots$Ratio)
hist(boots$Inside, xlim=c(0,200))
hist(boots$Outside, xlim=c(0,200))
