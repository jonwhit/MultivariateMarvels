
#############################################################################################################
#RESAMPLING Examples

#02 - Explore Fish Larvae Neuston
setwd("~/PROJECTS/Workshops/MultivariateMarvels/SlickNurseries-master/")
library(tidyverse)

#sample(base) is a simple bootstrap - resampling method
#sample(data, 100000, replace = TRUE, prob = NULL)

#import datasets
fs <- read.csv("SlickNurseries_FishLarvae_IndividualSizes.csv", header=T)
#fs = fish size (1 row per individual fish) - long format
fs <- fs %>% 
  mutate(size = total.length.mm) %>% #rename 
  select(-total.length.mm, -Habitat, -Habitat.verbose, -genus, -species, -volume.m3) #simplify variables

##### Resampling Procedure #############
#separate by habitat
fs.out <- filter(fs, type=="Outside")
fs.in <- filter(fs, type=="Inside")


#Resample each Habitat (w/ replacement) 100,000 times
#sample(base) is a simple bootstrap - resampling method
boot.out <- as.data.frame(sample(fs.out$size, 100000, replace = TRUE, prob = NULL))
colnames(boot.out)[1] <- "size"
boot.out$type <- "Outside"

boot.in <- as.data.frame(sample(fs.in$size, 100000, replace = TRUE, prob = NULL))
colnames(boot.in)[1] <- "size"
boot.in$type <- "Inside"

#Merge dfs by habitat
boot.all <- rbind(boot.in, boot.out)

#Histogram 
hist(boot.in$size)

#PLOTS
require(ggplot2)
theme_set( theme_classic(base_size=9))

#Plot both together - Truncated X
log.y.breaks100k <- c(1,10,100,1000,10000,100000)
ggplot(boot.all, aes(size, fill=type, colour=type)) +
  geom_histogram(breaks=seq(2,80,1), alpha=0.6, position="identity", lwd=0.2,  colour="black") +
  scale_fill_manual(values = alpha(c("lightblue1","darkblue"),.9), labels = c("Slick", "Ambient")) +
  scale_x_continuous(breaks = pretty(fs$size, n = 10)) +
  scale_y_log10(breaks = log.y.breaks100k, labels = log.y.breaks100k, expand=c(0,0)) +
  annotation_logticks(sides = "l", size=.4)  +
  labs(title="", x="Larval fish size (mm)", y="Frequency", fill="") +
      theme() +
  ggsave("Resample_SizeDistribution.pdf", device="pdf",plot=last_plot(),dpi=300, units="in",scale=1, width=3.5,height=3)





