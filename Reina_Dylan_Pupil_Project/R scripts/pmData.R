#### Pupilometry data processing 
## Reina Mizrahi - Summer 2017 
## Script imports data file from stripNtag, identifies missing pupil data and 
## interpolates missing values using na.approx (line 41). Quick sample graphs at the end of script.

## Pupilometry data 
library(data.table)
library(dplyr)
library(ggplot2)

# Set working directory
setwd("~/Documents/samplePO")

# Read in data file as "data.table" for processing efficiency in R
podat <- fread("podcdat.csv")

# Add column labels and inspect data 
names(podat) <- c("subject","unknown", "nums", "xcoor", "ycoor", "pupsize", "timing", "trial", "experiment", "tone1", "tone2", "condition", "image")
str(podat) 

# Edits timing to ensure all values are even. 
podat$newtiming <- ifelse(podat$timing %% 2 == 0, (podat$timing = podat$timing), (podat$timing = podat$timing - 1))

# Creates data.table identifying all time-points were pupil size is 0 and timepoints that are 300ms prior/post missing data (150*2)
missing.pup <- data.table(which(podat$pupsize == 0))
missing.pup$newcm <- missing.pup$V1 - 150
missing.pup$newcp <- missing.pup$V1 + 150

# Replace missing pupil-size with NA and pupil-sizes at 300ms prior and post missing pupil size 
for(i in 1:nrow(missing.pup)) {
  podat$pupsize[missing.pup$newcm[i]:missing.pup$V1[i]] <- NA
  podat$pupsize[missing.pup$newcp[i]:missing.pup$V1[i]] <- NA
  print(paste("Now in row", i))
}

# Interpolation
# Create new column for baselined pupil-size and baseline
podat[,"Newpup"] <- NA
podat$Newpup <- as.numeric(podat$Newpup) 

podat$Newpup <- na.approx(podat$pupsize) #interpolation

podat[,"Baseline"] <- 0
podat$Baseline <- as.numeric(podat$Baseline)

# Baselining and percentages
nTrials <- n_distinct(podat$trial)
for (j in 1:nTrials) {
   base = podat[trial == j & newtiming == 4500]$Newpup
   podat[trial == j, Baseline := base]
} 

podat[, "BVals"] <- podat$Newpup-podat$Baseline
podat[,"Percentage"] <- (podat$Newpup/podat$Baseline)*100
str(podat)

# Changes columns of "subject", "condition", and "trial" to be factor class variables
podat$subject <- as.factor(podat$subject)
podat$condition <- as.factor(podat$condition)
podat$trial <- as.factor(podat$trial)
str(podat)

# Averaging and plotting 
podat.gb <- group_by(podat, newtiming, condition)
podat.sm <- summarise(podat.gb, mPupSize = mean(BVals))

# Filters data after 8000ms
dtsub <- filter(podat.sm, newtiming <= 8000)

### Quick Graphs 
ggplot(dtsub, aes(x = newtiming, y = mPupSize, color = condition)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(aes(xintercept = 4500), colour = "darkgrey", linetype = "dashed")  + 
  ggtitle("Pupilometry") +
  labs(x="Time from trial onset",y="Pupil-Size Change")

ggplot(podat.sm, aes(x = newtiming, y = mPupSize, color = condition)) + 
  geom_smooth() +
  ggtitle("Pupilometry") +
  labs(x="Time from trial onset",y="Pupil-Size Change")

# Graph plotting percentage change 
dt.gb.perc <- group_by(podat, newtiming, condition)
dt.sm.perc <- summarise(dt.gb.perc, percPupSize = mean(Percentage))

dtsubp <- subset(dt.sm.perc, newtiming <= 8000)

ggplot(dtsubp , aes(x = newtiming, y = percPupSize, color = condition)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Pupilometry") +
  labs(x="Time from trial onset",y="Pupil-Size % Change")


