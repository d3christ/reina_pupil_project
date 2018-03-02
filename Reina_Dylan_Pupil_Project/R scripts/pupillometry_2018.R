#### Pupilometry data processing 
## Reina Mizrahi - Winter 2018
## Script imports data file from stripNtag, identifies missing pupil data and 
## interpolates missing values using na.approx (line 41). Quick sample graphs at the end of script.

## Pupilometry data 
library(data.table)
library(dplyr)
library(ggplot2)
library(zoo)

# Set working directory
setwd("~/Documents/po_data_edf/csv_files")

# Read in data file as "data.table" for processing efficiency in R
#Function that will load all participant .csv data files separately 
column_names <-c("subject","unknown", "nums", "xcoor", "ycoor", "pupsize", "timing", "trial", 
                 "experiment", "tone1", "tone2", "condition", "image", "one", "dot1", "dot2", 
                 "dot3", "trialfalse", "NOCLICK", "noclick")

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) 
  assign(temp[i], data.table(fread(temp[i], col.names = column_names)))

patt <- ls(pattern = "PO")

for (i in seq_along(patt)) {
  podat <- get(patt[i])
  podat$newtiming <- ifelse(podat$timing %% 2 == 0, (podat$timing = podat$timing), (podat$timing = podat$timing - 1))
  
  missing.pup <- data.table(which(podat$pupsize == 0))
  missing.pup$newcm <- missing.pup$V1 - 150
  missing.pup$newcp <- missing.pup$V1 + 150
  
  for(j in 1:nrow(missing.pup)) {
    podat$pupsize[missing.pup$newcm[j]:missing.pup$newcp[j]] <- NA
    print(paste("Now in row", j))
  }
  
  podat[,"Newpup"] <- NA
  podat$Newpup <- as.numeric(podat$Newpup) 
  
  podat$Newpup <- na.approx(podat$pupsize) #interpolation
  
  podat[,"Baseline"] <- 0
  podat$Baseline <- as.numeric(podat$Baseline)
  
  nTrials <- n_distinct(podat$trial)
  for (k in 1:nTrials) {
    base = podat[trial == k & newtiming == 4500]$Newpup
    podat[trial == k, Baseline := base]
  } 
  
  assign(paste("podat", i , sep = ""), podat) 
}
  

patt2 <- ls(pattern = "podat")
for (a in seq_along(patt2)) {
  bslng <- get(patt2[a])
  
  bslng[, "BVals"] <-  bslng$Newpup- bslng$Baseline
  bslng[,"Percentage"] <- ( bslng$Newpup/ bslng$Baseline)*100
  
  bslng$subject <- as.factor(bslng$subject)
  bslng$condition <- as.factor(bslng$condition)
  bslng$trial <- as.factor(bslng$trial)
  
  assign(paste("bslng", a , sep = ""), bslng) 
}

rm(bslng)
rm(po_all)

# The ones that end in 2 and 3, B 
grB <- rbind(bslng1, bslng4, bslng8, bslng9, bslng11, bslng13, bslng14, bslng15) 
grB$group <-  c("B")   
# The ones that end in 1 and 4, A 

grA <- rbind(bslng2, bslng3, bslng5, bslng6, bslng7,  bslng10, bslng12, bslng16)
grA$group <-  c("A")    

po_all <- rbind(grA, grB)
po_all$group <- as.factor(po_all$group)
str(po_all)

# Averaging and plotting 
po_all.gb <- group_by(po_all, subject, newtiming, condition, group)
po_all.sm <- summarise(po_all.gb, mPupSize = mean(BVals), sd = sd(BVals), nSubs = n_distinct(subject))

po_all.sm$se <- (po_all.sm$sd)/sqrt(po_all.sm$nSubs)
po_all.sm$upper <- ((po_all.sm$mPupSize) + (po_all.sm$se))
po_all.sm$lower <- ((po_all.sm$mPupSize) - (po_all.sm$se))
head(po_all.sm)
po_all.sm <- filter(po_all.sm, newtiming <= 8000)


# Filters data after 8000ms
dtsub <- filter(po_all.sm, newtiming <= 8000)

### Quick Graphs 
po_plot1 <- ggplot(dtsub, aes(x = newtiming, y = mPupSize, color = condition)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(aes(xintercept = 4500), colour = "darkgrey", linetype = "dashed")  + 
  ggtitle("Pupilometry") +
  labs(x="Time from trial onset",y="Pupil-Size Change")

po_plot2 <- ggplot(dtsub, aes(x = newtiming, y = mPupSize, color = condition)) + 
  geom_smooth() +
  ggtitle("Pupilometry") +
  facet_wrap(~group) + 
  geom_vline(aes(xintercept = 4500), colour = "darkgrey", linetype = "dashed")  + 
  labs(x="Time from trial onset",y="Pupil-Size Change")  + 
  theme_classic(base_size = 20) 

geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(), width = 0) + 
  geom_line(size = 1.5) +


# Graph plotting percentage change 
dt.gb.perc <- group_by(po_all, subject, newtiming, condition)
dt.sm.perc <- summarise(dt.gb.perc, percPupSize = mean(Percentage))

dtsubp <- subset(dt.sm.perc, newtiming <= 8000)

perc.plot1 <- ggplot(dtsubp , aes(x = newtiming, y = percPupSize, color = condition)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Pupilometry") +
  labs(x="Time from trial onset",y="Pupil-Size % Change")

perc.plot2 <- ggplot(dt.sm.perc, aes(x = newtiming, y = percPupSize, color = condition)) + 
  geom_smooth() +
  ggtitle("Pupilometry") +
  geom_vline(aes(xintercept = 4500), colour = "darkgrey", linetype = "dashed")  + 
  labs(x="Time from trial onset",y="Pupil-Size % Change") + 
  theme_classic(base_size = 20) 


po_plot2 
perc.plot2 



dtList <- list(POJIO4.csv,
               POJMI4.csv,
               POJLH4.csv,
               POJKL3.csv,
               POJLN3.csv,
               POJKI3.csv,
               POJIN2.csv,
               POJJM2.csv,
               POJMH2.csv,
               POJIL1.csv,
               POJLO1.csv,
               POJMK1.csv,
               POJKN1.csv,
               POJQL3.csv,
               POJJN4.csv,
               POJKK2.csv)