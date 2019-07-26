setwd("~/Documents/daviesLab/fgb_Harvey/HarveySalinityData/SalinityData/") # change to your working directory
library(tidyverse) # for data wrangling and plotting
library(gridExtra) # for grid.arrange

d <- read.table("salinity_2013_end.txt", quote="\"", comment.char="")
head(d)
names(d) <- c("Date", "Time", "WaterT", "Conductivity", "Salt", "Density", "Sound")

# Remove rows where all measurements are zero
d <- d %>% filter(!Salt==0)
head(d)
summary(d)

# Summarize per day
d <- d %>% mutate(Day = substr(Date, start = 1, stop = 5)) 

# check out some weird dates
head(d)
d %>% filter(Day == "08/05") %>% select(Date, Salt)
# Salinity was only 0.01 in 2016- remove these

d %>% filter(Day == "08/23") %>% select(Date, Salt)
# Salinity was only 0.3 in 2016- remove these

dFilt <- d %>% filter(!Date == "08/05/2016") %>% filter(!Date == "08/23/2016")
head(dFilt)
tail(dFilt)

# Summarize by day

dSum <- dFilt %>% ungroup() %>%
  group_by(Day) %>% 
  summarize(Salinity = mean(Salt), sdSalt = sd(Salt), 
            minSalt = min(Salt), maxSalt = max(Salt),
            Temperature = mean(WaterT), sdTemp = sd(WaterT), 
            minT = min(WaterT), maxT = max(WaterT))
head(dSum)

dSum %>% filter(Day == "09/16")
dSum %>% filter(Day == "10/21")

# Day   Salinity sdSalt minSalt maxSalt Temperature sdTemp  minT  maxT
# <chr>    <dbl>  <dbl>   <dbl>   <dbl>       <dbl>  <dbl> <dbl> <dbl>
# 09/16     35.8  0.615    34.2    36.6        29.1  0.890  27.6  30.4
# 10/21     36.1  0.286    35.6    36.5        27.8  0.496  27.2  28.6

# Actual sampling day mean?
septTime <- d %>% filter(Date == "09/16/2017") %>% 
  summarize(minSalt = min(Salt), meanSalt = mean(Salt), sdSalt = sd(Salt),
            minTemp = min(WaterT), meanTemp = mean(WaterT), sdTemp = sd(WaterT))

septTime

# minSalt meanSalt    sdSalt minTemp meanTemp    sdTemp
# 34.2 34.64958 0.2072203   27.61 27.81188 0.1288518

octTime <- d %>% filter(Date == "10/21/2017") %>% 
  summarize(minSalt = min(Salt), meanSalt = mean(Salt), sdSalt = sd(Salt),
            minTemp = min(WaterT), meanTemp = mean(WaterT), sdTemp = sd(WaterT))

octTime
# minSalt meanSalt     sdSalt minTemp meanTemp    sdTemp
# 35.55   35.785 0.09684381   27.55 27.71438 0.2186993

# Plot ------

head(dSum)

plotSalt <- ggplot(dSum, aes(x = Day, y = Salinity, group = 1)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=minSalt, ymax=maxSalt), alpha = 0.5, colour=NA)+
  labs(x = "August-October 2013-2017", y = "Mean Daily Salinity") +
  geom_point(size=2) + 
  # geom_vline(xintercept = 47, linetype="dotted", 
  #            color = "red", size=1)+
  # geom_vline(xintercept = 82, linetype="dotted", 
  #            color = "red", size=1)+
  geom_point(aes(x=47, y=septTime$meanSalt), colour="red", shape=8, size=3)+
  geom_point(aes(x=82, y=octTime$meanSalt), colour="red", shape=8, size=3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=12),
        axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))


plotTemp <- ggplot(dSum, aes(x = Day, y = Temperature, group = 1)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=minT, ymax=maxT), alpha = 0.5, colour=NA)+
  labs(x = "August-October 2013-2017", y = "Mean Daily Temperature") +
  geom_point(size=2) + 
  # geom_vline(xintercept = 47, linetype="dotted", 
             # color = "red", size=1)+
  # geom_vline(xintercept = 82, linetype="dotted", 
             # color = "red", size=1)+
  geom_point(aes(x=47, y=septTime$meanTemp), colour="red", shape=8, size=3)+
  geom_point(aes(x=82, y=octTime$meanTemp), colour="red", shape=8, size=3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=12),
        axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))


pdf(file="temp_and_salinity.pdf", height = 10, width = 20)
grid.arrange(plotSalt, plotTemp, ncol=1)
dev.off()




