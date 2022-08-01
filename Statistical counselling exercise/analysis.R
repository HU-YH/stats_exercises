####loading data
stroop_data <- read.csv("sta490_cognitive_flexibility_data.csv")
stroop_data
library(ggplot2)
library(naniar)
library(stringr)
library(dplyr)
library(plyr)
####change colnames to easy-to-use form
colnames(stroop_data)
new_colnames <- c("x",
                  "colour_blind","yrs_English","video_games","device","headphones","order_of_levels",
                  "level_1","sleep_1","start_hour_1","Offtime_1","Ontime_1","Offrun_1","Onrun_1","difference_1",
                  "level_2","sleep_2","start_hour_2","Offtime_2","Ontime_2","Offrun_2","Onrun_2","difference_2",
                  "level_3","sleep_3","start_hour_3","Offtime_3","Ontime_3","Offrun_3","Onrun_3","difference_3","ID")
colnames(stroop_data) <- (new_colnames)
colnames(stroop_data)
stroop_data <- subset(stroop_data, select = -c(x))
####initial data conversion and cleaning
#####colour_blindness
#stroop_data$colour_blind

####since there is only one obs 'Yes' entry in colour blindness, it's impossible to take 
####effect of colour blindness into account, so I simply remove that observation

#stroop_data <- stroop_data[stroop_data$colour_blind != 'Yes' ,]
#stroop_data <- subset(stroop_data,select = -c(colour_blind))

####################Question: is this operation acceptable?
####################i.e. instead of delete the observation, should I simply do not take coulour blindness 
####################as a predictor during analysis but keep other data of this observation during analysis?

#####years_english
stroop_data$yrs_English
class(stroop_data$yrs_English[1])
as.character(stroop_data$yrs_English)



for (years in stroop_data$yrs_English) {
  if(is.na(as.numeric(years))){
    years <- substring(years,1,2)
  }
}

stroop_data$yrs_English <- as.numeric(stroop_data$yrs_English)
stroop_data$yrs_English

class(stroop_data$yrs_English)
#####rather than put in numeric value, some people added 's' in to time measurement,
#####need clean it


#########################data conversion for sleep_x
pattern_hour <- "([hH])"
for (time in stroop_data$sleep_1) {
  time <- str_split(time, pattern_hour[[1]][[1]])
}
stroop_data$sleep_1 <- as.numeric(stroop_data$sleep_1)
class(stroop_data$sleep_1)

for (time in stroop_data$sleep_2) {
  time <- str_split(time, pattern_hour[[1]][[1]])
}
stroop_data$sleep_1 <- as.numeric(stroop_data$sleep_1)
class(stroop_data$sleep_1)

for (time in stroop_data$sleep_3) {
  time <- str_split(time, pattern_hour[[1]][[1]])
}
stroop_data$sleep_1 <- as.numeric(stroop_data$sleep_3)
class(stroop_data$sleep_3)


######levels convert to consistent form

distractionTolevel <- function(distractionColumn){
  distractionColumn <- as.character(distractionColumn)
  print(distractionColumn)
  for(distraction in distractionColumn){
    if(str_detect(distraction,'[qQ]uiet') | str_detect(distraction,'[Cc]ontorol')){
      distraction <- 'Control (quiet)'
      print(distraction)
    }
    if(str_detect(distraction,'[Cc]lass') | str_detect(distraction,'[Mm]ozart')){
      distraction <- 'Classical (Mozart)'
      print(distraction)
    }
    else{
      distraction <- 'Song with lyrics (Shape of You by Ed Sheeran)'
      print(distraction)
    }
  }
  print(distractionColumn)
  return(distractionColumn)
}

stroop_data$level_1 <- distractionTolevel(stroop_data$level_1)
stroop_data$level_1
stroop_data$level_2 <- distractionTolevel(stroop_data$level_2)
stroop_data$level_2
stroop_data$level_3 <- distractionTolevel(stroop_data$level_3)
stroop_data$level_3


#####function dont work, try other approach
stroop_data$level_1 <- as.character(stroop_data$level_1)
class(stroop_data$level_1)
for(i in c(1:length(stroop_data$level_1))){
  if(str_detect(stroop_data$level_1[i],'[qQ]u') | str_detect(stroop_data$level_1[i],'[Cc]ontorol')){
    stroop_data$level_1[i] <- 'Control (quiet)'
  }
  print('start secondif')
  if(str_detect(stroop_data$level_1[i],'[Cc]lass') | str_detect(stroop_data$level_1[i],'[Mm]ozart')){
    stroop_data$level_1[i] <- 'Classical (Mozart)'
  }
  print('start thirdif')
  if(str_detect(stroop_data$level_1[i], '[Ss]ha') | str_detect(stroop_data$level_1[i], '[Ee]d')
                                                      | str_detect(stroop_data$level_1[i], '[Ll]yric')){
    stroop_data$level_1[i] <- 'Song with lyrics (Shape of You by Ed Sheeran)'
  }
}
print(stroop_data$level_1)


#####

pattern_record <- '[Ss]'
####1
stroop_data$Offtime_1 <- as.character(stroop_data$Offtime_1)
stroop_data$Offtime_1[stroop_data$ID == 54] <- str_replace(stroop_data$Offtime_1[stroop_data$ID == 54],'`','')
print(stroop_data$Offtime_1[stroop_data$ID == 54])

######Question: I have tried to assign stroop_data$Offtime_1[stroop_data$ID == 54] to a new variable
################then perform operations on the new variable, as a result, the original entry is unaffected
################I guess this might because I assign the value to the new variable instead of the object
################then how to make a pointer in R?


################Non-functional code provided below
################strange_entry <- stroop_data$Offtime_1 <- as.character(stroop_data$Offtime_1)
################strange_entry <- str_replace(strange_entry,'`','')
################stroop_data$Offtime_1[stroop_data$ID == 54]

stroop_data$Offtime_1 <- str_replace(stroop_data$Offtime_1, pattern_record, '')
stroop_data$Offtime_1 <- as.numeric(stroop_data$Offtime_1)
checkNA <- is.na(stroop_data$Offtime_1)
all(eval((-checkNA>=0)))
####2
stroop_data$Ontime_1 <- as.character(stroop_data$Ontime_1)
stroop_data$Ontime_1 <- str_replace(stroop_data$Ontime_1, pattern_record, '')
stroop_data$Ontime_1 <- as.numeric(stroop_data$Ontime_1)
checkNA <- is.na(stroop_data$Ontime_1)
all(eval((-checkNA>=0)))
####3
stroop_data$Offtime_2 <- as.character(stroop_data$Offtime_2)
stroop_data$Offtime_2 <- str_replace(stroop_data$Offtime_2, pattern_record, '')
stroop_data$Offtime_2 <- as.numeric(stroop_data$Offtime_2)
checkNA <- is.na(stroop_data$Offtime_2)
all(eval((-checkNA>=0)))
####4
stroop_data$Ontime_2 <- as.character(stroop_data$Ontime_2)
stroop_data$Ontime_2 <- str_replace(stroop_data$Ontime_2, pattern_record, '')
stroop_data$Ontime_2 <- as.numeric(stroop_data$Ontime_2)
checkNA <- is.na(stroop_data$Ontime_2)
all(eval((-checkNA>=0)))

####5
stroop_data$Offtime_3 <- as.character(stroop_data$Offtime_3)
stroop_data$Offtime_3 <- str_replace(stroop_data$Offtime_3, pattern_record, '')
stroop_data$Offtime_3 <- as.numeric(stroop_data$Offtime_3)
checkNA <- is.na(stroop_data$Offtime_3)
all(eval((-checkNA>=0)))

####6
stroop_data$Ontime_3 <- as.character(stroop_data$Ontime_3)
stroop_data$Ontime_3 <- str_replace(stroop_data$Ontime_3, pattern_record, '')
stroop_data$Ontime_3 <- as.numeric(stroop_data$Ontime_3)
checkNA <- is.na(stroop_data$Ontime_3)
all(eval((-checkNA>=0)))

####7
stroop_data$difference_1 <- as.character(stroop_data$difference_1)
stroop_data$difference_1 <- str_replace(stroop_data$difference_1, pattern_record, '')
stroop_data$difference_1 <- as.numeric(stroop_data$difference_1)
checkNA <- is.na(stroop_data$difference_1)
all(eval((-checkNA>=0)))

####8
stroop_data$difference_2 <- as.character(stroop_data$difference_2)
stroop_data$difference_2 <- str_replace(stroop_data$difference_2, pattern_record, '')
stroop_data$difference_2 <- as.numeric(stroop_data$difference_2)
checkNA <- is.na(stroop_data$difference_2)
all(eval((-checkNA>=0)))

####9
stroop_data$difference_3 <- as.character(stroop_data$difference_3)
stroop_data$difference_3 <- str_replace(stroop_data$difference_3, pattern_record, '')
stroop_data$difference_3 <- as.numeric(stroop_data$difference_3)
checkNA <- is.na(stroop_data$difference_3)
all(eval((-checkNA>=0)))

####################is it possible to do the above in a iterative way?

boxplot(stroop_data$Offtime_1,stroop_data$Ontime_1,stroop_data$Offtime_2,stroop_data$Ontime_2,
        stroop_data$Offtime_3,stroop_data$Ontime_3)
boxplot(stroop_data$Offtime_1,stroop_data$Offtime_2,stroop_data$Offtime_3)
boxplot(stroop_data$Ontime_1,stroop_data$Ontime_2,stroop_data$Ontime_3)


####
pairwise.t.test(c(stroop_data$Offtime_1,stroop_data$Offtime_2,stroop_data$Offtime_3),p.adj='bonf')
####not supposed to do this at this stage

###################there are 3 candidate misunderstand expected data input for Ontime and Offtime, 
###################need to take them out, but their difference time can still be used
###################should I create a copy of the dataframe without these 3 observations, or should I simply set
###################their ontime and offtime to N/A?



#####Now compare sample mean of 6 trials
Ontime <- data.frame(data=c(stroop_data$Ontime_1,stroop_data$Ontime_2,stroop_data$Ontime_3),
                key=c(
                  rep("firstOn", length(stroop_data$Ontime_1)),
                  rep("secondOn", length(stroop_data$Ontime_2)),
                  rep("thirdOn", length(stroop_data$Ontime_3))))
pairwise.t.test(Ontime$data,Ontime$key,p.adj='bonf')

Offtime <- data.frame(data=c(stroop_data$Offtime_1,stroop_data$Offtime_2,stroop_data$Offtime_3),
                     key=c(
                       rep("firstOff", length(stroop_data$Offtime_1)),
                       rep("secondOff", length(stroop_data$Offtime_2)),
                       rep("thirdOff", length(stroop_data$Offtime_3))))
pairwise.t.test(Offtime$data,Ontime$key,p.adj='bonf')


time <- data.frame(data=c(stroop_data$Ontime_1,stroop_data$Ontime_2,stroop_data$Ontime_3,
                          stroop_data$Offtime_1,stroop_data$Offtime_2,stroop_data$Offtime_3),
                     key=c(
                       rep("firstOn", length(stroop_data$Ontime_1)),
                       rep("secondOn", length(stroop_data$Ontime_1)),
                       rep("thirdOn", length(stroop_data$Ontime_1)),
                       rep("firstOff", length(stroop_data$Ontime_1)),
                       rep("secondOff", length(stroop_data$Ontime_1)),
                       rep("thirdOff", length(stroop_data$Ontime_1)))
                       )
pairwise_p <- pairwise.t.test(time$data,time$key,p.adjust = 'bonf')
pairwise_p

#####compare mean of on-off difference
difference <- data.frame(data=c(stroop_data$difference_1,stroop_data$difference_2,stroop_data$difference_3),
                         key=c(
                           rep("first", length(stroop_data$difference_1)),
                           rep("second", length(stroop_data$difference_2)),
                           rep("third", length(stroop_data$difference_3))))
pairwise.t.test(difference$data,difference$key,p.adj='bonf')
mean(stroop_data$difference_1)
mean(stroop_data$difference_2)
mean(stroop_data$difference_3)
########################
hist(stroop_data$Ontime_1)
hist(stroop_data$Offtime_1)
hist(stroop_data$Ontime_2)
hist(stroop_data$Offtime_2)
hist(stroop_data$Ontime_3)
hist(stroop_data$Offtime_3)

stroop_data <- subset(stroop_data, ID != 12 & ID!= 31 &ID != 66)

###############change test time to levels like (morning, afternoon, evening)
###############this could be an example of EDA comment
###############eg.test time is kind of mess ,so I decided to change them to levels


###############people intuitively choose the same order of test(same as default
###############data collection page), but it's supposed to be chosen randomly
###############so what to do about that?

###############check if difference equals actual difference

###############tranform from 72 row datafram to 216 row dataframe
###############in order to consider effect of both distration order and trial order
###############reshape() function

#################################################################################


data1 <- stroop_data %>% 
  select(ID, colour_blind, yrs_English, video_games, device, headphones, order_of_levels,
         level_1, sleep_1, start_hour_1, Offtime_1, Ontime_1, Offrun_1, Onrun_1, difference_1) %>%
  mutate(order = 1);

data2 <- stroop_data %>% 
  select(ID, colour_blind, yrs_English, video_games, device, headphones, order_of_levels,
         level_2, sleep_2, start_hour_2, Offtime_2, Ontime_2, Offrun_2, Onrun_2, difference_2) %>%
  mutate(order = 2);

data3 <- stroop_data %>% 
  select(ID, colour_blind, yrs_English, video_games, device, headphones, order_of_levels,
         level_3, sleep_3, start_hour_3, Offtime_3, Ontime_3, Offrun_3, Onrun_3, difference_3) %>%
  mutate(order = 3);


names(data1)[8:15] <- c("distraction_level", "sleep", "start_time", "OffTime", "OnTime", "Total_runs_Stroop_Off", "Total_runs_Stroop_On", "OnTime_minus_OffTime")
names(data2)[8:15] <- c("distraction_level", "sleep", "start_time", "OffTime", "OnTime", "Total_runs_Stroop_Off", "Total_runs_Stroop_On", "OnTime_minus_OffTime")
names(data3)[8:15] <- c("distraction_level", "sleep", "start_time", "OffTime", "OnTime", "Total_runs_Stroop_Off", "Total_runs_Stroop_On", "OnTime_minus_OffTime")

longdata <- rbind(data1, data2, data3)

longdata <- longdata %>% mutate(distraction_level = factor(distraction_level, levels=c("control", "classical", "lyrics")))

longdata <- longdata %>% arrange(id, order)

