#load data
BLR_user_HRA <- read.csv('BLR_USER_HRA.csv',na.strings = c('NA','NULL'))
colnames(BLR_user_HRA)[1]<-"UserId"
BLR_user_HRA[,1] <- as.character(unlist(BLR_user_HRA[,1]))
#############################################################################

#remove rows with N/A BLR column
rmNA<-c()
for(i in 1:nrow(BLR_user_HRA)){
  if (is.na(BLR_user_HRA[i,][4])){
    rmNA <- c(rmNA,i)
  }
}

BLR_user_HRA <- BLR_user_HRA[-rmNA,]
BLR_user_HRA <- subset(BLR_user_HRA, duplicated(UserId)|duplicated(UserId,fromLast = TRUE))

rm(rmNA)
rm(i)

#rm long interval and na
rmLongInterval <- c()
for(i in 1:nrow(BLR_user_HRA)){
  if (as.numeric(difftime(as.Date(BLR_user_HRA$FinishedDate[i]), as.Date(BLR_user_HRA$CreatedDate[i]),units='auto'))>7){
    rmLongInterval <- c(rmLongInterval,i)
  }
}
BLR_user_HRA <- BLR_user_HRA[-rmLongInterval,]
BLR_user_HRA_duplicated <- subset(rmLongInterval, duplicated(UserId)|duplicated(UserId,fromLast = TRUE))
rm(rmLongInterval)
rm(i)


rmLongInterval <- BLR_user_HRA_duplicated[,-c(5,14)]
rm(BLR_user_HRA_duplicated)
BLR_user_HRA_duplicated_rmLongInterval <- BLR_user_HRA
########################################################################################

#calculate interval
BLR_user_HRA_duplicated <- BLR_user_HRA_duplicated %>%
  group_by(UserId) %>%
  mutate(Interval = as.numeric(difftime(as.Date(CreatedDate), 
                                        lag(as.Date(CreatedDate), default = as.Date(CreatedDate[1])), units = 'days')))
#########################################################################################
#rm starting row
num_row <- nrow(BLR_user_HRA_duplicated)
zeros <- rep(c(0),times = 17)
start_row <- c(1)
j=2
for(i in 2:num_row){
  #can do so here since I have ordered the dataframe by user id in previous code chunk
  if(all(BLR_user_HRA_duplicated[i,26:42]== zeros,na.rm=TRUE)& 
     BLR_user_HRA_duplicated$UserId[i] != BLR_user_HRA_duplicated$UserId[i-1]){
    start_row[j] <- i
    j <- j+1
  }
}
BLR_user_HRA_duplicated_omit_start <- BLR_user_HRA_duplicated[-start_row,]

#########################################################################################################


diff[paste(features[4],'change',sep='_')] <- (sorted_last[[features[4]]] - sorted_first[[features[4]]])
diff[paste(features[5],'change',sep='_')] <- (sorted_last[[features[5]]] - sorted_first[[features[5]]])
diff[paste(features[6],'change',sep='_')] <- (sorted_last[[features[6]]] - sorted_first[[features[6]]])
diff[paste(features[7],'change',sep='_')] <- (sorted_last[[features[7]]] - sorted_first[[features[7]]])
diff[paste(features[8],'change',sep='_')] <- (sorted_last[[features[8]]] - sorted_first[[features[8]]])
diff[paste(features[9],'change',sep='_')] <- (sorted_last[[features[9]]] - sorted_first[[features[9]]])
diff[paste(features[10],'change',sep='_')] <- (sorted_last[[features[10]]] - sorted_first[[features[10]]])
diff[paste(features[11],'change',sep='_')] <- (sorted_last[[features[11]]] - sorted_first[[features[11]]])
diff[paste(features[12],'change',sep='_')] <- (sorted_last[[features[12]]] - sorted_first[[features[12]]])
diff[paste(features[13],'change',sep='_')] <- (sorted_last[[features[13]]] - sorted_first[[features[13]]])
diff[paste(features[14],'change',sep='_')] <- (sorted_last[[features[14]]] - sorted_first[[features[14]]])
diff[paste(features[15],'change',sep='_')] <- (sorted_last[[features[15]]] - sorted_first[[features[15]]])
diff[paste(features[16],'change',sep='_')] <- (sorted_last[[features[16]]] - sorted_first[[features[16]]])
diff[paste(features[17],'change',sep='_')] <- (sorted_last[[features[17]]] - sorted_first[[features[17]]])
diff[paste(features[18],'change',sep='_')] <- (sorted_last[[features[18]]] - sorted_first[[features[18]]])
diff[paste(features[19],'change',sep='_')] <- (sorted_last[[features[19]]] - sorted_first[[features[19]]])
diff[paste(features[20],'change',sep='_')] <- (sorted_last[[features[20]]] - sorted_first[[features[20]]])
diff[paste(features[21],'change',sep='_')] <- (sorted_last[[features[21]]] - sorted_first[[features[21]]])

features <- colnames(diff)
for (i in seq(5,ncol(diff))){
  hist(diff[[features[i]]],main = features[[i]])
  print(i)
}
rm(i)

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}
