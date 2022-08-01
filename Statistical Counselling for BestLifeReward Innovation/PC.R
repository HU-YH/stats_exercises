PC <- cov(BLR_predictor)
Val <- eigen(PC)$values
Val
max(Val)/sum(Val)

PC.new <- cov2cor(cov(BLR_predictor))
Val.new <- eigen(PC.new)$values
Val.new
max(Val.new)/sum(Val.new)
Vec.new <- eigen(PC.new)$vectors

W = BLR_predictor  # just to create a data matrix of the same size of X

# now fill in the entries by calculating sample PCs
x.bar = apply(BLR_predictor,2,mean)

for(i in 1:6){
  for(j in 1:20){
    W[j,i] = Vec.new[,i] %*% ( BLR_predictor[j,] -x.bar)  # centered PCs
  }}

colnames(W) = paste("W", 1:17, sep="")

PC.model = lm(BLR_response ~ W[,1]+W[,3]+W[,7]+W[,9]+W[,10]+W[,16]+W[,11]+W[,12]+W[,13])
summary(PC.model)
plot(PC.model)
