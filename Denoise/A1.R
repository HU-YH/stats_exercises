library(dtt)
boats <- matrix(scan('boats.txt'),ncol=256,byrow=T)
image(boats,axes=F,col=grey(seq(0,1,length=256)))


########################hard
denoise <- function(dctmat,lambda) {
  # if lambda is missing, set it to the 0.8 quantile of abs(dctmat)
  if(missing(lambda)) lambda <- quantile(abs(dct),0.8)
  # hard-thresholding
  a <- dctmat[1,1]
  dctmat1 <- ifelse(abs(dctmat)>lambda,dctmat,0)
  dctmat1[1,1] <- a
  # inverse DCT to obtain denoised image "clean"
  clean <- mvdct(dctmat1,inverted=T)
  clean <- ifelse(clean<0,0,clean)
  clean <- ifelse(clean>1,1,clean)
  clean
}

mvdct.boats <- mvdct(boats)
boats10 <- denoise(mvdct.boats, lambda = 10)
boats20 <- denoise(mvdct.boats, lambda = 20)
boats30 <- denoise(mvdct.boats, lambda = 30)
boats40 <- denoise(mvdct.boats, lambda = 40)

image(boats10,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 10',asp=1)
image(boats20,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 20',asp=1)
image(boats30,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 30',asp=1)
image(boats40,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 40',asp=1)

#####################soft


denoise <- function(dctmat,lambda) {
  # if lambda is missing, set it to the 0.8 quantile of abs(dctmat)
  if(missing(lambda)) lambda <- quantile(abs(dct),0.8)
  # soft-thresholding
  a <- dctmat[1,1]
  dctmat1 <- sign(dctmat)*pmax(abs(dctmat)-lambda,0)
  dctmat1[1,1] <- a
  # inverse DCT to obtain denoised image "clean"
  clean <- mvdct(dctmat1,inverted=T)
  clean <- ifelse(clean<0,0,clean)
  clean <- ifelse(clean>1,1,clean)
  clean
}


mvdct.boats <- mvdct(boats)
boats10 <- denoise(mvdct.boats, lambda = 10)
boats20 <- denoise(mvdct.boats, lambda = 20)
boats30 <- denoise(mvdct.boats, lambda = 30)
boats40 <- denoise(mvdct.boats, lambda = 40)

image(boats10,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 10',asp=1)
image(boats20,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 20',asp=1)
image(boats30,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 30',asp=1)
image(boats40,axes = F,col = grey(seq(0,1,length=256)),sub='lambda = 40',asp=1)




#################Q2
####################c

EvaluateM <- function(lambdau,lambdav,s){
  result <- min(lambdau(s-1)+lambdav(s^2-1)-log(1.e-5)/log(s))
  return(result)
}