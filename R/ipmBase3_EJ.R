#### SET CLASSES
IPM <- setClass("IPM",
                  slots = c(species="character",
                            citation="character",
                            doi="character",
                            pars="data.frame",
                            vr="list",
                            boundaries="data.frame",
                            states="data.frame",
                            Pmat="function", 
                            Fmat="function", 
                            Cmat="function"))

#### EXAMPLE PARAMETERISATION
m<-IPM(species="Carduus nutans")
m@citation="Jongejans E, Shea K, Skarpaas O, Kelly D & Ellner SP (2011) Importance of individual and environmental variation for invasive species spread: a spatial integral projection model. Ecology 92:86-97"
m@doi="http://dx.doi.org/10.1890/09-2226.1"
m@pars<-data.frame(sigmaIntercept = -2.270,
                    sigmaSizeSlope= 0.569,
                    gammaIntercept= 2.751,
                    gammaSizeSlope= 0.407,
                    gammaGrowthVariationParam1= 9.119,
                    gammaGrowthVariationParam2= -0.228,
                    betaIntercept= -2.107,
                    betaSizeSlope= 0.860,
                    omegaIntercept= 6.363,
                    omegaSizeSlope= 0.0056,
                    pi= 374,
                    phi= 1,
                    epsilon= 0.019,
                    chiMean= -0.771,
                    chiSd= 1.719,
                    epsilon1= 0.1847,
                    sigma1= 0.038,
                    nu= 0.157)
rosetteSurvival<-function(size,pars) { 
  u <- exp(pars$sigmaIntercept+pars$sigmaSizeSlope*size)
  rosetteSurvival <- u/(1+u)
  return(rosetteSurvival)
}
rosetteGrowth<-function(sizeNext,size,pars) {
  meanSizeNext<-pars$gammaIntercept+pars$gammaSizeSlope*size
  sdSizeNext<-(pars$gammaGrowthVariationParam1*exp(pars$gammaGrowthVariationParam2*2*size))^0.5
  rosetteGrowth<-dnorm(sizeNext,mean=meanSizeNext,sd=sdSizeNext)
  return(rosetteGrowth)
}
floweringProbability<-function(size,pars) {
  u <- exp(pars$betaIntercept+pars$betaSizeSlope*size)
  floweringProbability <- u/(1+u)
  return(floweringProbability)
}
numberFlowerHeads<-function(size,pars) {
  numberFlowerHeads<-pars$omegaIntercept+pars$omegaSizeSlope*exp(size)
  return(numberFlowerHeads)
}
seedlingSizes<-function(sizeNext,pars) {
  seedlingSizeNext<-dnorm(sizeNext,mean=pars$chiMean,sd=pars$chiSd)
}

m@vr<-list(rosetteSurvival=rosetteSurvival,rosetteGrowth=rosetteGrowth,floweringProbability=floweringProbability,numberFlowerHeads=numberFlowerHeads,seedlingSizes=seedlingSizes)
m@boundaries<-data.frame(minSize=-5.578801,maxSize=7.446354)
m@states<-data.frame(seedBank=1,rosetteSize=50)

UmatN<-function(vr,pars,sizes,sizesNext=NA) {
  survival<-vr$rosetteSurvival(size=sizes,pars=pars)
  flowering<-vr$floweringProbability(size=sizes,pars=pars)
  survivalRates<-c(pars$sigma1+pars$epsilon1,survival*(1-flowering))
  return(matrix(survivalRates,byrow=T,nrow=1+length(sizesNext),ncol=1+length(sizes)))
}
UmatD<-function(vr,pars,sizes,sizesNext=NA,binWidth,correction="none") {
  mat<-matrix(0,nrow=1+length(sizesNext),ncol=1+length(sizes))
  mat[2:(1+length(sizesNext)),2:(1+length(sizes))]<-outer(sizes,sizesNext,vr$rosetteGrowth,pars=pars)*binWidth
  if (correction=="constant") mat<-mat/matrix(colSums(mat),nrow=nrow(mat),ncol=ncol(mat),byrow=T) 
  mat[1,1]<-pars$sigma1/(pars$sigma1+pars$epsilon1)
  seedlingSizeD<-vr$seedlingSizes(sizeNext=sizesNext,pars)*binWidth
  if (correction=="constant") seedlingSizeD<-seedlingSizeD/sum(seedlingSizeD)
  mat[2:(1+length(sizesNext)),1]<-seedlingSizeD*pars$epsilon1/(pars$epsilon1+pars$sigma1)
  return(mat)
}
FmatN<-function(vr,pars,sizes,sizesNext=NA) {
  survival<-vr$rosetteSurvival(size=sizes,pars=pars)
  flowering<-vr$floweringProbability(size=sizes,pars=pars)
  numberFlowerHeads<-vr$numberFlowerHeads(size=sizes,pars=pars)
  seedProductionRates<-c(0,survival*flowering*numberFlowerHeads*pars$pi*pars$phi)
  return(matrix(seedProductionRates,byrow=T,nrow=1+length(sizesNext),ncol=1+length(sizes)))
}
FmatD<-function(vr,pars,sizes,sizesNext=NA,binWidth,correction="none") {
  mat<-matrix(0,nrow=1+length(sizesNext),ncol=1+length(sizes))
  mat[2:(1+length(sizesNext)),2:(1+length(sizes))]<-outer(sizes,sizesNext,vr$rosetteGrowth,pars=pars)*binWidth
  if (correction=="constant") mat<-mat/matrix(colSums(mat),nrow=nrow(mat),ncol=ncol(mat),byrow=T) 
  mat[1,1]<-pars$sigma1/(pars$sigma1+pars$epsilon1)
  seedlingSizeD<-vr$seedlingSizes(sizeNext=sizesNext,pars)*binWidth
  if (correction=="constant") seedlingSizeD<-seedlingSizeD/sum(seedlingSizeD)
  mat[2:(1+length(sizesNext)),1]<-seedlingSizeD*pars$epsilon1/(pars$epsilon1+pars$sigma1)
  return(mat)
}

# - survival missing in equation 3. included here
# - eviction not dealt with. here an option
# - According to the model (equation 4), seeds in the seed bank cannot establish and flower within a year.

m@Pmat<-function(m,midPoints,correction="none") {
  surv<-m@vr$rosetteSurvival(midPoints,pars)
  flowering<-vr$floweringProbability(bins,pars)
  growth<-outer(m=m,bins,bins,vr$rosetteGrowth,pars=pars)
  
  return(PmatLayers[[1]]*PmatLayers[[2]]*PmatLayers[[3]])
}

#USE
borders<-seq(from=unlist(m@boundaries[1]),to=unlist(m@boundaries[2]),length=m@states$rosetteSize+1)
binWidth<-borders[2]-borders[1]
sizes<-sizesNext<-midPoints<-borders[1:(length(borders)-1)]+binWidth/2

