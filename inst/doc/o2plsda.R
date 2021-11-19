## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  install.package("o2plsda")

## -----------------------------------------------------------------------------
library(o2plsda)
set.seed(123)
# sample * values
X = matrix(rnorm(5000),50,100)
# sample * values
Y = matrix(rnorm(5000),50,100)
##add sample names
rownames(X) <- paste("S",1:50,sep="")
rownames(Y) <- paste("S",1:50,sep="")
## gene names
colnames(X) <- paste("Gene",1:100,sep="")
colnames(Y) <- paste("Lipid",1:100,sep="")
##scaled
X = scale(X, scale = TRUE)
Y = scale(Y, scale = TRUE)
## group factor could be omitted if you don't have any group 
group <- rep(c("Ctrl","Treat"), each = 25)

## -----------------------------------------------------------------------------
set.seed(123)
## nr_folds : cross validation k-fold (suggest 10)
## ncores : parallel paramaters for large datasets
cv <- o2cv(X,Y,1:5,1:3,1:3, group = group, nr_folds = 10)
#####################################
# The best parameters are nc =  5 , nx =  3 , ny =  3 
#####################################
# The Qxy is  0.08222935  and the RMSE is:  2.030108 
#####################################

## -----------------------------------------------------------------------------
fit <- o2pls(X,Y,5,3,3)
summary(fit)
######### Summary of the O2PLS results #########
### Call o2pls(X, Y, nc= 5 , nx= 3 , ny= 3 ) ###
### Total variation 
### X: 4900 ; Y: 4900  ###
### Total modeled variation ### X: 0.286 ; Y: 0.304  ###
### Joint, Orthogonal, Noise (proportions) ###
#               X     Y
#Joint      0.176 0.192
#Orthogonal 0.110 0.112
#Noise      0.714 0.696
### Variation in X joint part predicted by Y Joint part: 0.906 
### Variation in Y joint part predicted by X Joint part: 0.908 
### Variation in each Latent Variable (LV) in Joint part: 
#      LV1     LV2     LV3     LV4     LV5
#X 181.764 179.595 191.210 152.174 157.819
#Y 229.308 204.829 175.926 173.382 155.934
### Variation in each Latent Variable (LV) in X Orthogonal part: 
#      LV1     LV2     LV3
#X 227.856 166.718 143.602
### Variation in each Latent Variable (LV) in Y Orthogonal part: 
#      LV1     LV2     LV3
#Y 225.833 166.231 157.976


## -----------------------------------------------------------------------------
Xl <- loadings(fit,loading="Xjoint")
Xs <- scores(fit,score="Xjoint")
plot(fit,type="score",var="Xjoint", group=group)
plot(fit,type="loading",var="Xjoint", group=group,repel=F,rotation=TRUE)

## -----------------------------------------------------------------------------
res <- oplsda(fit,group, nc=5)
plot(res,type="score", group=group)
vip <- vip(res)
plot(res,type="vip", group = group, repel = FALSE,order=TRUE)

