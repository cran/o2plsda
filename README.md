# o2plsda: Multiomics Data Integration

_o2plsda_ provides functions to do O2PLS-DA analysis for multiple omics integration.The algorithm came from "O2-PLS, a two-block (X±Y) latent variable regression (LVR) method with an integral OSC filter" which published by Johan Trygg and Svante Wold at 2003. O2PLS is a bidirectional multivariate regression method that aims to separate the covariance between two data sets (it was recently extended to multiple data sets) (Löfstedt and Trygg, 2011; Löfstedt et al., 2012) from the systematic sources of variance being specific for each data set separately. 

### Cross-Validation

In order to avoid overfitting of the model, the optimal number of latent variables for each model structure was estimated using group-balanced MCCV. The package could use the group information when we select the best paramaters with cross-validation. In cross-validation (CV) one minimizes a certain measure of error over some parameters that should be determined a priori. Here, we have three parameters: (nc, nx, ny). A popular measure is the prediction error ||Y - \hat{Y}||, where \hat{Y} is a prediction of Y. In our case the O2PLS method is symmetric in X and Y, so we minimize the sum of the prediction errors: 
||X - \hat{X}||+||Y - \hat{Y}||. 


Here nc should be a positive integer, and nx and ny should be non-negative. The best integers are then the minimizers of the prediction error.

The O2PLS-DA analysis was performed as described by Bylesjö et al. (2007); briefly, the O2PLS predictive variation [$TW^\top$, $UC^\top$] was used for a subsequent O2PLS-DA analysis. The Variable Importance in the Projection (VIP) value was calculated as a weighted sum of the squared correlations between the OPLS-DA components and the original variable.

## Installation
```{r,eval=FALSE}
library(devtools)
install_github("guokai8/o2plsda")
``` 
## Examples
```{r}
library(o2plsda)
set.seed(123)
# sample * values
X = matrix(rnorm(5000),50,100)
# sample * values
Y = matrix(rnorm(5000),50,100)
rownames(X) <- paste("S",1:50,sep="")
rownames(Y) <- paste("S",1:50,sep="")
colnames(X) <- paste("Gene",1:100,sep="")
colnames(Y) <- paste("Lipid",1:100,sep="")
X = scale(X, scale=T)
Y = scale(Y, scale=T)
## group factor could be omitted if you don't have any group 
group <- rep(c("Ctrl","Treat"),each = 25)
```
Do cross validation with group information
```{r}
set.seed(123)
## nr_folds : cross validation k-fold (suggest 10)
## ncores : parallel paramaters for large datasets
cv <- o2cv(X,Y,1:5,1:3,1:3,group=group,nr_folds = 10)
#####################################
## The best parameters are nc = 1, nx = 2, ny = 1
#####################################
## The the RMSE is: 1.97990443734287
#####################################
```

Then we can do the O2PLS analysis with nc = 1, nx = 2, ny =1. You can also select the best paramaters by looking at the cross validation results.
```{r}
fit <- o2pls(X,Y,1,2,1)
summary(fit)
## 
## ######### Summary of the O2PLS results #########
## ### Call o2pls(X, Y, nc= 1 , nx= 2 , ny= 1 ) ###
## ### Total variation 
## ### X: 4900 ; Y: 4900  ###
## ### Total modeled variation ### X: 0.108 ; Y: 0.098  ###
## ### Joint, Orthogonal, Noise (proportions) ###
##                X     Y
## Joint      0.039 0.052
## Orthogonal 0.070 0.046
## Noise      0.892 0.902
## ### Variation in X joint part predicted by Y Joint part: 0.882 
## ### Variation in Y joint part predicted by X Joint part: 0.882 
## ### Variation in each Latent Variable (LV) in Joint part: 
##     LV1
## X 0.039
## Y 0.052
## ### Variation in each Latent Variable (LV) in X Orthogonal part: 
##     LV1   LV2
## X 0.036 0.034
## ### Variation in each Latent Variable (LV) in Y Orthogonal part: 
##     LV1
## Y 0.046
## 
## ############################################

############################################

```

Extract the loadings and scores from the fit results

```{r}
Xl <- loadings(fit,loading="Xjoint")
Xs <- scores(fit,score="Xjoint")
plot(fit,type="score",var="Xjoint", group=group)
plot(fit,type="loading",var="Xjoint", group=group,repel=F,rotation=TRUE)
```

Do the OPLSDA based on the O2PLS results
```{r}
res <- oplsda(fit,group, nc=1)
plot(res, type="score", group=group)
vip <- vip(res)
plot(res,type="vip", group = group, repel = FALSE,order=TRUE)
```

## Note
The package is still under development.

## Citation
If you like this package, please contact me for the citation.

## Contact information

For any questions please contact guokai8@gmail.com or https://github.com/guokai8/o2plsda/issues
