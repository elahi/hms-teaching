#changelog: 1/3/12 - changed npar calculated to difference between baseline and object df to accomodate fixed.x mishegas

library(AICcmodavg)

AICc.lavaan<-function(object, second.ord=TRUE, c.hat = 1, return.K = FALSE){
	object <- as.list(fitMeasures(object))
  npar<-object$baseline.df - object$df
	if(return.K==T) return(object$npar)
	if(second.ord==F && c.hat>1) return(-2*object$logl/c.hat+2*npar)
	if(second.ord==F) return(object$aic)
    if(c.hat>1) return( -2*object$logl/c.hat+2*npar + 2*( npar*(object$npar+1))/(object$ntotal-npar-1))
    object$aic + 2*( npar*(npar+1))/(object$ntotal-npar-1)
}
    
aictab.lavaan<-function(cand.set, modnames, sort = TRUE, c.hat = 1, second.ord = TRUE, nobs = NULL){
	if(is.null(modnames)) modnames<-1:length(cand.set)
	# check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
   # if (length(unique(check.resp)) > 1) 
   #     stop("You must use the same response variable for all models\n")
    Results <- NULL
    Results <- data.frame(Modnames = modnames)
    Results$K <- unlist(lapply(X = cand.set, FUN = AICc.lavaan, 
        return.K = TRUE, c.hat = c.hat,second.ord = second.ord))
    Results$AICc <- unlist(lapply(X = cand.set, FUN = AICc.lavaan, 
        return.K = FALSE, c.hat = c.hat,second.ord = second.ord))
    Results$Delta_AICc <- Results$AICc - min(Results$AICc)
    Results$ModelLik <- exp(-0.5 * Results$Delta_AICc)
    Results$AICcWt <- Results$ModelLik/sum(Results$ModelLik)
    if (length(unique(Results$AICc)) != length(cand.set)) 
        warning("\nCheck model structure carefully as some models may be redundant\n")
    if (second.ord == TRUE && c.hat == 1) {
        Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    }
    if (second.ord == TRUE && c.hat > 1) {
        colnames(Results) <- c("Modnames", "K", "QAICc", "Delta QAICc", 
            "ModelLik", "QAICcWt")
        LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
        Results$Quasi.LL <- LL/c.hat
        Results$c_hat <- c.hat
    }
    if (second.ord == FALSE && c.hat == 1) {
        colnames(Results) <- c("Modnames", "K", "AIC", "Delta AIC", 
            "ModelLik", "AICWt")
        Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    }
    if (second.ord == FALSE && c.hat > 1) {
        colnames(Results) <- c("Modnames", "K", "QAIC", "Delta QAIC", 
            "ModelLik", "QAICWt")
        LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
        Results$Quasi.LL <- LL/c.hat
        Results$c_hat <- c.hat
    }
    if (sort) {
        Results <- Results[rev(order(Results[, 6])), ]
        Results$Cum.Wt <- cumsum(Results[, 6])
    }
    else {
        Results$Cum.Wt <- NULL
    }
    class(Results) <- c("aictab", "data.frame")
    return(Results)
	
}


### The industrialization and Political Democracy Example 
### Bollen (1989), page 332
#model <- ' 
#  # latent variable definitions
#     ind60 =~ x1 + x2 + x3
#     dem60 =~ y1 + y2 + y3 + y4
#     dem65 =~ y5 + equal("dem60=~y2")*y6 
#                 + equal("dem60=~y3")*y7 
#                 + equal("dem60=~y4")*y8
#
#  # regressions
#    dem60 ~ ind60
#    dem65 ~ ind60 + dem60
#
#  # residual correlations
#    y1 ~~ y5
#    y2 ~~ y4 + y6
#    y3 ~~ y7
#    y4 ~~ y8
#    y6 ~~ y8
#'
#
#fit <- sem(model, data=PoliticalDemocracy)
#summary(fit, fit.measures=TRUE)
#
#
###no residual correlations
#modelNoRes <- ' 
#  # latent variable definitions
#     ind60 =~ x1 + x2 + x3
#     dem60 =~ y1 + y2 + y3 + y4
#     dem65 =~ y5 + equal("dem60=~y2")*y6 
#                 + equal("dem60=~y3")*y7 
#                 + equal("dem60=~y4")*y8
#
#  # regressions
#    dem60 ~ ind60
#    dem65 ~ ind60 + dem60
#
#'
#fitNoRes <- sem(modelNoRes, data=PoliticalDemocracy)
#
#aictab.lavaan(list(fitNoRes, fit), c("No Residual Covariances", "Residual Covariances"))