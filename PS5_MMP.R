#PS5 - Miguel Pereira

options(stringsAsFactors=F)
library(foreign)
library(plyr)
## read in data
anes <- read.dta("C:/Users/ststest/Dropbox/Spr16/Programming/HW5/PS5/anes_timeseries_2012_stata12.dta")

###I START BY CREATING CLEAN VARIABLES

######################
#demographic variables
######################
#Gender: gender_respondent_x
anes$female<-ifelse(anes$gender_respondent_x=="1. Male",1,0)

#Education: table(anes$dem_edu)
anes$educ<-as.numeric(substr(anes$dem_edu,1,2))
  #Substituting Refusals and Others with mean value of education (68 observations)
  anes$educ<-ifelse(anes$educ<0|anes$educ>90,round(mean(anes$educ,na.rm=TRUE),0),anes$educ)

#Owning house: dem3_ownhome
anes$own<-as.numeric(gsub("\\..*","",anes$dem3_ownhome))
  #Whoever gave an answer different from "own home" coded as "doesn't own home"
  anes$own[anes$own!=1]<-0

#Black: dem_racecps_black
anes$black<-as.numeric(gsub("\\..*","",anes$dem_racecps_black))

  
###############
#Political Vars
###############
#Obama thermometer
  #Refusals and Ns imputed with mean value
  anes$ft_dpc[anes$ft_dpc<0]<-round(mean(anes$ft_dpc[anes$ft_dpc>=0]),0)

#Clinton thermometer
  #Refusals and Ns imputed with mean value
  anes$ft_hclinton[anes$ft_hclinton<0]<-round(mean(anes$ft_hclinton[anes$ft_hclinton>=0]),0)

#George W Bush thermometer
  #Refusals and Ns imputed with mean value
  anes$ft_gwb[anes$ft_gwb<0]<-round(mean(anes$ft_gwb[anes$ft_gwb>=0]),0)
  
  
#Political interest: interest_attention
anes$pol_inattention<-as.numeric(gsub("\\..*","",anes$interest_attention))
  #DKs and Refusals imputed with mean value
  anes$pol_inattention<-ifelse(anes$pol_inattention<0,round(mean(anes$pol_inattention),0),anes$pol_inattention)

#Campaign interest: interest_following
#2008 turnout: interest_voted2008
anes$voted08<-as.numeric(gsub("\\..*","",anes$interest_voted2008))
  #DKs and Refusals as didn't vote
anes$voted08[anes$voted08!=1]<-0

#2008 vote: interest_whovote2008
  #Note: this assumes refusals did not vote Obama.
  anes$VoteObama08<-ifelse(as.character(anes$interest_whovote2008)=="1. Barack obama",1,0)



###########
#OTHER VARS
###########

#Attention to the news: prmedia_wkinews
anes$media_attention<-as.numeric(gsub("\\..*","",anes$prmedia_wkinews))
  #Refusals and DKs imputed with mean value
  anes$media_attention[anes$media_attention<0]<-round(mean(anes$media_attention[anes$media_attention>=0]),0)

  

  
########################
  ##############
  #######EXERCISE 1
  ##############
########################
  
    
##########################
#CREATING TRAIN/TEST SETS
##########################
set.seed(1)
train<-sort(sample(dim(anes)[1],dim(anes)[1]/2,replace=FALSE))
anes.train<-anes[train,]
anes.test<-anes[-train,]


#######################
#MODEL 1: Clinton and GW Bush's feeling thermometer predicting Obama's
#######################
  
## Obama's feeling thermometer score as function
## of Clinton's and Bush's feeling thermometer scores
model1 <- lm(ft_dpc ~ ft_hclinton+ft_gwb, anes.train)
#summary(model1)

## Model predicts fairly well

#######################
#MODEL 2: previous vote and media attention
#######################
model2<-lm(ft_dpc ~ voted08+VoteObama08+media_attention, anes.train)
#summary(model2)

## Previous turnout and vote choice do not predict Obama evaluations as well.

#######################
#MODEL 3: demographics predicting Obama's feeling thermometer. 
#Owning home used as proxy for income and interacted with black, since the effect of income can be exclusive to non-blacks
#######################
model3<-lm(ft_dpc ~ female + educ + own*black, anes.train)
#summary(model3)

## This model is the weakest of all three. Still, for a model exclusively based on demographic variables it works decently


########################
##############
#######EXERCISE 2
##############
########################

pred1 <- predict(model1, anes.test)
pred2 <- predict(model2, anes.test)
pred3 <- predict(model3, anes.test)



########################
##############
#######EXERCISES 3 and 4
##############
########################
#Training outside of the function
#e's
#test<-t(aaply(pred.matrix,2,function(x,y) abs(y-x), y=Y.test))
#a's
#test2<-t(aaply(test,2,function(x,y) x/abs(y+.01)*100, y=Y.test))


#' Fit statistics. 
#'
#' Calculates fit statistics a la carte using predictions from regression models. 
#'
#' @param Y A vector of true Yi values. 
#' @param preds A X by 3 matrix where each row corresponds to a predicted Yi value and each
#' column corresponds to a different regression model.
#' @param rmse Whether to compute the RMSE statistic or not. 
#' @param mad Whether to compute the MAD statistic or not. Defaults to T.  
#' @param rmsle Whether to compute the RMSLE statistic or not. Defaults to T. 
#' @param mape Whether to compute the MAPE statistic or not. Defaults to T. 
#' @param meape Whether to compute the MEAPE statistic or not. Defaults to T.
#'
#' @return A matrix with fit statistics for all three models added by the user.
#' @author Miguel Pereira.

#' @rdname fit.stats.flexible


fit.stats.flexible<-function(Y,preds, rmse=TRUE, mad=TRUE, rmsle=TRUE, mape=TRUE, meape=TRUE){
  #Creates matrix with abs error (each column corresponding to each prediction)
  e<-t(aaply(preds,2,function(x,y) abs(y-x), y=Y))
  #Creates matrix with abs percentage error  (each column corresponding to each prediction)
  #Since the outcome variable can be 0 I added .01 to make the calculation possible
  a<-t(aaply(e,2,function(x,y) x/abs(y+.01)*100, y=Y))
  
  #RMSE
  RMSE<-apply(e,2,function(x) sum(x^2)/length(x))
  #MAD
  MAD<-apply(e,2,median)
  #RMSLE
  #Here the function does not allow negative predited values so I transformed negative values into 0s
  new.preds<-ifelse(preds<0,0,preds)
  RMSLE<-apply(new.preds,2,function(x,y) sqrt( sum((log(x+1)-log(y+1))^2) / length(x)), y=(Y))
  #MAPE
  MAPE<-apply(a,2,function(x) sum(x)/length(x))
  #MEAPE
  MEAPE<-apply(a,2,median)
  
  #TO PRINT
  #This is the full matrix with outputs
  output<-cbind(RMSE,MAD,RMSLE,MAPE,MEAPE)
  
  #Here I identify which stats the user wants to see
  #Lower case stats are the arguments of the function
  stats.to.print<-c(rmse,mad,rmsle,mape,meape)*1
  
  #Finally, printing
  ifelse (stats.to.print > 0, return(output[,which(stats.to.print==1)]), 
          return("If you don't feel like getting some fit statistics, that's fine with me."))
}


############
#Now testing
############
pred.matrix<-as.matrix(cbind(Model1=pred1,Model2=pred2,Model3=pred3))
Y.test<-anes.test$ft_dpc

  #Here the full thing
  fit.stats.flexible(Y.test,pred.matrix)


  #Here without the first one
  fit.stats.flexible(Y.test,pred.matrix,rmse=F)

  #Here only with measure starting with M
  fit.stats.flexible(Y.test,pred.matrix,rmse=F,rmsle=F)


  #Here with just with MEAPE
  fit.stats.flexible(Y.test,pred.matrix,rmse=F,rmsle=F, mad=F, mape=F)
  
  #Here what happens when you are too picky
  fit.stats.flexible(Y.test,pred.matrix,rmse=F,rmsle=F, mad=F, mape=F, meape=F)


########################
##############
#######EXERCISE 5
##############
########################
fit.stats.flexible(Y.test,pred.matrix)

#The first models has the smaller values in all five statistics, followed by the second and the third models.


########################
##############
#######EXERCISE 6
##############
########################

#' More fit statistics. 
#'
#' Calculates fit statistics using predictions from regression models, and allowing for the inclusion of naive predictions. 
#'
#' @param Y A vector of true Yi values. 
#' @param preds A X by 3 matrix where each row corresponds to a predicted Yi value and each
#' column corresponds to a different regression model.
#' @param rmse Whether to compute the RMSE statistic or not. 
#' @param mad Whether to compute the MAD statistic or not. Defaults to T.  
#' @param rmsle Whether to compute the RMSLE statistic or not. Defaults to T. 
#' @param mape Whether to compute the MAPE statistic or not. Defaults to T. 
#' @param meape Whether to compute the MEAPE statistic or not. Defaults to T.
#' @param mrae Whether to compute the MRAE statistic or not. Defaults to F.
#' @param R A vector of naive forecasts
#'
#' @return A matrix with fit statistics for all three models added by the user.
#' @author Miguel Pereira.

#' @rdname fit.stats.extra

  #Note, if the user does not add a vector of naive forecasts, the function randomly generates one.

fit.stats.extra<-function(Y,preds, R=lm(Y~rnorm(length(Y),50,4))$fitted,
                          rmse=TRUE, mad=TRUE, rmsle=TRUE, mape=TRUE, meape=TRUE, mrae=FALSE){
  #Creates matrix with abs error (each column corresponding to each prediction)
  e<-t(aaply(preds,2,function(x,y) abs(y-x), y=Y))
  #Creates matrix with abs percentage error  (each column corresponding to each prediction)
  #Since the outcome variable can be 0 I added .01 to make the calculation possible
  a<-t(aaply(e,2,function(x,y) x/abs(y+.01)*100, y=Y))
  
  #RMSE
  RMSE<-apply(e,2,function(x) sum(x^2)/length(x))
  #MAD
  MAD<-apply(e,2,median)
  #RMSLE
  #Here the function does not allow negative predited values so I transformed negative values into 0s
  new.preds<-ifelse(preds<0,0,preds)
  RMSLE<-apply(new.preds,2,function(x,y) sqrt( sum((log(x+1)-log(y+1))^2) / length(x)), y=(Y))
  #MAPE
  MAPE<-apply(a,2,function(x) sum(x)/length(x))
  #MEAPE
  MEAPE<-apply(a,2,median)
  #MRAE
  b<-abs(R-Y)
  MRAE<-apply(e,2,function(x,y) median(x/y), y=b)
  
  #TO PRINT
  #This is the full matrix with outputs
  output<-cbind(RMSE,MAD,RMSLE,MAPE,MEAPE,MRAE)
  
  #Here I identify which stats the user wants to see
  #Lower case stats are the arguments of the function
  stats.to.print<-c(rmse,mad,rmsle,mape,meape,mrae)*1
  
  #Finally, printing
  ifelse (stats.to.print > 0, return(output[,which(stats.to.print==1)]), 
          return("If you don't feel like getting some fit statistics, that's fine with me."))
}

#With the default naive estimator
fit.stats.extra(Y.test,pred.matrix, mrae=T)

#With an absurd forecast
nv<-runif(length(Y.test),0,100)
fit.stats.extra(Y.test,pred.matrix,R=nv, mrae=T)