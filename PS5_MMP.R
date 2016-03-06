#PS5 - Miguel Pereira

options(stringsAsFactors=F)
library(foreign)
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


#######################
#MODEL 2: demographics predicting Obama's feeling thermometer. 
#Owning home used as proxy for income and interacted with black, since the effect of income can be exclusive to non-blacks
#######################
model2<-lm(ft_dpc ~ female + educ + own*black, anes.train)
summary(model2)

#######################
#MODEL 3: previous vote and media attention
#######################
model3<-lm(ft_dpc ~ voted08+VoteObama08+media_attention, anes.train)
summary(model3)


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
#######EXERCISE 3
##############
########################
library(plyr)

pred.matrix<-as.matrix(cbind(pred1,pred2,pred3))
Y.test<-anes.test$ft_dpc
?apply
test<-t(aaply(pred.matrix,2,function(x,y) abs(y-x), y=Y.test))

a=e/abs(y)

test2<-



fit.stats<-function(Y,preds){
  #Creates matrix with 
  e<-t(aaply(preds,2,function(x,y) abs(y-x), y=Y))
  a<-t(aaply(e,2,function(x,y) x/abs(y+.01)*100, y=Y))
  
  #RMSE
  #MAD
  #RMSLE
  #MAPE
  #MEAPE
}


