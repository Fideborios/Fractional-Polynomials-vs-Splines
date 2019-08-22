## ---- echo=FALSE---------------------------------------------------------
library(knitr)
## General output of the chunks 
opts_chunk$set(fig.path='Figs/',                # The path where to save the figures
               echo=F,                          # Logical whether the chunck will be printed 
               warning=FALSE,                   # Logical whether the warnings will be printed
               message=FALSE,                   # Logical whether the messages will be printed
               fig.pos = " ",                   # The position of the 
               comment = "",                     #
               fig.height=8, 
               fig.width=9 
               )

options(knitr.table.format = "html")


Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Program Files/MiKTeX 2.9/miktex/bin/x64/",sep=";"))
Sys.setenv(R_GSCMD = "/usr/local/bin/gs")


## ----global_options , echo=FALSE, message=FALSE, warning= FALSE----------
rm(list=ls()) ### To clear namespace

# Libraries for loading and saving data
## Load data-set for loading xlsx data-sets
if(!require("readr")) install.packages("readr")
## Load readxl for loading xlsx data-sets
if(!require("readxl")) install.packages("readxl")
## Load haven for loading sas data-sets 
if(!require("haven")) install.packages("haven")

################################################
# Libraries for plotting
## Load ggpubr for plotting
if(!require("ggpubr")) install.packages("ggpubr") # automatically loads the ggplot2
## Load gridExtra  to arrange multiple grid-based plots on a page, and draw tables
if(!require("gridExtra")) install.packages("gridExtra")
## Load ggsci for better looking colors
if(!require("ggsci")) install.packages("ggsci")
## Load sjPlot to get easy statistical summaries and plots
if(!require("sjPlot")) install.packages("sjPlot")
## Load itsadug to have nice plots of GAMMs
if(!require("itsadug")) install.packages("itsadug")
## Load magick for graphics and image processing in R
if(!require("magick")) install.packages("magick")
## Load webshot for tranforming HTML objects into pictures
if(!require("webshot")) install.packages("webshot")
## Load lme4 for lme4
if(!require("pander")) install.packages("pander")
## Load kableExtra for better looking kable objects
if(!require("kableExtra")) install.packages("kableExtra") # automatically loads kable

################################################
# Libraries for data manipulation
## Load knitr for fine tuning
if(!require("knitr")) install.packages("knitr")
## Load dplyr for data manipulation
if(!require("dplyr")) install.packages("dplyr")
## Load tidyr for data manipulation
if(!require("tidyr")) install.packages("tidyr")
## Load broom for data manipulation
if(!require("broom")) install.packages("broom")
## Load tidyverse for data manipulation
if(!require("tidyverse")) install.packages("tidyverse")



################################################
# Libraries for the statistical analysis 
## Load lme4 for lme4
if(!require("lme4")) install.packages("lme4")
## Load lmerTest to add information into the summaries  
if(!require("lmerTest")) install.packages("lmerTest")
## Load mcgv to fit gamms and bamm
if(!require("mgcv")) install.packages("mgcv")
if(!require("splines")) install.packages("splines")
library(gamm4)
library(meta)
library(mvmeta)

################################################



## General output of the chunks 
opts_chunk$set(fig.path='Figs/',                # The path where to save the figures
               echo=F,                          # Logical whether the chunck will be printed 
               warning=FALSE,                   # Logical whether the warnings will be printed
               message=FALSE,                   # Logical whether the messages will be printed
               fig.pos = " ",                   # The position of the 
               comment = ""                     #
               )




## ----Simulating the datasets---------------------------------------------
set.seed(25)

df = data.frame(BMI= rep(seq(18.5,to = 40, length.out = 2500),each= 2),
                Treat =  rep(c(0,1),2500), 
                Study = sample(1:5, size = 1000, replace = T), 
                Study2= c(rep(1,750), sample(1:2,replace = T,size = 500), 
          rep(2,500), sample(2:3,replace = T, size = 500),
          rep(3,500), sample(3:4,replace = T, size = 500),
          rep(4,500), sample(4:5,replace = T, size = 500),
          rep(5,750)))

df$BMI.standardised =  with(df, 2*(BMI-25)/40)
df$`Mortality risk` = NA
df$`Mortality risk 2` = NA
df$Study.noise.intercept =  df$Study
df$Study.noise.slope =  df$Study
noise =  round(runif(5, -0.05,0.05),2)
noise2 = round(runif(5, -0.05,0.05),2)

df=df%>%
  mutate(Study.noise.intercept=recode(Study.noise.intercept,
                '1' = noise[1],
                '2' = noise[2],
                '3' = noise[3],
                '4' = noise[4],
                '5'= noise[5]  ), 
         Study.noise.slope = recode(Study.noise.slope,
                '1' = noise2[1],
                '2' = noise2[2],
                '3' = noise2[3],
                '4' = noise2[4],
                '5'= noise2[5]  ))
df =  df%>%
  mutate(Study = recode(Study, 
                '1' = "1st Study",
                '2' = "2nd Study",
                '3' = "3rd Study",
                '4' = "4th Study",
                '5'= "5th Study"  ),
         Study2 = recode(Study2, 
                '1' = "1st Study",
                '2' = "2nd Study",
                '3' = "3rd Study",
                '4' = "4th Study",
                '5'= "5th Study"  ))

df[df$Treat==0,]$`Mortality risk` =  with(df[df$Treat==0,], 0.2+BMI.standardised^2 + Study.noise.intercept)
df[df$Treat==1,]$`Mortality risk` =  with(df[df$Treat==1,], 0.2+ Study.noise.intercept + BMI.standardised^4+ Study.noise.slope)
df$Y <-rbinom(5000,1,df$`Mortality risk`)  

df[df$Treat==0,]$`Mortality risk 2` =  with(df[df$Treat==0,], 0.2+BMI.standardised^2 )
df[df$Treat==1,]$`Mortality risk 2` =  with(df[df$Treat==1,], 0.2+ BMI.standardised^4)
df$Y <-rbinom(5000,1,df$`Mortality risk`) 
df$Y2 <-rbinom(5000,1,df$`Mortality risk 2`) 

df$Treatment =  factor(df$Treat, levels = c(0,1), labels = c("Control","Treated"))

df = df %>% mutate(Study = factor(Study, levels = c("1st Study","2nd Study",
                                             "3rd Study","4th Study", "5th Study" )))
df = df %>% mutate(Study2 = factor(Study2, levels = c("1st Study","2nd Study",
                                             "3rd Study","4th Study", "5th Study" )))

df$dum <- rep(1,dim(df)[1])
source(file = "Functions/help functions.R")
rm(noise,noise2)


## ------------------------------------------------------------------------
set.seed(1703)
fig_cap <- captioner::captioner()
tab_cap <- captioner::captioner("Table")


## ----eval=FALSE----------------------------------------------------------
df%>%
    group_by(Study2)%>%
    rename(Studies =  Study2)%>%
    summarise("Minimum BMI" =round(min(BMI)), `Maximum BMI`= round(max(BMI)))%>%
    kable(format = "html",caption = "BMI characteristics per study",align = "c")%>%
    kable_styling()


## ----fig.height=8, fig.width=9-------------------------------------------

ggplot(df, aes(x = BMI, `Mortality risk`, linetype= Treatment, color= Treatment)) + 
  geom_line() + facet_wrap(.~Study, dir="v")+ ylab("Mortality risk") + 
  scale_color_jama()+
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = c(1, 0), legend.justification = c(1, 0.25)) 


## ----fig.height=8, fig.width=9-------------------------------------------
df %>%
  bind_rows(df %>% mutate(Study2 = "Studies combined"))%>%
ggplot(aes(x = BMI, `Mortality risk 2`, linetype= Treatment, color= Treatment)) + 
  geom_line() + facet_wrap(fct_relevel(Study2, "total", after = Inf)~., ncol = 2, dir="v")+ ylab("Mortality risk") + scale_color_jama()+ 
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Script for B-splines (cubic) Heterogeneous Data---------------------

 ### To clear all enviroment besides the data-set

### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation

Knots =   c(min(df$BMI),20,25,30,35,max(df$BMI))
formula = Y~ bs(x = BMI,knots = Knots,intercept = T,degree = 3, Boundary.knots = c(min(df$BMI),max(df$BMI)))*Treatment

fit = gam( formula =formula , family = binomial("logit"), data = df)

#####
#   ggplot(df, aes(BMI, Y, linetype= Treatment)) + geom_point()+
#    geom_line(aes(y = expit(predict(fit)))) + 
#    geom_vline(xintercept = fit$smooth[[1]]$knots,linetype =2, show.legend = F)  
#        

### Create an empty data frame for the fitted values of the gam
predicted.values = data.frame(   BMI= rep(rep(seq(18.5,40, length.out = 1000),each=2)),
                                 Treatment = rep(rep(rep(levels(df$Treatment),500),2)),
                                 matrix(NA, nrow = 2000,ncol=10,dimnames = list(NULL, apply(expand.grid(c("fit","se.fit"),c(1:5)),1, paste, collapse=""))))
### Create an empty matrix for the estimated splines coefficients

estimated.coefficients = matrix(NA,ncol = length(fit$coefficients),nrow=5,dimnames = list( apply(expand.grid(c("Study"),c(1:5)),1, paste, collapse=" "),
c(1:length(fit$coefficients))))

### Create an empty matrix for the variance-covariance matrix of the coefficients

S = matrix(NA, ncol=sum(c(1:length(fit$coefficients))), nrow = 5 )


k=3
j=1
for( i in c("1st Study","2nd Study","3rd Study","4th Study", "5th Study" )){

minidf = df%>%
    filter(Study == i)

# Fit the GAM

fit = gam(formula =formula , family = binomial("logit"), data = minidf)

## Extract the fitted lines
new.data = predicted.values[,1:2]

temp.pred.df = data.frame(predict.gam(fit,se.fit = T,newdata.guaranteed = T,newdata = new.data))

### Store the fitted values with their 95% CI in the data.frame

predicted.values[,k] =  temp.pred.df[,1]
predicted.values[,k+1] =  temp.pred.df[,2]


### Extract the coefficients and their standard errors for mvmeta
estimated.coefficients[j,] = fit$coefficients
S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]

#mat1 <- predict.gam(fit, type = "lpmatrix")

k=k+2
j=j+1
rm(i,minidf,temp.pred.df,fit)
}
rm(k,j)

#### Multi-variate meta-analysis
fit = gam(formula =formula  , family = binomial("logit"), data = df)

Xp =  predict.gam(fit, type="lpmatrix",se.fit = T)

mv.fit = mvmeta(estimated.coefficients, S)



mv.vcov =  as.data.frame(cbind(Lower = coef(mv.fit)-qt(0.975,df = Inf) *sqrt(diag(vcov(mv.fit))),
                 Upper = coef(mv.fit)+qt(0.975,df = Inf) *sqrt(diag(vcov(mv.fit)))))

prediction.interval.mvmeta =  Xp%*% coef(mv.fit)
prediction.interval.mvmeta.lower =  Xp%*%mv.vcov$Lower
prediction.interval.mvmeta.upper =  Xp%*% mv.vcov$Upper

mvmeta.df = cbind(df[,c("Study","BMI","Treatment")],
                  fit =  prediction.interval.mvmeta, 
                  Lower= prediction.interval.mvmeta.lower,
                  Upper =prediction.interval.mvmeta.upper )

#### Point-wise meta-analysis
#### Here we meta-analyse the fitted lines

#### We generate 2 new columns to accept the Pooled estimates
predicted.values$Overall_fit = NA
predicted.values$Overall_se.fit =  NA

for(i in 1:dim(predicted.values)[1]){
  minidf =  predicted.values[i,]
  betas =  t(predicted.values[i,]%>%
    select(-contains("."))%>%
    select(-contains("Overall"))%>%
    select(contains("fit")))
  se.betas =  t(predicted.values[i,]%>%
                      select(-contains("Overall"))%>%
                  select(contains("se.fit")))
  
  
  meta = metagen(TE = betas, seTE = se.betas, hakn = T, method.tau = "REML")
  predicted.values[i,]$Overall_fit = meta$TE.random
  predicted.values[i,]$Overall_se.fit =  meta$seTE.random
  # print(i) for controlling erroneous meta-analyses
  rm(i)
}




#### Create a long object that contains the predicted lines with their standard error
long.predicted.values = predicted.values%>%
  select(-contains("se.fit"))%>%
  gather(key ,value= fit, -c("BMI","Treatment" ))
### Save the predicted standard errors in the long object
long.predicted.values$se.fit =
  as.vector(predicted.values%>%
  select(contains("se.fit"))%>%
  gather(key ,value= se.fit)%>%
  select(contains("se.fit")))
### Recode the fit values to Study IDs
  long.predicted.values =  long.predicted.values%>%
  mutate(Study=recode(key,
                'fit1' = "1st Study",
                'fit2' = "2nd Study",
                'fit3' = "3rd Study",
                'fit4' = "4th Study",
                'fit5'= "5th Study",
                'Overall_fit'= "Pooled fit"))

### Calculate the CIs in log-scale using 1.96*standard errors 
  
  long.predicted.values$Lower = long.predicted.values$fit - 1.96*long.predicted.values$se.fit$se.fit
  long.predicted.values$Upper = long.predicted.values$fit + 1.96*long.predicted.values$se.fit$se.fit
  
### Back transform to absolute risk the logit transformed predicted values  
  long.predicted.values$Lower =  expit(long.predicted.values$Lower)
  long.predicted.values$Upper =  expit(long.predicted.values$Upper)
  long.predicted.values$probability =  expit(long.predicted.values$fit)

  



## ----Plot of multi-variate meta-analysis B-splines (cubic) Heterogeneous Data----
ggplot(mvmeta.df,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line()+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 



## ----Plot of point-wise meta-analysis Basis splines (cubic) Heterogeneous Data----
### Plot the fitted lines per study and Pooled
long.predicted.values[long.predicted.values$Study == "Pooled fit",]%>%
ggplot(aes(x = BMI, probability, linetype= Treatment, color= Treatment)) + 
  geom_line() +
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Script for Basis splines (cubic) different range Data---------------

 ### To clear all enviroment besides the data-set

### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation

Knots =  seq(min(df$BMI),max(df$BMI),length.out = 3)

formula =  Y2~ bs(x = BMI,knots = Knots,intercept = T,degree = 3,Boundary.knots = c(min(df$BMI),max(df$BMI)))*Treatment

# Fit on the full data a GAM to get lengths for coefficients and variance-covariance matrices

fit = gam(formula = formula, family = binomial("logit"), data = df)

### Create an empty data frame for the fitted values of the gam
predicted.values = data.frame(   df[,c("BMI","Treatment")],
                                 matrix(NA, nrow = 5000,ncol=10,dimnames = list(NULL, apply(expand.grid(c("fit","se.fit"),c(1:5)),1, paste, collapse=""))))
### Create an empty matrix for the estimated splines coefficients

estimated.coefficients = matrix(NA,ncol = length(fit$coefficients),nrow=5,dimnames = list( apply(expand.grid(c("Study"),c(1:5)),1, paste, collapse=" "),
c(1:length(fit$coefficients))))

### Create an empty matrix for the variance-covariance matrix of the coefficients

S = matrix(NA, ncol=sum(c(1:length(fit$coefficients))), nrow = 5 )


k=3
j=1
i ="5th Study"
for( i in c("1st Study","2nd Study","3rd Study","4th Study", "5th Study" )){

### Load the different range data-sets
minidf = df%>%
    filter(Study2 == i)

# Fit the GAM
Knots =  seq(min(minidf$BMI),max(minidf$BMI),length.out = 3)
fit = gam(formula = formula , family = binomial("logit"), data = minidf)

## Extract the fitted lines
new.data = predicted.values[,1:2]

### Here I introduce a some lag points to handle smoothness over the non-overlapping Studies

lag.points = 0.5
new.data = new.data[which(max(minidf$BMI)+lag.points>=new.data$BMI & new.data$BMI  >= min(minidf$BMI )-lag.points),]


temp.pred.df = data.frame(predict.gam(fit,se.fit = T,newdata.guaranteed = T,newdata = new.data))

### Store the fitted values with their 95% CI in the data.frame

predicted.values[which(max(minidf$BMI)+lag.points>=predicted.values$BMI & predicted.values$BMI  >= min(minidf$BMI )-lag.points),k] =  temp.pred.df[,1]
predicted.values[which(max(minidf$BMI)+lag.points>=predicted.values$BMI & predicted.values$BMI  >= min(minidf$BMI )-lag.points),k+1] =  temp.pred.df[,2]


### Extract the coefficients and their standard errors for mvmeta
estimated.coefficients[j,] = fit$coefficients
S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]

#mat1 <- predict.gam(fit, type = "lpmatrix")

k=k+2
j=j+1
rm(i,minidf,temp.pred.df,fit)
}
rm(k,j)

#### Multi-variate meta-analysis
fit = gam(formula = formula , family = binomial("logit"), data = df)

Xp =  predict.gam(fit, type="lpmatrix",se.fit = T)

mv.fit = mvmeta(estimated.coefficients, S)



mv.vcov =  as.data.frame(cbind(Lower = coef(mv.fit)-qt(0.975,df = Inf) *sqrt(diag(vcov(mv.fit))),
                 Upper = coef(mv.fit)+qt(0.975,df = Inf) *sqrt(diag(vcov(mv.fit)))))

prediction.interval.mvmeta =  Xp%*% coef(mv.fit)
prediction.interval.mvmeta.lower =  Xp%*%mv.vcov$Lower
prediction.interval.mvmeta.upper =  Xp%*% mv.vcov$Upper

mvmeta.df = cbind(df[,c("Study","BMI","Treatment")],
                  fit =  prediction.interval.mvmeta, 
                  Lower= prediction.interval.mvmeta.lower,
                  Upper =prediction.interval.mvmeta.upper )


#### Point-wise meta-analysis
#### Here we meta-analyse the fitted lines

#### We generate 2 new columns to accept the Pooled estimates
predicted.values$Overall_fit = NA
predicted.values$Overall_se.fit =  NA

for(i in 1:dim(predicted.values)[1]){
  minidf =  predicted.values[i,]
  betas =  t(predicted.values[i,]%>%
    select(-contains("."))%>%
    select(-contains("Overall"))%>%
    select(contains("fit")))
  se.betas =  t(predicted.values[i,]%>%
                      select(-contains("Overall"))%>%
                  select(contains("se.fit")))
  
  
  meta = metagen(TE = betas, seTE = se.betas, hakn = T, method.tau = "REML",warn = F)
  predicted.values[i,]$Overall_fit = meta$TE.random
  predicted.values[i,]$Overall_se.fit =  meta$seTE.random
  # print(i) # for controlling erroneous meta-analyses
  
  rm(i)
}

#### Create a long object that contains the predicted lines with their standard error
long.predicted.values = predicted.values%>%
  select(-contains("se.fit"))%>%
  gather(key ,value= fit, -c("BMI","Treatment" ))
### Save the predicted standard errors in the long object
long.predicted.values$se.fit =
  as.vector(predicted.values%>%
  select(contains("se.fit"))%>%
  gather(key ,value= se.fit)%>%
  select(contains("se.fit")))
### Recode the fit values to Study IDs
  long.predicted.values =  long.predicted.values%>%
  mutate(Study2=recode(key,
                'fit1' = "1st Study",
                'fit2' = "2nd Study",
                'fit3' = "3rd Study",
                'fit4' = "4th Study",
                'fit5'= "5th Study",
                'Overall_fit'= "Pooled fit"))
### Calculate the CIs in log-scale using 1.96*standard errors 
  
  long.predicted.values$Lower = long.predicted.values$fit - 1.96*long.predicted.values$se.fit$se.fit
  long.predicted.values$Upper = long.predicted.values$fit + 1.96*long.predicted.values$se.fit$se.fit
  
### Back transform to absolute risk the logit transformed predicted values  
  long.predicted.values$Lower =  expit(long.predicted.values$Lower)
  long.predicted.values$Upper =  expit(long.predicted.values$Upper)
  long.predicted.values$probability =  expit(long.predicted.values$fit)
  



## ----Plot of multi-variate meta-analysis B-splines (cubic) different range Data----

ggplot(mvmeta.df,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line()+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 



## ----Plot of point-wise meta-analysis B-splines(cubic) different range Data----
### Plot the fitted lines per study and Pooled
long.predicted.values[long.predicted.values$Study == "Pooled fit",]%>%
ggplot(aes(x = BMI, probability, linetype= Treatment, color= Treatment)) + 
  geom_line() +
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Script for smoothing splines heterogeneous data---------------------
 ### To clear all enviroment besides the data-set
source("Functions/help functions.R")
### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation

fit = gam(Y ~ s(BMI,bs="tp",by = Treatment),
          family = binomial("logit"), data = df, nthreads = 8, method = "GCV.Cp")

#ggplot(df, aes(BMI, Y, linetype= Treatment)) + geom_point()+
#    geom_line(aes(y = expit(predict(fit))))



### Create an empty data frame for the fitted values of the gam
predicted.values = data.frame(   BMI= rep(rep(seq(18.5,40, length.out = 1000),each=2)),
                                 Treatment = rep(rep(rep(levels(df$Treatment),500),2)),
                                 matrix(NA, nrow = 2000,ncol=10,dimnames = list(NULL, apply(expand.grid(c("fit","se.fit"),c(1:5)),1, paste, collapse=""))))

### Create an empty matrix for the estimated splines coefficients

estimated.coefficients = matrix(NA,ncol = length(fit$coefficients),nrow=5,dimnames = list( apply(expand.grid(c("Study"),c(1:5)),1, paste, collapse=" "),
c(1:length(fit$coefficients))))

### Create an empty matrix for the variance-covariance matrix of the coefficients

S = matrix(NA, ncol=sum(c(1:length(fit$coefficients))), nrow = 5 )


k=3
j=1
for( i in c("1st Study","2nd Study","3rd Study","4th Study", "5th Study" )){

minidf = df%>%
    filter(Study == i)

# Fit the GAM

fit = gam(Y ~ s(BMI,bs="tp",by = Treatment),
          family = binomial("logit"), data = minidf, nthreads = 8, method = "GCV.Cp")

## Extract the fitted lines

temp.pred.df = data.frame(predict(fit,se.fit = T, 
                                  newdata = data.frame(BMI= rep(seq(18.5,40, length.out = 1000),each=2),
                                                       Treatment = rep(rep(levels(df$Treatment),500),2))))

### Store the fitted values with their 95% CI in the data.frame

predicted.values[,k] =  temp.pred.df[,1]
predicted.values[,k+1] =  temp.pred.df[,2]


### Extract the coefficients and their standard errors for mvmeta
estimated.coefficients[j,] = fit$coefficients
S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]


#mat1 <- predict.gam(fit, type = "lpmatrix")

k=k+2
j=j+1
rm(i,minidf,temp.pred.df,fit)
}
rm(k,j)

#### Point-wise meta-analysis
#### Here we meta-analyse the fitted lines

#### We generate 2 new columns to accept the Pooled estimates
predicted.values$Overall_fit = NA
predicted.values$Overall_se.fit =  NA

for(i in 1:dim(predicted.values)[1]){
  minidf =  predicted.values[i,]
  betas =  t(predicted.values[i,]%>%
    select(-contains("."))%>%
    select(-contains("Overall"))%>%
    select(contains("fit")))
  se.betas =  t(predicted.values[i,]%>%
                      select(-contains("Overall"))%>%
                  select(contains("se.fit")))
  
  
  meta = metagen(TE = betas, seTE = se.betas, hakn = T, method.tau = "REML")
  predicted.values[i,]$Overall_fit = meta$TE.random
  predicted.values[i,]$Overall_se.fit =  meta$seTE.random
  # print(i) for controlling erroneous meta-analyses
  rm(i)
}




#### Create a long object that contains the predicted lines with their standard error
long.predicted.values = predicted.values%>%
  select(-contains("se.fit"))%>%
  gather(key ,value= fit, -c("BMI","Treatment" ))
### Save the predicted standard errors in the long object
long.predicted.values$se.fit =
  as.vector(predicted.values%>%
  select(contains("se.fit"))%>%
  gather(key ,value= se.fit)%>%
  select(contains("se.fit")))
### Recode the fit values to Study IDs
  long.predicted.values =  long.predicted.values%>%
  mutate(Study=recode(key,
                'fit1' = "1st Study",
                'fit2' = "2nd Study",
                'fit3' = "3rd Study",
                'fit4' = "4th Study",
                'fit5'= "5th Study",
                'Overall_fit'= "Pooled fit"))
### Load some help function for expit transformation


  
### Calculate the CIs in log-scale using 1.96*standard errors 
  
  long.predicted.values$Lower = long.predicted.values$fit - 1.96*long.predicted.values$se.fit$se.fit
  long.predicted.values$Upper = long.predicted.values$fit + 1.96*long.predicted.values$se.fit$se.fit
  
### Back transform to absolute risk the logit transformed predicted values  
  long.predicted.values$Lower =  expit(long.predicted.values$Lower)
  long.predicted.values$Upper =  expit(long.predicted.values$Upper)
  long.predicted.values$probability =  expit(long.predicted.values$fit)



## ----Plot of point-wise meta-analysis using smoothing splines heterogeneous data----
### Plot the fitted lines per study and Pooled
long.predicted.values[long.predicted.values$Study == "Pooled fit",]%>%
ggplot(aes(x = BMI, probability, linetype= Treatment, color= Treatment)) + 
  geom_line() +
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Script for smoothing splines different range Data-------------------

 ### To clear all enviroment besides the data-set

### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation

formula = Y2 ~ s(BMI,bs="tp",by = Treatment)
fit = gam(formula =formula ,
          family = binomial("logit"), data = df, nthreads = 8, method = "GCV.Cp")


### Create an empty data frame for the fitted values of the gam
predicted.values = data.frame(   df[,c("BMI","Treatment")],
                                 matrix(NA, nrow = 5000,ncol=10,dimnames = list(NULL, apply(expand.grid(c("fit","se.fit"),c(1:5)),1, paste, collapse=""))))
### Create an empty matrix for the estimated splines coefficients

estimated.coefficients = matrix(NA,ncol = length(fit$coefficients),nrow=5,dimnames = list( apply(expand.grid(c("Study"),c(1:5)),1, paste, collapse=" "),
c(1:length(fit$coefficients))))

### Create an empty matrix for the variance-covariance matrix of the coefficients

S = matrix(NA, ncol=sum(c(1:length(fit$coefficients))), nrow = 5 )


k=3
j=1
i = "1st Study"
for( i in c("1st Study","2nd Study","3rd Study","4th Study", "5th Study" )){

minidf = df%>%
    filter(Study2 == i)

# Fit the GAM

fit = gam(formula =formula ,
          family = binomial("logit"), data = minidf, nthreads = 8, method = "GCV.Cp")

## Extract the fitted lines
new.data = predicted.values[,1:2]

### Here I introduce a some lag points to handle smoothness over the non-overlapping Studies

lag.points = 0.5
new.data = new.data[which(max(minidf$BMI)+lag.points>=new.data$BMI & new.data$BMI  >= min(minidf$BMI )-lag.points),]


temp.pred.df = data.frame(predict.gam(fit,se.fit = T,newdata.guaranteed = T,newdata = new.data))

### Store the fitted values with their 95% CI in the data.frame

predicted.values[which(max(minidf$BMI)+lag.points>=predicted.values$BMI & predicted.values$BMI  >= min(minidf$BMI )-lag.points),k] =  temp.pred.df[,1]
predicted.values[which(max(minidf$BMI)+lag.points>=predicted.values$BMI & predicted.values$BMI  >= min(minidf$BMI )-lag.points),k+1] =  temp.pred.df[,2]


### Extract the coefficients and their standard errors for mvmeta
estimated.coefficients[j,] = fit$coefficients
S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]

#mat1 <- predict.gam(fit, type = "lpmatrix")

k=k+2
j=j+1
rm(i,minidf,temp.pred.df,fit)
}
rm(k,j)



#### Point-wise meta-analysis
#### Here we meta-analyse the fitted lines
#### We generate 2 new columns to accept the Pooled estimates
predicted.values$Overall_fit = NA
predicted.values$Overall_se.fit =  NA

for(i in 1:dim(predicted.values)[1]){
  minidf =  predicted.values[i,]
  betas =  t(predicted.values[i,]%>%
    select(-contains("."))%>%
    select(-contains("Overall"))%>%
    select(contains("fit")))
  se.betas =  t(predicted.values[i,]%>%
                      select(-contains("Overall"))%>%
                  select(contains("se.fit")))
  
  
  meta = metagen(TE = betas, seTE = se.betas, hakn = T, method.tau = "REML")
  predicted.values[i,]$Overall_fit = meta$TE.random
  predicted.values[i,]$Overall_se.fit =  meta$seTE.random
  # print(i) for controlling erroneous meta-analyses
  rm(i)
}




#### Create a long object that contains the predicted lines with their standard error
long.predicted.values = predicted.values%>%
  select(-contains("se.fit"))%>%
  gather(key ,value= fit, -c("BMI","Treatment" ))
### Save the predicted standard errors in the long object
long.predicted.values$se.fit =
  as.vector(predicted.values%>%
  select(contains("se.fit"))%>%
  gather(key ,value= se.fit)%>%
  select(contains("se.fit")))
### Recode the fit values to Study IDs
  long.predicted.values =  long.predicted.values%>%
  mutate(Study2=recode(key,
                'fit1' = "1st Study",
                'fit2' = "2nd Study",
                'fit3' = "3rd Study",
                'fit4' = "4th Study",
                'fit5'= "5th Study",
                'Overall_fit'= "Pooled fit"))
### Calculate the CIs in log-scale using 1.96*standard errors 
  
  long.predicted.values$Lower = long.predicted.values$fit - 1.96*long.predicted.values$se.fit$se.fit
  long.predicted.values$Upper = long.predicted.values$fit + 1.96*long.predicted.values$se.fit$se.fit
  
### Back transform to absolute risk the logit transformed predicted values  
  long.predicted.values$Lower =  expit(long.predicted.values$Lower)
  long.predicted.values$Upper =  expit(long.predicted.values$Upper)
  long.predicted.values$probability =  expit(long.predicted.values$fit)



## ----Plot of point-wise meta-analysis smoothing splines different range Data----
 ### Plot the fitted lines per study and Pooled
long.predicted.values[long.predicted.values$Study == "Pooled fit",]%>%
ggplot(aes(x = BMI, probability, linetype= Treatment, color= Treatment)) + 
  geom_line() +
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Script for P-splines splines heterogeneous data---------------------

#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set

### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation
Knots =   c(min(df$BMI),20,25,30,35,max(df$BMI))
Knots =   list(BMI= c(seq(min(df$BMI)-1,min(df$BMI),length.out = 5), Knots[-c(1,6)],seq(max(df$BMI),max(df$BMI) + 1,length.out = 5)) )
formula = Y~ BMI + s(BMI,bs = "ps",by = Treatment,fx = T,k = 10,m = c(2,2))
fit = gam(formula =formula,
          gamma =1,knots =  Knots,
          family = binomial("logit"), data = df)

#ggplot(df, aes(BMI, Y, linetype= Treatment)) + geom_point()+
#    geom_line(aes(y = expit(predict(fit)))) + 
#    geom_vline(xintercept = fit$smooth[[1]]$knots,linetype =2, show.legend = F)


### Create an empty data frame for the fitted values of the gam
predicted.values = data.frame(   BMI= rep(rep(seq(18.5,40, length.out = 1000),each=2)),
                                 Treatment = rep(rep(rep(levels(df$Treatment),500),2)),
                                 matrix(NA, nrow = 2000,ncol=10,dimnames = list(NULL, apply(expand.grid(c("fit","se.fit"),c(1:5)),1, paste, collapse=""))))

### Create an empty matrix for the estimated splines coefficients

estimated.coefficients = matrix(NA,ncol = length(fit$coefficients),nrow=5,dimnames = list( apply(expand.grid(c("Study"),c(1:5)),1, paste, collapse=" "),
c(1:length(fit$coefficients))))

### Create an empty matrix for the variance-covariance matrix of the coefficients

S = matrix(NA, ncol=sum(c(1:length(fit$coefficients))), nrow = 5 )


k=3
j=1
i = "1st Study"
for( i in c("1st Study","2nd Study","3rd Study","4th Study", "5th Study" )){

minidf = df%>%
    filter(Study == i)

# Fit the GAM
fit = gam(formula =formula ,
          gamma =1,knots = Knots,
          family = binomial("logit"), data = minidf)


## Extract the fitted lines

temp.pred.df = data.frame(predict(fit,se.fit = T, 
newdata = predicted.values[,1:2]))

### Store the fitted values with their 95% CI in the data.frame

predicted.values[,k] =  temp.pred.df[,1]
predicted.values[,k+1] =  temp.pred.df[,2]


### Extract the coefficients and their standard errors for mvmeta
estimated.coefficients[j,] = fit$coefficients
S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]


#mat1 <- predict.gam(fit, type = "lpmatrix")

k=k+2
j=j+1
rm(i,minidf,temp.pred.df,fit)
}
rm(k,j)

#### Point-wise meta-analysis
#### Here we meta-analyse the fitted lines

#### We generate 2 new columns to accept the Pooled estimates
predicted.values$Overall_fit = NA
predicted.values$Overall_se.fit =  NA

for(i in 1:dim(predicted.values)[1]){
  minidf =  predicted.values[i,]
  betas =  t(predicted.values[i,]%>%
    select(-contains("."))%>%
    select(-contains("Overall"))%>%
    select(contains("fit")))
  se.betas =  t(predicted.values[i,]%>%
                      select(-contains("Overall"))%>%
                  select(contains("se.fit")))
  
  
  meta = metagen(TE = betas, seTE = se.betas, hakn = T, method.tau = "REML")
  predicted.values[i,]$Overall_fit = meta$TE.random
  predicted.values[i,]$Overall_se.fit =  meta$seTE.random
  # print(i) # for controlling erroneous meta-analyses
  rm(i)
}




#### Create a long object that contains the predicted lines with their standard error
long.predicted.values = predicted.values%>%
  select(-contains("se.fit"))%>%
  gather(key ,value= fit, -c("BMI","Treatment" ))
### Save the predicted standard errors in the long object
long.predicted.values$se.fit =
  as.vector(predicted.values%>%
  select(contains("se.fit"))%>%
  gather(key ,value= se.fit)%>%
  select(contains("se.fit")))
### Recode the fit values to Study IDs
  long.predicted.values =  long.predicted.values%>%
  mutate(Study=recode(key,
                'fit1' = "1st Study",
                'fit2' = "2nd Study",
                'fit3' = "3rd Study",
                'fit4' = "4th Study",
                'fit5'= "5th Study",
                'Overall_fit'= "Pooled fit"))
### Load some help function for expit transformation


  
### Calculate the CIs in log-scale using 1.96*standard errors 
  
  long.predicted.values$Lower = long.predicted.values$fit - 1.96*long.predicted.values$se.fit$se.fit
  long.predicted.values$Upper = long.predicted.values$fit + 1.96*long.predicted.values$se.fit$se.fit
  
### Back transform to absolute risk the logit transformed predicted values  
  long.predicted.values$Lower =  expit(long.predicted.values$Lower)
  long.predicted.values$Upper =  expit(long.predicted.values$Upper)
  long.predicted.values$probability =  expit(long.predicted.values$fit)
  



## ----Plot of point-wise meta-analysis P-splines splines heterogeneous data----
### Plot the fitted lines per study and Pooled
long.predicted.values[long.predicted.values$Study == "Pooled fit",]%>%
ggplot(aes(x = BMI, probability, linetype= Treatment, color= Treatment)) + 
  geom_line() +
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Script for P-splines different range Data---------------------------

#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set

### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation
formula = Y2~ BMI + s(BMI,bs = "ps",by = Treatment,fx = T,k = 4,m = c(2,2))
fit = gam(formula =formula ,gamma =1,
          family = binomial("logit"), data = df, nthreads = 8, method = "GCV.Cp")

### Create an empty data frame for the fitted values of the gam
predicted.values = data.frame(   df[,c("BMI","Treatment")],
                                 matrix(NA, nrow = 5000,ncol=10,dimnames = list(NULL, apply(expand.grid(c("fit","se.fit"),c(1:5)),1, paste, collapse=""))))
### Create an empty matrix for the estimated splines coefficients

estimated.coefficients = matrix(NA,ncol = length(fit$coefficients),nrow=5,dimnames = list( apply(expand.grid(c("Study"),c(1:5)),1, paste, collapse=" "),
c(1:length(fit$coefficients))))

### Create an empty matrix for the variance-covariance matrix of the coefficients

S = matrix(NA, ncol=sum(c(1:length(fit$coefficients))), nrow = 5 )


k=3
j=1
i = "1st Study"
for( i in c("1st Study","2nd Study","3rd Study","4th Study", "5th Study" )){

minidf = df%>%
    filter(Study2 == i)

formula = Y2~ BMI + s(BMI,bs = "ps",by = Treatment,fx = T,k = 4,m = c(2,2))
# Fit the GAM

fit = gam(formula =formula ,gamma =1,
          family = binomial("logit"), data = minidf, nthreads = 8, method = "GCV.Cp")

#####
#   ggplot(minidf, aes(BMI, Y, linetype= Treatment)) + geom_point()+
#    geom_line(aes(y = expit(predict(fit)))) + 
#    geom_vline(xintercept = fit$smooth[[1]]$knots,linetype =2, show.legend = F)  
#        


## Extract the fitted lines
new.data = predicted.values[,1:2]

### Here I introduce a some lag points to handle smoothness over the non-overlapping Studies

lag.points = 0.5
new.data = new.data[which(max(minidf$BMI)+lag.points>=new.data$BMI & new.data$BMI  >= min(minidf$BMI )-lag.points),]


temp.pred.df = data.frame(predict.gam(fit,se.fit = T,newdata.guaranteed = T,newdata = new.data))

### Store the fitted values with their 95% CI in the data.frame

predicted.values[which(max(minidf$BMI)+lag.points>=predicted.values$BMI & predicted.values$BMI  >= min(minidf$BMI )-lag.points),k] =  temp.pred.df[,1]
predicted.values[which(max(minidf$BMI)+lag.points>=predicted.values$BMI & predicted.values$BMI  >= min(minidf$BMI )-lag.points),k+1] =  temp.pred.df[,2]


### Extract the coefficients and their standard errors for mvmeta
estimated.coefficients[j,] = fit$coefficients
S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]

#mat1 <- predict.gam(fit, type = "lpmatrix")

k=k+2
j=j+1
rm(i,minidf,temp.pred.df,fit)
}
rm(k,j)

#### Point-wise meta-analysis
#### Here we meta-analyse the fitted lines
#### We generate 2 new columns to accept the Pooled estimates
predicted.values$Overall_fit = NA
predicted.values$Overall_se.fit =  NA

for(i in 1:dim(predicted.values)[1]){
  minidf =  predicted.values[i,]
  betas =  t(predicted.values[i,]%>%
    select(-contains("."))%>%
    select(-contains("Overall"))%>%
    select(contains("fit")))
  se.betas =  t(predicted.values[i,]%>%
                      select(-contains("Overall"))%>%
                  select(contains("se.fit")))
  
  
  meta = metagen(TE = betas, seTE = se.betas, hakn = T, method.tau = "REML")
  predicted.values[i,]$Overall_fit = meta$TE.random
  predicted.values[i,]$Overall_se.fit =  meta$seTE.random
  # print(i) for controlling erroneous meta-analyses
  rm(i)
}




#### Create a long object that contains the predicted lines with their standard error
long.predicted.values = predicted.values%>%
  select(-contains("se.fit"))%>%
  gather(key ,value= fit, -c("BMI","Treatment" ))
### Save the predicted standard errors in the long object
long.predicted.values$se.fit =
  as.vector(predicted.values%>%
  select(contains("se.fit"))%>%
  gather(key ,value= se.fit)%>%
  select(contains("se.fit")))
### Recode the fit values to Study IDs
  long.predicted.values =  long.predicted.values%>%
  mutate(Study2=recode(key,
                'fit1' = "1st Study",
                'fit2' = "2nd Study",
                'fit3' = "3rd Study",
                'fit4' = "4th Study",
                'fit5'= "5th Study",
                'Overall_fit'= "Pooled fit"))
### Calculate the CIs in log-scale using 1.96*standard errors 
  
  long.predicted.values$Lower = long.predicted.values$fit - 1.96*long.predicted.values$se.fit$se.fit
  long.predicted.values$Upper = long.predicted.values$fit + 1.96*long.predicted.values$se.fit$se.fit
  
### Back transform to absolute risk the logit transformed predicted values  
  long.predicted.values$Lower =  expit(long.predicted.values$Lower)
  long.predicted.values$Upper =  expit(long.predicted.values$Upper)
  long.predicted.values$probability =  expit(long.predicted.values$fit)
  



## ----Plot of point-wise meta-analysis P-splines splines different range Data----
### Plot the fitted lines per study and Pooled
long.predicted.values[long.predicted.values$Study == "Pooled fit",]%>%
ggplot(aes(x = BMI, probability, linetype= Treatment, color= Treatment)) + 
  geom_line() +
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----B-splines with random effects heterogeneous data--------------------
#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set
fit = gam(Y ~ bs(BMI,knots = c(min(df$BMI),20,25,30,35,max(df$BMI)))*Treatment +  
            s(Study,by =  dum,bs = "re"),
          family = binomial("logit"), data = df, nthreads = 8, method = "REML",  discrete=TRUE)

new.data = data.frame(cbind(df[,c("Study","BMI","Treatment")],dum=rep(0,dim(df)[1])))
preds=  predict.gam(fit, se.fit = T,newdata = new.data,newdata.guaranteed = T)

preds$lower = preds$fit -1.96*preds$se.fit
preds$upper = preds$fit +1.96*preds$se.fit



## ----Plot B-splines with random effects heterogeneous data---------------
ggplot(df, aes(BMI, Y, linetype= Treatment,color = Treatment)) + geom_point()+
    geom_line(aes(y = expit(preds$fit))) + 
  geom_ribbon(ymin = expit(preds$lower),ymax=expit(preds$upper),alpha=0.2)+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----B-splines with random effects different range data------------------

#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set


fit = gam(Y2 ~ bs(BMI,knots = c(min(df$BMI),20,25,30,35,max(df$BMI)))*Treatment +  
            s(Study2,by =  dum,bs = "re"),
          family = binomial("logit"), data = df, nthreads = 8, method = "REML")

new.data = data.frame(cbind(df[,c("Study2","BMI","Treatment")],dum=rep(0,dim(df)[1])))
preds=  predict.gam(fit, se.fit = T,newdata = new.data,newdata.guaranteed = T)

preds$lower = preds$fit -1.96*preds$se.fit
preds$upper = preds$fit +1.96*preds$se.fit



## ----Plot of B-splines with random effects different range data----------
ggplot(df, aes(BMI, Y2, linetype= Treatment,color = Treatment)) + geom_point()+
    geom_line(aes(y = expit(preds$fit))) + 
  geom_ribbon(ymin = expit(preds$lower),ymax=expit(preds$upper),alpha=0.2)+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Smoothing splines with random effects heterogeneous data------------
#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set

df$dum <- rep(1,dim(df)[1])

fit = gam(Y ~ s(BMI,by = Treatment,bs="tp") +  s(Study,by =  dum,bs = "re"),
          family = binomial("logit"), data = df, nthreads = 8, method = "REML",  discrete=TRUE)

new.data = data.frame(cbind(df[,c("Study","BMI","Treatment")],dum=rep(0,dim(df)[1])))
preds=  predict.gam(fit, se.fit = T,newdata = new.data,newdata.guaranteed = T)


preds$lower = preds$fit -1.96*preds$se.fit
preds$upper = preds$fit +1.96*preds$se.fit


## ----Plot of smoothing splines with random effects heterogeneous data----
ggplot(df, aes(BMI, Y, linetype= Treatment,color = Treatment)) + geom_point()+
    geom_line(aes(y = expit(preds$fit))) + 
  geom_ribbon(ymin = expit(preds$lower),ymax=expit(preds$upper),alpha=0.2)+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----Smoothing splines with random effects different range data----------
#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set

df$dum <- rep(1,dim(df)[1])

fit = gam(Y2 ~ s(BMI,by = Treatment,bs="tp") +  s(Study2,by =  dum,bs = "re"),
          family = binomial("logit"), data = df, nthreads = 8, method = "REML",  discrete=TRUE)

new.data = data.frame(cbind(df[,c("Study2","BMI","Treatment")],dum=rep(0,dim(df)[1])))
preds=  predict.gam(fit, se.fit = T,newdata = new.data,newdata.guaranteed = T)


preds$lower = preds$fit -1.96*preds$se.fit
preds$upper = preds$fit +1.96*preds$se.fit


## ----Plot of smoothing splines with random effects different range data----
ggplot(df, aes(BMI, Y2, linetype= Treatment,color = Treatment)) + geom_point()+
    geom_line(aes(y = expit(preds$fit))) + 
  geom_ribbon(ymin = expit(preds$lower),ymax=expit(preds$upper),alpha=0.2)+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----P-splines with random effects heterogeneous data--------------------

#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set

fit = gam(Y ~ s(BMI,by = Treatment,bs="ps") +  s(Study,by =  dum,bs = "re"),
          family = binomial("logit"), data = df, nthreads = 8, method = "REML",  discrete=TRUE)

new.data = data.frame(cbind(df[,c("Study","BMI","Treatment")],dum=rep(0,dim(df)[1])))
preds=  predict.gam(fit, se.fit = T,newdata = new.data,newdata.guaranteed = T)


preds$lower = preds$fit -1.96*preds$se.fit
preds$upper = preds$fit +1.96*preds$se.fit


## ----Plot of P-splines with random effects heterogeneous data------------
ggplot(df, aes(BMI, Y, linetype= Treatment)) + geom_point()+
    geom_line(aes(y = expit(preds$fit))) + 
  geom_ribbon(ymin = expit(preds$lower),ymax=expit(preds$upper),alpha=0.2)+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----P-splines with random effects different range data------------------

#rm(list=ls()[! ls() %in% c("df","expit")]) ### To clear all enviroment besides the data-set

fit = gam(Y2 ~ s(BMI,by = Treatment,bs="ps") +  s(Study2,by = dum,bs = "re"),
          family = binomial("logit"), data = df, nthreads = 8, method = "REML")

new.data = data.frame(cbind(df[,c("Study2","BMI","Treatment")],dum=rep(0,dim(df)[1])))
preds=  predict.gam(fit, se.fit = T,newdata = new.data,newdata.guaranteed = T)

preds$lower = preds$fit -1.96*preds$se.fit
preds$upper = preds$fit +1.96*preds$se.fit



## ----Plot of P-splines with random effects different range data----------

ggplot(df, aes(BMI, Y2, linetype= Treatment,color = Treatment)) + geom_point()+
    geom_line(aes(y = expit(preds$fit))) + 
  geom_ribbon(ymin = expit(preds$lower),ymax=expit(preds$upper),alpha=0.2)+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(0, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "top") 


## ----loading the datasets------------------------------------------------

IPDMA <- read_sas("Data/IPDMA.sas7bdat")
names(IPDMA) <- tolower(names(IPDMA))
IPDMA$treat =  factor(IPDMA$treat  , labels = c("Placebo","Antibiotics") )
IPDMA$study = factor(IPDMA$study, labels = c("Damoiseaux","Burke","Appelman","Little","Saux","McCormick"))
IPDMA$bilat_0 =  factor(IPDMA$bilat_0  , labels = c("No","Yes") )
Tubano <- read_csv("Data/VolledigeDS.csv")
somatostatin <- read_sav("Data/somatostatin.sav")
source("Data/somatostatin_descriptives.R")




## ----Random effects smoothing splines------------------------------------

miniIPD= IPDMA%>% filter((study != "Little"))
miniIPD$bilat_0 =  factor(miniIPD$bilat_0 , labels = c("Unilateral","Bilateral") )

miniIPD$dum =  rep(1, dim(miniIPD)[1])

m1 <- gam(formula = poutcome ~ treat*bilat_0*age + s(age,by = bilat_0, bs="tp")+
            s(age, by = treat, bs="tp") + s(study, by = dum, bs="re"), 
          family = binomial("logit"), data = miniIPD, 
          method="REML")


age =  vector()
study = vector()
for(i in unique(miniIPD$study)){
  
  minimum =  floor(min(miniIPD[miniIPD$study==i,]$age))
  maximum =  ceiling(max(miniIPD[miniIPD$study==i,]$age))
  age= c(age,rep(seq( from  = minimum,to =  maximum, by = 0.01),4))
  l= length(rep(seq( from  = minimum,to =  maximum, by = 0.01),4))
  study = c(study,rep(i,l ))
  rm(minimum,maximum)
}


new.dat = data.frame(study   = study,
                     treat   = as.factor(rep(x = unique(miniIPD$treat)  ,7810)),
                     bilat_0 = as.factor(rep(rep(x = unique(miniIPD$bilat_0),each =2)  ,3905)),
                     age = age,
                     dum = rep(1))


#rm(age, study, i, l)


m1.pred =  as.data.frame(predict.gam(m1, se.fit = T,  rm.ranef=TRUE,newdata = new.dat) )


m1.pred$low  = with(m1.pred, expit(fit-1.38*se.fit)) ; 
m1.pred$high = with(m1.pred, expit(fit+1.38*se.fit))

new.dat =  cbind(new.dat,m1.pred )
new.dat$fit = expit(new.dat$fit) 


ggplot(miniIPD, aes(x = age, y = poutcome, linetype = treat, color = study) )+
  facet_wrap(.~bilat_0 +study, ncol = 5) +
  geom_point() + scale_color_jama(guide = FALSE)+ scale_linetype_discrete(name ="Treatment")+
  geom_ribbon(data = new.dat, aes(y=fit, x=age,ymin= low, ymax=high), alpha=0.2)  +
  geom_line(data=new.dat,aes(y = fit)) + ylab("Predicted probability") + xlab("Children's Age")+
  ggtitle("Predicted probability of having fever after 1 week", 
          subtitle = "Stratified by study and otitis media status (unilateral / bilateral)")+
  theme(plot.title = element_text(hjust = 0.5,size = 28),
        legend.text = element_text(hjust = 0.5,size = 24),
        legend.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(hjust = 0.5,size = 24),
        axis.title.y = element_text(hjust = 0.5,size = 24),
        axis.text.x = element_text(hjust = 0.5,size = 24),
        axis.text.y = element_text(hjust = 0.5,size = 24),
        plot.subtitle = element_text(hjust = 0.5,size = 24),
        strip.text.x = element_text(size = 18))


## ------------------------------------------------------------------------

## Re-calculate the 95% CIs
m1.pred$low  = with(m1.pred, expit(fit-1.96*se.fit)) ; 
m1.pred$high = with(m1.pred, expit(fit+1.96*se.fit))

new.dat$low  =  m1.pred$low
new.dat$high =  m1.pred$high

p1 =  new.dat[new.dat$treat =="Placebo",]$fit
p2 =  new.dat[new.dat$treat =="Antibiotics",]$fit
l1 =  new.dat[new.dat$treat =="Placebo",]$low
l2 =  new.dat[new.dat$treat =="Antibiotics",]$low
u1 =  new.dat[new.dat$treat =="Placebo",]$high
u2 =  new.dat[new.dat$treat =="Antibiotics",]$high


new.diff =  data.frame(diff.fit = new.dat[new.dat$treat =="Antibiotics",]$fit-   new.dat[new.dat$treat =="Placebo",]$fit )


new.diff$low =  new.diff$diff.fit - sqrt((p1-l1)^2 + (u2-p2)^2)
new.diff$high =  new.diff$diff.fit + sqrt((p2-l2)^2 + (u1-p1)^2)

new.diff =  cbind(new.diff, new.dat[new.dat$treat =="Antibiotics", 
                                    colnames(new.dat) %in% c("study", "age", "treat","bilat_0" )])


new.diff %>% 
  mutate(study = factor(new.diff$study, levels = c("Damoiseaux", "Burke", "Appelman","Saux","McCormick")))%>%
ggplot(aes(x = age, y = diff.fit, color = study) )+ 
  ylim(-0.5,1)+ xlim(0.02,13)+
  facet_wrap(.~bilat_0 +study, ncol = 5) + scale_color_jama(guide = F)+
  geom_line(data=new.diff,aes(y = diff.fit))+ ylab("Predicted risk difference") + xlab("Children's Age")+ 
  geom_ribbon(data = new.diff, aes(y=diff.fit, x=age,ymin= low, ymax=high), alpha=0.2) +
  ggtitle("Risk differences over age", subtitle = "Stratified by otitis media status (unilateral / bilateral)")+
  theme(plot.title = element_text(hjust = 0.5,size = 28),
        legend.text = element_text(hjust = 0.5,size = 24),
        legend.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(hjust = 0.5,size = 24),
        axis.title.y = element_text(hjust = 0.5,size = 24),
        axis.text.x = element_text(hjust = 0.5,size = 24),
        axis.text.y = element_text(hjust = 0.5,size = 24),
        plot.subtitle = element_text(hjust = 0.5,size = 24),
        strip.text.x = element_text(size = 18))


## ------------------------------------------------------------------------
  age.dif= seq( from  = min(new.diff$age),to =  max(new.diff$age), by = 0.01)
  new.diff$se =  new.diff$low
  new.diff =  write.csv(new.diff, "Data/new.diff.csv")
  meta.new.diff = read.csv("Data/meta.new.diff.csv",header = T)
  library(metafor)
  
  meta.new.diff$pooled.est = NA
  meta.new.diff$pooled.est.lower = NA
  meta.new.diff$pooled.est.upper = NA
  
  
  
  meta.new.diff$Damoiseaux.fit =  -meta.new.diff$Damoiseaux.fit
  meta.new.diff$Burke.fit =  -meta.new.diff$Burke.fit
  meta.new.diff$Appelman.fit =  -meta.new.diff$Appelman.fit
  meta.new.diff$Saux.fit =  -meta.new.diff$Saux.fit
  meta.new.diff$McCormick.fit =  -meta.new.diff$McCormick.fit
  
  for(i in 1:dim(meta.new.diff)[1]){
    meta.fit = rma(yi = meta.new.diff[i,3:7],sei = meta.new.diff[i,8:12])
    
    meta.new.diff[i,]$pooled.est = meta.fit$b
    meta.new.diff[i,]$pooled.est.lower = meta.fit$ci.lb
    meta.new.diff[i,]$pooled.est.upper = meta.fit$ci.ub
  }
  
  
  meta.new.diff$bilat_0 =  factor(meta.new.diff$bilat_0, labels = c("Unilateral", "Bilateral"))
  


g1= ggplot(meta.new.diff, aes(x = age, y = pooled.est, colour= bilat_0) )+ 
  xlim(0.01,13) + scale_color_jama(name = "Otitis media status", 
                                   labels = c("Unilateral", "Bilateral"))+facet_wrap(.~bilat_0,nrow = 2)+
  theme_minimal()+ ylab("Pooled estimated risk difference")+xlab("Children's Age")+
  stat_smooth(aes(x=age, y=pooled.est), method="loess", se=FALSE, linetype= 1,size = 0.5)+
  stat_smooth(aes(x=age, y=pooled.est.lower), method="loess", se=FALSE, linetype= 2,size = 0.5) +
  stat_smooth(aes(x=age, y=pooled.est.upper), method="loess", se=FALSE, linetype= 2,size = 0.5) +
  ggtitle("Pooled risk difference over age", 
          subtitle = "Stratified per otitis media status (unilateral / bilateral)")+
  theme(plot.title = element_text(hjust = 0.5,size = 28),
        legend.text = element_text(hjust = 0.5,size = 22),
        legend.title = element_text(hjust = 0.5,size = 22),
        axis.title.x = element_text(hjust = 0.5,size = 22),
        axis.title.y = element_text(hjust = 0.5,size = 22),
        axis.text.x = element_text(hjust = 0.5,size = 22),
        axis.text.y = element_text(hjust = 0.5,size = 22),
        plot.subtitle = element_text(hjust = 0.5,size = 22),
        strip.text.x = element_text(size = 18))


g1
# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
df2 <- data.frame(x = gg1$data[[1]]$x,
                  y= gg1$data[[1]]$y,
                  ymin = gg1$data[[2]]$y,
                  ymax = gg1$data[[3]]$y,
                  bilat_0 = factor(gg1$data[[1]]$PANEL, labels =  c("Unilateral", "Bilateral"))) 

# use the loess data to add the 'ribbon' to plot 
g1 +  
  geom_ribbon(data = df2, aes(y= y,x = x, ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.4) + 
  geom_hline(yintercept = 0, linetype =3)





## ----One-Stage smoothing splines in Somatostatin ,eval=FALSE-------------
# Note: this is really not the best fitting model for the data:
m1 <- bam(dif_liver_perc ~ Drug*Gender*Age + 
            s(Age,by = Study) + 
            s(Study,Gender,bs = "re") + 
            s(Study,Drug,bs="re") , data = somatostatin,method="fREML")

m1.pred =  as.data.frame(predict.gam(m1,somatostatin, se.fit = T) ); m1.pred$low = with(m1.pred, fit-1.96*se.fit) ; m1.pred$high = with(m1.pred, fit+ 1.96*se.fit)

ggplot(somatostatin, aes(x = Age, y = dif_liver_perc, color = Study) ) +
    geom_line(aes(y = exp(m1.pred$fit), linetype = Drug), size = 1) +
    geom_point() + facet_wrap(~ Gender, ncol = 2) + scale_color_futurama()


# Change the colors:
plot_data(m1, view="Age", split_by="Drug", type='p', col=c("#6A659999","#79AF9799"), alpha=1)


par(mfrow=c(2,1), cex=0.75)
plot_diff(m1, view='Age',cond = list(Gender="Male") , comp=list(Drug=c("placebo", "somatostatin")), 
    rm.ranef=TRUE, col = "darkblue",print.summary = F, main = "Men", 
    xlab = "Age (in years)",
    ylab = "Estimated treatment difference") 

plot_diff(m1, view='Age',cond = list(Gender="Female") , comp=list(Drug=c("placebo", "somatostatin")), 
    rm.ranef=TRUE, col = "darkgreen",print.summary = F, main = "Women",
    xlab = "Age (in years)",
    ylab = "Estimated treatment difference ")



## ----eval=FALSE----------------------------------------------------------
# version 1:
plot_smooth(m1, view="Age", plot_all="Drug", rm.ranef=TRUE, se = 2.56,print.summary = T)



## ----two-stage GAMs, eval=FALSE------------------------------------------
testIPDMA =  IPDMA
testIPDMA$fit = NA
testIPDMA$se.fit = NA

for (i in unique(IPDMA$study)){
  minIPD =  IPDMA %>%
    filter(study ==  i)
  
  if(length(unique(minIPD$age ))<10){ 
  fit = try(glm(poutcome ~ age*treat*bilat_0 , family = binomial("logit"), data = minIPD), silent = T)
  }else{
  fit = try(bam(poutcome ~ age*treat*bilat_0 + + s(age, by = treat, bs = "cs", k=-1) + s(age, by = bilat_0, bs = "cs", k=-1) , family = binomial("logit"), data = minIPD, method="fREML" ,  discrete=TRUE), silent = T)  
  }
  
  
  
  if(class(fit) != "try-error"){
  pred.fit =  predict(fit, se.fit = T)
  testIPDMA[testIPDMA$study == i,]$fit    = pred.fit$fit
  testIPDMA[testIPDMA$study == i,]$se.fit = pred.fit$se.fit
  print(i)
  }else{
    print("error")
  }
}


ggplot(testIPDMA, aes(x = age, y = poutcome, color = study) ) +
    geom_line(aes(y = expit(fit), linetype = treat), size = 1) +
    geom_point() + facet_wrap(~ bilat_0 + study, ncol = 5) + theme_minimal()





## ----One-Stage smoothing splines in AOM study ,eval=FALSE----------------


miniIPD= IPDMA%>%
    filter((study != "Little"))


m2 <-  gamm4(poutcome ~ treat+ bilat_0 + s(age, by = treat) + s(age, by = bilat_0), 
             random = ~ (age|study) + (treat|study) + (age|study) , family = binomial("logit"), data = miniIPD, method="fREML" )

m2.pred =  as.data.frame(predict.gam(m2$gam, se.fit = T) ); m2.pred$low = with(m2.pred, fit-1.96*se.fit) ; m2.pred$high = with(m2.pred, fit+ 1.96*se.fit)

miniIPD =  cbind(miniIPD,m2.pred )

ggplot(miniIPD, aes(x = age, y = poutcome, color = study) )+ facet_wrap(.~bilat_0 +study, ncol = 5) +
    geom_line(aes(y = expit(fit), linetype = treat), size = 1) +
    geom_point() + scale_color_futurama() 



# Note: this is really not the best fitting model for the data:
m1 <- bam(poutcome ~ treat+ bilat_0 + (age) + s(age, by = treat) + s(age, by = bilat_0) + treat*bilat_0 + s(study,bs = "re")  , family = binomial("logit"), data = miniIPD, method="fREML" ,  discrete=TRUE)

m1.pred =  as.data.frame(predict.gam(m1, se.fit = T) ); m1.pred$low = with(m1.pred, fit-1.96*se.fit) ; m1.pred$high = with(m1.pred, fit+ 1.96*se.fit)

miniIPD =  cbind(miniIPD,m1.pred )

ggplot(miniIPD, aes(x = age, y = poutcome, color = study) )+ facet_wrap(.~bilat_0 +study, ncol = 5) +
    geom_line(aes(y = expit(fit), linetype = treat), size = 1) +
    geom_point() + scale_color_futurama() 

par(mfrow=c(2,1), cex=0.75)
plot_smooth(m1, view="age", plot_all="treat", cond = list(bilat_0="No"), transform = "expit" ,  rm.ranef=TRUE, se = 1.96)
plot_diff(model = m1, view='age',cond = list(bilat_0="No") ,se = 1.96,n.grid = 1000,sim.ci = T,
          comp=list(treat=c("Placebo", "Antibiotics")), 
          rm.ranef=TRUE, col = "darkblue",print.summary = F, main = "Children with unilateral otitis media", 
          xlab = "Age (in years)", ylab = "Estimated difference (log-scale)")


## ----eval=FALSE----------------------------------------------------------
par(mfrow=c(2,1), cex=0.75)
plot_smooth(m1, view="age", plot_all="treat", cond = list(bilat_0="Yes") ,  
            rm.ranef=TRUE, sim.ci=TRUE, se = 0.8, transform = "expit")
plot_diff(m1, view='age',cond = list(bilat_0="Yes") , comp=list(treat=c("Placebo", "Antibiotics")), 
    rm.ranef=TRUE, col = "darkgreen",print.summary = F, main = "Children with bilateral otitis media",
    xlab = "Age (in years)", mark.diff = T,col.diff =  "purple",sim.ci=TRUE, 
    ylab = "Estimated difference (log-scale)")

