### MVmeta

## ----Code to simulate some data----------------------------------------
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

rm(noise,noise2)






## ----Script for Basis splines (cubic)------------------------------------

# Inverse logit function
#rs is the risk score (or linear predictor or predicted log-odds)
expit<-function(rs) {1/(1+exp(-rs))}

### fit a preliminary model in order to get the lengths of the coefficients and 
### use it as a guide for further data manipulation

Knots =   c(min(df$BMI),20,25,30,35,max(df$BMI))
fit = gam(Y~ bs(x = BMI,knots = Knots,intercept = T,degree = 3,
                Boundary.knots = c(min(df$BMI),max(df$BMI)))*Treatment+BMI , 
          family = binomial("logit"), data = df)

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
  
# Fit the GAM using 
  
  fit = gam(Y~ bs(x = BMI,knots = Knots,intercept = T,degree = 3,
                  Boundary.knots = c(min(df$BMI),max(df$BMI)))*Treatment +BMI, 
            family = binomial("logit"), data = minidf)
  
### Get the coefficients and their standard errors for mvmeta
  estimated.coefficients[j,] = fit$coefficients
  S[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  rm(i,minidf,fit)
}
rm(k,j)

#### Multi-variate meta-analysis

mv.fit = mvmeta(estimated.coefficients, S)
fit = gam(Y~ BMI+ Treatment + bs(x = BMI,knots = Knots,intercept = T,degree = 3,
                                 Boundary.knots = c(min(df$BMI),max(df$BMI)))*Treatment+BMI , 
          family = binomial("logit"), data = df)

Xp =  predict.gam(fit, type="lpmatrix")

mv.vcov =  as.data.frame(cbind(Lower = coef(mv.fit)-qt(0.975,df = Inf) *sqrt(diag(vcov(mv.fit))),
                               Upper = coef(mv.fit)+qt(0.975,df = Inf) *sqrt(diag(vcov(mv.fit)))))

prediction.interval.mvmeta =  Xp%*% coef(mv.fit)
prediction.interval.mvmeta.lower =  Xp%*%mv.vcov$Lower
prediction.interval.mvmeta.upper =  Xp%*% mv.vcov$Upper

mvmeta.df = cbind(df[,c("Study","BMI","Treatment")],
                  fit =  expit(prediction.interval.mvmeta), 
                  Lower= expit(prediction.interval.mvmeta.lower),
                  Upper =expit(prediction.interval.mvmeta.upper ))

ggplot(mvmeta.df,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line()+ 
  ylab("Estimated Mortality risk") + scale_color_jama()+ 
  labs(title = "Multi-variate meta-analysis approache",
       subtitle = "Trying to correct the CIs" , 
       caption = "Figure 6.") + 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 32,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 24,face = "bold.italic"),
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



