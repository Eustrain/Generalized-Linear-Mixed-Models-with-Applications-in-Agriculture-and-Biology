##################################################################################
########### GENERALIZED LINEAR MIXED MODELS WITH  APPLICATIONS IN AGRICULTURE AND BIOLOGY

### Eustrain 
##### ANALYSIS OF COVARIANCE
library(lme4)
library(lsmeans)
library(tidyverse)
library(lmerTest)
library(glmer)
library(emmeans)
library(agricolae)
library(multcomp) 
library(stringi)

df<- read.csv("table4.1.csv",header = TRUE)
head(df)

datos<- df %>% 
  mutate(xbar= Total.no..of.flowers-mean(Total.no..of.flowers)) %>% 
  mutate(Population=as.factor(Population)) %>% 
  rename(plants=Total.no..of.flowers)  
  mutate(plants=as.factor(plants))

str(datos)

model_ovules <- lmer(Eggs~Population*xbar+(1|plants/Population),data = datos)
summary(model_ovules)

anova(model_ovules,ddf="Satterthwaite")

lsmeans_df <- emmeans(model_ovules, "Population", type = "interaction")
#### comparation of means
cld(lsmeans_df, alpha = 0.05, Letters = LETTERS)

######################################################

###### Exercises

table1.28 <- read.csv("table1.28.csv",header = TRUE)


 table1.28<- table1.28 %>% rename_with(~str_replace_all(.,c("[^[:alnum:]]"="",
                                               "X"="",
                                               "1"="",
                                               "2"=""))) %>% 
   pivot_longer(cols = 2:6, names_to = "Trt",values_to = "growt") %>% 
   as.data.frame()
   
## model yi = means + treatment + error
 
modelo_1.28<- lm(growt~Trt,data=table1.28) 
anova(modelo_1.28)
lsmeans_1.28<- lsmeans(modelo_1.28,"Trt")
cld(lsmeans_1.28, alpha = 0.05, Letters = LETTERS)

### 
table1.29 <- read.csv("table1.29.csv",header = TRUE)
table1.29<- table1.29 %>% 
  pivot_longer(cols = 2:5,names_to = "Trt",values_to = "growth") %>% 
  as.data.frame()

head(table1.29)
####
### modelo yi = means + Trt+ Species+ Trt*Species+error
modelo_1.29 <- lm(growth~Trt*Species,data = table1.29)
anova_1.29 <- anova(modelo_1.29)

lsmeans_1.29<- lsmeans(modelo_1.29,~Trt|Species)

cld(lsmeans_1.29, alpha = 0.05, Letters = LETTERS)
#####################################################
###Chapter 2 Generalized Linear Models






#######################################################
####### percentaje of germinated seed 
df <- read.csv("table3.2.csv",header = TRUE)
head(df)
datos<- df %>% mutate(Tr.=as.factor(Tr.),y2=(y/n)) 

anv <- lm(y~Tr.,data=datos)
anv1<- anova(anv)
summary(anv)
### we estimated means foe each trt
t1<-coef(anv)[1] 
t2<- coef(anv)[1]+coef(anv)[2]
t3<- coef(anv)[1]+coef(anv)[3]
##############################################
### difference between treatments

# t1-t2= (n+t1)-(n+t2)= t1-t2
t1-t2
t1-t3
t2-t3
## or
anv2 <- lm(y~1+Tr.,data= datos)
summary(anv2)

## or 
library(sasLM)
library(emmeans)
LSM(y~Tr.,df_anv)
emm1<- emmeans(anv,specs = ~ Tr.)
T1 <- c(1,0,0)
T2 <- c(0,1,0)
contrast(emm1, method = list(T1 - T2) )
T3 <- c(0,0,1)
contrast(emm1, method = list(T2-T3) )
#### using emmeans
emmeans(anv, specs = pairwise ~ Tr., 
        at = list(sub.rate = c("Trt1", "Trt3") ) )

######################################################
################ analyze the same data, also using a CRD assuming a binomial
head(datos)
germi <- glm(y2~1+Tr.,family = "binomial",data = datos)
summary(germi)
n1_coef <- coef(germi)[1]
n2_coef <- coef(germi)[1]+coef(germi)[2]
n3_coef <- coef(germi)[1]+coef(germi)[3]
n1_coef-n2_coef


glm_coef <- emmeans(germi,"Tr.")


n1<- c(1,0,0)
n2 <- c(0,1,0)
n3 <- c(0,0,1)
contrast(glm_coef, method = list(n1 - n2),trans = "log")
contrast(glm_coef, method = list(n1 - n3))
contrast(glm_coef, method = list(n2 - n3))

##### SAME RESULT THAR BOOK
 
GLM(y2~Tr.,Data = datos,BETA = TRUE,EMEAN = TRUE)
###################################################
##### CALCULATE THE PROBAILITY

odds_1 <- exp(n1_coef)
odds_1/(1+odds_1)
odd_2 <- exp(n2_coef)
odd_2/(1+odd_2)
odd_3 <- exp(n3_coef)
odd_3/(1+odd_3)

emmeans(germi,"Tr.")

###(a) Odds ratio estimates
c2<- emmeans(germi,"Tr.", type = "mu")
contrast(c2, method = list(n1 - n3))
contrast(c2, method = list(n1 - n3))
###(b) Trt least squares means

emmeans(germi,"Tr.")

### (c) Differences of Trt least squares means
contrast(glm_coef, method = list(n1 - n2),trans = "log")
contrast(glm_coef, method = list(n1 - n3))
contrast(glm_coef, method = list(n2 - n3))





##### we can convert the rhe estimate to mean
regrid(glm_coef, transform = "response")

emmeans(germi, specs = pairwise ~ Tr., 
        at = list(sub.rate = c("Trt1", "Trt3") ) )


###############################################
#Percentage of germinated seeds (Y) out of total seeds (N) in a randomized complete block design
df <- read.csv("tabla.3.8.csv",header = TRUE)
head(df)
df_analisis<- df %>%
  rename(trt= Treatment, block=Block,y=Y..no..of.germinated.seeds.,n=N..total.no..of.seeds.) %>% 
  mutate(trt=as.factor(trt),block=as.factor(block), y2= y/n)

model <- lm (y~trt+block,data = df_analisis)

summary(model)
coef(model)
#(b) Trt least squares means
lsmeans_model <- emmeans(model,~trt)
#(c) Differences of Trt least squares means
t1<- c(1,0,0)
t2 <- c(0,1,0)
t3 <- c(0,0,1)
contrast(lsmeans_model, method = list(t1 - t2),type ="response")
contrast(lsmeans_model, method = list(t1 - t3))
contrast(lsmeans_model, method = list(t2 - t3))


###### the same sintaxis like R istimated 
ESTM(t(c(0,2,-1,-1,0,0,0)),y~trt+block,df_analisis)
ESTM(t(c(0,-1,2,-1,0,0,0)),y~trt+block,df_analisis)
#contrast 'type 3 trt ss' trt 1 0 -1 0,trt 0 1 -1;
ESTM(t(c(1,0,-1,0,0,0,0)),y~trt+block,df_analisis)

##### using r 
means <- emmeans(model , specs = ~ trt)
contr <- list("tr1-tr2 vs tr3" = c(2, -1, -1),
              "trt2 vs tr1-tr3" = c(-1, 2, -1))
contrast(means, method = contr)


#### Result of the analysis of variance in the binomial model
model2 <- model <-glm(y2~trt+block,family = "binomial",data = df_analisis)
summary(model2)

emm_model2<-emmeans(model2,~trt) 

contr <- list("tr1-tr2 " = c(1,-1,-1),
              "trt2 vs tr3" = c(0, 1, -1))

emm<- contrast(emm_model2, method = contr,type="response")
confint(emm, adjust = "sidak", level = 0.95)

model_2_block <- emmeans(model2,~block)
contr_blk <- list("block1-block3" = c(1,0,-1),
                  "block2-block3" = c(0,1,-1))
emm_blk<- contrast(model_2_block, method = contr_blk,type="response" )
confint(emm_blk, adjust = "sidak", level = 0.05)


contr_2 <- list("tr1 vs tr2y3" = c(2, -1, -1),
                "tr1 vs tr2" = c(1, -1, 0),
              "trt1 vs tr2" = c(1, 0, -1),
              "tr2 vs tr3"= c(0,1,-1))
contrast(emm_model2, method = contr_2,type = "reponse")
contrast(emm_model2, method = contr_2,adjust = "sidak")

#############################################################
#############################################################
# Mixed models with a normal response
library(lme4)
model_mix <- lmer(y ~ trt+ (1|block), data = df_analisis)
summary(model_mix)

means_mix <- emmeans(model_mix,"trt")
#####################################
### the estimable function :  y = n + t, because Block is 0

fixef(model_mix)[1]

trt1 <- fixef(model_mix)[1]
trt2 <- fixef(model_mix)[2]
trt3 <- fixef(model_mix)[3]

#### estime the BLUPS for each treatment
blocks_effects<- ranef(model_mix)
blocks<- sum(blocks_effects$block[1:3,])
str(trt1)
trt1_blup <- trt1+blocks
tr2_blup <- trt2+blocks
tr3_blup <- trt3+blocks



blups_result <- lsmeans(model_mix, specs = "trt", at = list(trt = c("Trt1","Trt2","Trt3")), type = "response")

####################################################
## Marginal and conditional models
##  conditional  mean : u + t + b
## the marginal mean  : u + t
library(nlme)
library(car)
df <- read.csv("table3.17.csv",header = TRUE)
head(df)


df_analisis<- df %>% rename(block=Block,y =Mortality) %>% 
  mutate(S=as.numeric(S),C=as.numeric(C),block=as.numeric(block))
  
str(df_analisis)
model <- lmer(y ~ S*C + (1|block/S)+(1|block/C), data = df_analisis)#similar
  
model <- lmer(y ~ S*C + (S|block)+(C|block), data = df_analisis)
summary(model)
emmeans(model, ~ S*C)
model <- lmer(y ~ (1|C) , data = df_analisis) 

model <- lmer(y ~ S*C + (1+S|block)+(1+C|block), data = df_analisis)
model <- lmer(y ~ S*C + (S||block)+(C||block),data = df_analisis)
summary(model)

m3 <- lme(distance ~ age + Sex, data = Orthodont, random = list(Subject = pdDiag(~ age)))
  m1<- lme(y~S*C,data = df_analisis,random = list(block=pdDiag(~S),block=pdDiag(~C)))
  summary(m1)        
          
  
  summary(model)
  # Compute least squares means (LSMEANS)
  ranef(model)
  
  lsmeans <- emmeans(model, ~ S*C)
  


model1.1 <- lmer(y~S*C+(1|C),data = df_analisis)
summary(model1.1)

M<- emmeans(model1.1, ~S*C)
contrast(M, interaction = "pairwise")
#####################################
library(glmm)

head(df_analisis)
# Fit the generalized linear mixed model
model <- glmer(Pct ~ S*C + (1|block) + (1|S:block) + (1|C:block), 
               data = df_analisis, family = Gamma(link = "log"), 
               control = glmerControl(optimizer = "bobyqa"))

summary(model)
# Compute least squares means (LSMEANS)
lsmeans <- emmeans(model, ~ S*C)

# Plot mean plot
plot(lsmeans, by = "S", mean.plot = TRUE)

# Conduct slice difference tests
summary(lsmeans, infer = c(TRUE, TRUE))


###################################################################
#################################################################
######## overdispersion
library(blmeco)
library(nlme)
library(lme4)

df <- read.csv("table4.2.csv",header = TRUE)
head(df)
df_analisis<- df %>% 
  pivot_longer(cols = starts_with("H"),names_to = "trt",values_to = "count") 
  mutate(trt=as.factor(trt))

m1 <- glmer(count~1+trt+(1|Block),family = poisson(link = "log"),data = df_analisis)
summary(m1)


dispersion_glmer(m1)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
overdisp_fun(m1)
means_m1 <- emmeans(m1,~trt, type = "mu")
#####################################

m2<- glmer.nb(count~1+trt+(1|Block),family = poisson(link = "log"),data = df_analisis)
summary(m2)    

overdisp_fun(m2)
dispersion_glmer(m2)

emmeans(m2,~trt, type = "mu")

model <- lme(
  count ~ trt + (1 | Block) + random(1 | 1),  # Random intercept for residuals
  data = df_analisis
)
########Generalized Linear Mixed Models for Counts
######## the poisson model

#### CRD  with a poisson response

rm(list = ls()) ### clean the enviroment
library(tidyverse)
library(lme4)
library(nlme)
library(emmeans)
library(blmeco)
df <- read.csv("table5.1.csv",header = TRUE)
head(df)

datos<- df %>% select(nb=NB,sub=sub1,rep=Rep1) %>% mutate(sub=as.factor(sub))
m1 <- glm(nb~1+sub,family = poisson(link = "log"),data = datos)
m1_lm <- lm(nb~sub,data = datos)
means_m1 <- emmeans(m1,"sub")### comparations means this is like estimate 
means_m1_scale<- emmeans(m1, pairwise ~ "sub",type="mu")
df_comparations_means<- cld(means_m1_scale$emmeans ,adjust = "tukey",Letters = letters,decreasing = TRUE,alpha = 0.05)


ggplot(data = df_comparations_means,aes(x = sub, y = rate,group=sub))+
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="white")+
    scale_y_continuous(limits = c(0,60), breaks = seq(0,60,10))+
  # errorbars
  geom_errorbar(aes(
                  ymin = rate - SE,
                  ymax = rate + SE
                ),
                width = 0.1) +
  geom_text(
    aes(
      y = rate + SE,
      x = sub,
      label = str_trim(.group)
    ),
    hjust = 0.5,
    vjust = -0.5
  ) +
  # caption
  labs(x = "Avegere of shoots per plants",
       y = "Subcultures",
    caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", width = 70)
  )+
  theme_bw()

anova(m1_lm)
summary(m1)
#### overdiperstion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
overdisp_fun(m1)

#####################################################
###Example 2: CRDs with Poisson Response

rm(list = ls()) ### clean the enviromen
library(lme4)
library(nlme)
library(emmeans)
library(tidyverse)
df <- read.csv ("table5.5.csv",header = TRUE)
datos<- df %>% rename(count=yi,trt = Trt) %>%  mutate(trt=str_replace(trt,"Bl","B1")) %>% 
                mutate(trt=str_remove(trt,c("A"))) %>% 
                mutate(trt=str_remove(trt,c("B"))) %>% 
                mutate(trt=str_replace(trt,"C","4")) %>% 
mutate(count=as.numeric(count),trt=as.factor(trt),rep=as.factor(rep)) 


#Linear predictor: ηij = η þ τi þ rj
model <- glmer(count~trt+(1|rep),family = poisson(link = "log"),data = datos)
summary(model)
lsmean <- emmeans(model,specs = ~trt)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(model),3)

modelo2<- glmer.nb(count~(trt)+(1|rep),data = datos)
summary(modelo2)
emmeans(modelo2,"trt")
round(overdisp_fun(modelo2),3)
#######################################
##Example 3: Control of Weeds in Cereal Crops in an RCBD
rm(list = ls()) ### clean the enviromen
library(lme4)
library(nlme)
library(emmeans)
library(tidyverse)
df <- read.csv ("table5.11.csv",header = TRUE)
df <- df %>% mutate(trt=as.factor(trt))
modelo <- glmer(count~trt+(1|block),family = poisson(link = "log"),data = df)
summary(modelo)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(modelo),3)
emmeans(modelo,"trt")
emmeans(modelo,"trt",type = "mu")



modelo3<- glmmPQL(count~trt, random = ~1|block,family = "poisson",
        correlation=corAR1(form=~1|block), data= df)
emmeans(modelo3,~trt,type="response" )
summary(modelo3)


#proc glimmix method=laplace;
#class Block Trt;
#model Count = Trt/dist=Poisson; random intercept Trt/subject=block; lsmeans Trt/ ilink ;
#run;

modelo <- gls(count ~ trt, 
              data = df,
              weights = varIdent(form = ~1 | trt),
              correlation = corCompSymm(form = ~ 1 | block),
              method = "ML")

Exam3.9fm4   <-
  glmmPQL(count~trt
    , random      = ~1|trt
    , family      =  poisson
    , data        =  df
    , correlation =  corCompSymm(form=~1|trt)
    # , weights
    # , control
    , verbose     = TRUE
    # , ...
  )

library(MASS)
library(lme4)
mix <- glmer( count ~ block + trt + (1|block),
              family=MASS::negative.binomial(theta=1.75),
              data = df)

summary(mix)

library(MASS)
library(lme4)

emmeans(Exam3.9fm4,"trt")
modelo4<- glmer.nb(count~(trt)+(1|block),data = df)
emmeans(modelo4,"trt")
summary(modelo4)
overdisp_fun(modelo4)
#######Example: A 2 × 4 Factorial with a Poisson Response
rm(list = ls())
df <- read.csv("table5.19.csv",header = TRUE)
head(df)

datos<- df %>% pivot_longer(cols = starts_with("X"),names_to = "explant",values_to = "y") %>% 
                mutate(Genotype=as.factor(Genotype),Culture=as.factor(Culture),explant= str_remove(explant,"X")) %>% 
                mutate(explant=as.factor(explant))


modelo <- glmer(y~Genotype*Culture+(1|Petridish/explant),family = poisson(link = "log"),data = datos)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(modelo),3)
summary(modelo)

emmeans(modelo,specs = ~Genotype/Culture)

modelo2<- glmer.nb(y~Genotype*Culture+(1|Petridish/explant),data = datos)
summary(modelo2)
round(overdisp_fun(modelo2),3)
anova(modelo2)
emmeans(modelo2,specs = ~Genotype)
emmeans(modelo2,specs = ~Genotype*Culture)
comparacion_de_medias<- emmeans(modelo2,specs = ~Culture,type ="mu")%>% # get adjusted means for varieties
  cld(adjust="none", Letters=letters,decreasing = TRUE,alpha = 0.05)


ggplot(data = comparacion_de_medias,aes(x = Culture, y = response,group=Culture))+
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill="white")+
  scale_y_continuous(limits = c(0,16), breaks = seq(0,16,2))+
  # errorbars
  geom_errorbar(aes(
    ymin = response - SE,
    ymax = response + SE
  ),
  width = 0.1) +
  geom_text(
    aes(
      y = response + SE,
      x = Culture,
      label = str_trim(.group)
    ),
    hjust = 0.5,
    vjust = -0.5
  ) +
  # caption
  labs(x = "Avegere number of buds",
       y = "Culture medium",
       caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", width = 70)
  )+
  theme_classic()

comparacion_de_medias_int<- emmeans(modelo2,specs = ~Culture*Genotype,type = "response")%>% # get adjusted means for varieties
  cld(adjust="none", Letters=letters,decreasing = TRUE,alpha = 0.05) %>% as.data.frame()
    #unite(col = "trt",c("Genotype","Culture"),sep = "")

comparacion_de_medias_int

ggplot(data = comparacion_de_medias_int,aes(x = Culture, y = response,fill= Genotype))+
  geom_bar(stat = "identity", position = position_dodge(), color ="black")+
  scale_y_continuous(limits = c(0,18), breaks = seq(0,18,2))+
# errorbars
  geom_errorbar(aes(
    ymin = response - SE,
    ymax = response + SE
  ),
  width = 0.2, position=position_dodge(.9)) +
  geom_text(
    aes(
      y = response + SE,
      x = Culture,
      label = str_trim(.group)
    ), position=position_dodge(.9),
    hjust = 0.8,
    vjust = -0.8
  ) +
  # caption
  labs(x = "Culture medium",
       y = "Avegere number of buds",
       caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the LSD-test at the 5% level of significance.", width = 70)
  )+
  theme_classic()+  theme(legend.position = "top")+
  scale_fill_manual(values=c('#999999','gray'))
  
#################### LATIN SQUEARE

modelo <- glmer(count~trt+(1|row*column),family = poisson(link = "log"),data = datos)
emmeans(model, trt)


#################Randomized Complete Block Design in a Split Plot
##yijk =μþαi þrk þαðrÞik þβj þðαβÞij þεijk
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

df <- read.csv("table5.31.csv",header = TRUE)
head(df)
str(df)
datos <- df %>% 
            mutate(block=as.factor(Block),a=as.factor(A),b=as.factor(B),count=as.numeric(Count)) %>% 
  select(-Block,-A,-B,-Count)

head(datos)
#The following GLIMMIX program fits a split-plot block design with a Poisson response variable:

m1 <- glmer(count~a*b+(1|block/a),family = poisson(link = "log"),data = datos )
summary(m1)
emmeans(m1,~a*b)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m1),3)
car::Anova(m1,type ='III')
##### subject/a
m2 <- glmer.nb(count~a*b+(1|block/a),data = datos)
summary(m2)
round(overdisp_fun(m2),3)
anova(m1)
car::Anova(m2,type ='III')
emmeans(m2,~a*b)
glmer(count~a*b+(1|block/a),family = poisson(link = "log"),data = datos)

######################################################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(glmmTMB)
library(nlme)
library(AICcmodavg)
library(mixedup)

df <- read.csv("coffe_83.csv",header = TRUE)
head(df)

datos<- df %>% 
  pivot_longer(cols = starts_with(c("y","Y")),names_to = "time", values_to = "y" ) %>% 
        mutate(time= str_replace(time,"Y","y")) %>% 
        mutate(Shade=as.factor(Shade),
               Clone=as.factor(Clone),
               Tray=as.factor(Tray),
               Rep=as.factor(Rep),
              time=as.factor(time),
               y =as.numeric(y))

str(datos)
df1<- datos %>%
  mutate(date_densf = interaction(Shade, Clone,Tray))

#model y = shade|clone|tray|time/dist=poi link=log;
#random intercept shade shade*clone*tray/subject=rep type=ar(1) ; 
#lsmeans shade|clone|tray|time /lines ilink;
#run;

m2_alt <- glmmTMB(y ~ Shade*Clone*Tray*time+ar1(date_densf+ 0|Rep),family = poisson(link = "log"),data = df1)

summary(m2_alt)
### pendiente





##############################################################################################
######Generalized Linear Mixed Models for Proportions and Percentages


rm(list = ls()) ## clean the enviroment
library(tidyverse)## data managament
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
df <- read.csv("table6.1.csv",header = TRUE)

length(datos$trt)


rep(1:4,each = 6)

datos<- df %>% 
  pivot_longer(cols = starts_with("X"),names_to = "trt", values_to = "value") %>% 
    mutate(value=as.character(value)) %>% 
    mutate(value=gsub("[()]","_",value)) %>% 
    separate(value,into = c("x1","x2","x3"),sep = "_") %>% 
    rename(n=x1,y = x2) %>% 
    mutate(trt=str_remove(trt,"X")) %>% 
    mutate(trt=as.factor(trt),
           dose=as.numeric(as.character(trt)),
           n=as.numeric(n),
           y=as.numeric(y)) %>% 
          mutate(value=as.numeric(n/y)) %>% 
  dplyr::  select(-x3)    %>% 
  mutate(rep=rep(1:4,each = 6)) %>% 
  arrange(trt) %>% 
  mutate(rep=as.factor(rep))
 
str(datos)

## fi 6.1

 ggplot(data = datos,aes(x = trt, y = value))+
   geom_point()+
   theme_bw()

m1 <- glm(value~ trt,family=binomial(link="logit"),data = datos )
summary(m1)
emmeans(m1, "trt")
comparations_means<- emmeans(m1, specs = ~"trt",type = "response")
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m1),3)
anova(m1)


comparations_means %>%  # get adjusted means for varieties
    cld(adjust="none", Letters=letters,decreasing = TRUE,alpha = 0.05) %>% as.data.frame() %>% 
  ggplot(aes(x = trt, y = prob,fill= trt))+
  geom_bar(stat = "identity", position = position_dodge(), color ="black", fill='#999999')+
  #scale_y_continuous(limits = c(0,18), breaks = seq(0,18,2))+
  # errorbars
  geom_errorbar(aes(
    ymin =  prob - SE,
    ymax =  prob + SE
  ),
  width = 0.2, position=position_dodge(.9)) +
  # caption
  labs(x = "Dose",
       y = "Avegere proportion",
       caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the LSD-test at the 5% level of significance.", width = 70)
  )+
  theme_classic()

  
########################################################################
###########
str(datos)
datos

library(sjPlot)
m2 <- glm(value ~ dose,family=binomial,data = datos )

summary(m2)


overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m2),3)
p1<- plot_model(m2,type = "pred")
p1$data
z <- coef(m2)[1]+sum(coef(m2)[-1]*0.10)
1/(1 + exp(-z))

predict(m2, datos[ c("dose")], type="response")

############################################3
#Factorial Design in a Randomized Complete 
#Block Design (RCBD) with Binomial Data: Toxic Effect of Different Treatments on Two Species of Fleas
#(Appendix: Fleas).
rm(list = ls())
df <-  read.csv("fleas.csv",header = TRUE)
datos<- df %>% 
  mutate(sp=as.factor(SP),bioen=as.factor(Bioen),trt=as.factor(Treat),rep=as.factor(Rep),
         y=Dead/10,y=as.numeric(y)) %>%
  mutate(trt=str_replace(trt,"Tl","T1"),
         sp=str_replace(sp,"Da hnia","Daphnia")) %>% 
  mutate(trt=as.factor(trt),sp=as.factor(sp)) %>% 
  dplyr:: select(-SP,-Bioen,-Treat,-Rep)

as.data.frame(datos)

levels(datos$trt)


 model <- glmer(y~trt*sp+(1|bioen/sp/rep),family = binomial,data = datos)
summary(model)
emmeans(model,specs = "trt")


#####################################################################3
#####################################################################
library(tidyverse)
library(lme4)
library(emmeans)



df <- read.csv("carriots.csv",header = TRUE)
head(df)
datos<- df %>% 
  pivot_longer(cols = starts_with("B"),names_to = "block",values_to = "y") %>% 
    mutate(trt=case_when(block == "Block4"~"2",
                         block == "Block5"~ "2",
                         block == "Block6"~ "2",
                         T~"1")) %>% 
  mutate(block=case_when(block == "Block4"~"Block1",
                       block == "Block5"~ "Block1",
                       block == "Block6"~ "Block1",
                       T~block)) %>% 
  separate(col = y, into = c("y","n"),sep = "/") %>% 
  mutate(block=as.factor(block), Genotype=as.factor(Genotype),trt=as.factor(trt),y =as.numeric(y), n = as.numeric(n)) %>% 
  mutate(value=round(as.numeric(y/n),2))
datos
str(datos)

m1 <- glmer(y/n~Genotype*trt+(1|block/trt),family = binomial,data = datos)
summary(m1)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m1),4)

anova(m1)

m2 <- glmer(y/n~trt+Genotype/trt+(1|block/trt),family = binomial,data = datos)
summary(m2)
anova(m2)
round(overdisp_fun(m2),2)


library(glmmTMB)

m3<- glmmTMB(value~ trt+Genotype/trt+(1|block/trt), data = datos, family = betabinomial(link = "logit"))
anova(m3)
summary(m3)
overdisp_fun(m3)
emmeans(m2,"Genotype",type="mu")

m4 <- lmer(value~ trt+Genotype/trt+(1|block/trt), data = datos)
summary(m4)
anova(m4)
means_c<- emmeans(m4,specs = ~Genotype, type = "response")

ggplot(means_c,aes(x = ))

#############################Percentages
rm(list = ls())
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcomp)
df <- read.csv("table6.29.png.csv",header = TRUE)
head(df)
datos <-  df %>%  
                mutate(Plant=as.factor(Plant),Density=as.factor(Density))

m1<- glmmTMB(Pij~ Density+(1|Density:Plant), data = datos, family = betabinomial(link = "logit"))
summary(m3)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m1),4)
emmeans(m1,~Density)
emmeans(m1,~Density, type = "response")%>% # get adjusted means for varieties
  cld(adjust="none", Letters=letters,decreasing = TRUE,alpha = 0.05) %>% 
ggplot(  mapping = aes(x = Density, y = prob,group =  Density)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill='#999999') +
  # errorbars
  geom_errorbar(aes(
    ymin = prob  - SE,
    ymax = prob + SE
  ),
  width = 0.1) +
  geom_text(
    aes(
      y =  prob + SE,
      x =  Density,
      label = str_trim(.group)
    ),
    hjust = 0.5,
    vjust = -0.5
  ) +
  # caption
  labs(x = "Conidial density",
       y = "Proportion",
       caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", width = 70)
  )+
  theme_classic()


#############################################
rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)

df <- read.csv("table6.33.csv",header = TRUE)
head(df)
datos<- df %>% mutate(Variety=as.factor(Variety),Time=as.factor(Time),Block=as.factor(Block),p =y/100)
m1<- glmmTMB(p~ Variety+Variety:Time+(1|Block), data = datos, family = betabinomial(link = "logit"))
summary(m1)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m1),4)

emmeans(m1,specs = ~ Variety)
emmeans(m1,specs = ~ Variety,type = "response")%>% # get adjusted means for varieties
  cld(adjust="none", Letters=letters,decreasing = TRUE,alpha = 0.05) %>% 
  ggplot(  mapping = aes(x = Variety, y = prob,group = Variety)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black", fill='#999999') +
  # errorbars
  geom_errorbar(aes(
    ymin = prob  - SE,
    ymax = prob + SE
  ),
  width = 0.1) +
  geom_text(
    aes(
      y =  prob + SE,
      x =  Variety,
      label = str_trim(.group)
    ),
    hjust = 0.5,
    vjust = -0.5
  ) +
  # caption
  labs(x = "Variety",
       y = "Proportion",
       caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", width = 70)
  )+
  theme_classic()


###############
rm(list = ls())
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcomp)
df <- read.csv("table6.41.csv",header = TRUE)
head(df)

datos <- df %>% mutate(Block=as.factor(Block),Variety=as.factor(Variety),Dose =as.factor(Dose), y =  y/100)
head(datos)


m1<- glmmTMB(y~ Dose*Variety+(1|Block/Dose), data = datos, family = betabinomial(link = "logit"))
summary(m1)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}### OPTION 1
round(overdisp_fun(m1),4)

lsmeans_model <- emmeans(m1,~Dose)

t1<- c(1,0,0,0)
t2 <- c(0,1,0)
t3 <- c(0,0,1)


contr<- list("Linear dose" = c(3,-1,1, 3),
             "quadratic dose" = c(1,-1,-1,-1),
             "dose cubic" = c(-1,3,-3,-1))
emm<- contrast(lsmeans_model, method = contr,type="response")

library(sjPlot)



emmeans(m1,~Variety/Dose, type = "response")%>% 
  as.data.frame() %>% # get adjusted means for varieties
  ggplot( mapping = aes(x = Dose, y = prob,group =  Variety,color= Variety)) +
  geom_point()+
  geom_line()
  

###Generalized Linear Mixed Models
#for Categorical and Ordinal Responses

# (clean the enviroment 
library(tidyverse)
rm(list = ls())
df <- read.csv("table8.1.csv",header = TRUE)
df
head(df)
 datos<- df %>% pivot_longer(cols = starts_with("X"),names_to = "cat", values_to = "pl") %>% 
   mutate(cat=str_remove(cat,"X"),cat = round(as.numeric(cat))) %>% 
 mutate(H = "H", M = "M" ) %>% 
  unite("Ma",M,m,sep = "") %>% 
  unite("ha",H,f,sep = "") %>% 
  unite("Cross",Ma,ha,sep = "") %>% 
   mutate(rep =rep(1:4,time = 12, each = 3)) %>%
 mutate(cat=case_when(cat==1~"Without",
                         cat==2~"Moderate",
                        cat==3~"Severe"))%>% 
  dplyr::select(rep,trt=Cross,cat, freq=pl) %>% 
  group_by(trt,rep,cat) %>% 
  mutate(cat=as.factor(cat),trt=as.factor(trt),rep=as.factor(rep))

head(datos)
   

 dat <- data.frame("cat"=rep(datos$cat,times = datos$freq),"trt"=rep(datos$trt,times = datos$freq),
                   rep = rep(datos$rep,times = datos$freq))



m1 <- vglm(cat~+trt,data = dat, family = multinomial(refLevel = "Without"))
summary(m1)
c1<- exp(0.77319 +(-0.14458))
c2 <- exp(1.20397+(-0.57536))

p1<- 1-(c1+c2)/(1+c1+c2)
p1
p2 <- c1/(1+c1+c2)
p2
p3 <- c2/(1+c1+c2)
p3
nw <- data.frame("trt"=levels(as.factor(datos$trt))) 
predict(m1, nw, type = "response")
m3 <- clm(cat~trt,data = dat)
summary(m3)
predict(m3, nw, type = "prob")

###########################################################
####  
rm(list = ls())
df <- read.csv("table8.11.csv",header = TRUE)
head(df)
datos <- data.frame("cat"=rep(df$Category, times = df$Frequency),"trt"= rep(df$Trt, times = df$Frequency))
datos$cat <- as.factor(datos$cat)
datos$trt <- as.factor(datos$trt)
head(datos)
str(datos)
levels(datos$cat)
m1 <- vglm(cat~trt,data = datos, family = multinomial(refLevel = "Without"))
summary(m1)
nw <- data.frame("trt"=levels(as.factor(datos$trt))) 
str(nw)



m2 <- clm(cat~trt,data = datos)
lsmeans_model <- emmeans(m2,~trt)

t1<- c(1,0,0,0)
t2 <-c(0,1,0,0)
t3 <-c(0,0,1,0)
t4<- c(0,0,0,1)

contr<- list("trt1 vs trt4" = c(1,0,0,-1),
             "trt2 vs trt4" = c(0,1,0,1),
             "trt3 vs trt4" = c(0,0,1,1))
emm<- contrast(lsmeans_model, method = contr)




emmeans(m2,"trt")
summary(m2)
predict(m2, nw, type = "prob")
plot_datos<-as.data.frame(predict(m2,nw,type = "prob"))
plot_datos$trt <- levels(as.factor(datos$trt))
plot_datos<- plot_datos %>% 
  pivot_longer(cols = 1:3, names_to = "cat", values_to = "prob") %>% 
  mutate(cat=str_remove(cat,"fit."))

plot_datos$cat <- factor(plot_datos$cat, levels = c("Without","Slight","Severe"))
plot_datos$prob <- round(plot_datos$prob,3)

ggplot(data = plot_datos,aes(x=trt, y = prob,fill = cat))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = prob),vjust=1.9,
            position = position_dodge(.9))+
  scale_fill_manual(values=c("gray82",'gray','#999999'))+
  labs(x = "Treatment", y = "Pobability of lesion")+
  theme_classic()+
  theme(legend.position = "top")

  theme_bw()

  
  m3 <- vglm(cat~trt,data = datos, family = cumulative(link = "logitlink") )
summary(m3)  
predict(m3,nw,type = "response")

?vglm
