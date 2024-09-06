# Yulia N. Muzychenko
# yulia.n.muzychenko@gmail.com
# summer 2024
#
#Script 2/2


#Complete Case Analysis



##Install packages
install.packages("MVN")


#Libraries
library(MVN)
library(readxl)
library(dplyr)


# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

# Loading the data after the data cleaning
df = read_excel(path = '4unies_quantitative_no_sds_clean_noid.xlsx')

# 1.\ Check multivariate normality ####
MVN::mvn(df[,c(1:6,43:65,71:74)], mvnTest = "mardia",univariateTest ="SW")
MVN::mvn(df[,c(1:6,43:65,71:74)], mvnTest = "hz",univariateTest ="SW")
MVN::mvn(df[,c(1:6,43:65,71:74)], mvnTest = "royston",univariateTest ="SW")
MVN::mvn(df[,c(1:6,43:65,71:74)], mvnTest = "dh",univariateTest ="SW")
MVN::mvn(df[,c(1:6,43:65,71:74)], mvnTest = "energy",univariateTest ="SW", multivariatePlot="qq")

MVN::mvn(imp_n1[,c(86:89,92:103)], mvnTest = "mardia",univariateTest ="SW")


mvn(data = df2[,c(1:6,43:65,71:74)], mvnTest = "hz", multivariateOutlierMethod = "quan")

#### Inverse decoding #################
# ES - Emotional Stability [10 items]
# ES - items 1,2,3,4,6,7,8,9 are reverse coded
# SOC - Sense of Coherence [13 items]
# SOC- items 1,2,3,7,10 are reverse coded
# AS - Assignment Satisfaction [3 items]
# AS - item 2 is reverse coded

# For As, only the second item
df$as2<-recode(df$as2,'1'=5,'2'=4,'4'=2,'5'=1)

# Es items all except for 5 and 10.
df$es1<-recode(df$es1,'1'=5,'2'=4,'4'=2,'5'=1)
df$es2<-recode(df$es2,'1'=5,'2'=4,'4'=2,'5'=1)
df$es3<-recode(df$es3,'1'=5,'2'=4,'4'=2,'5'=1)
df$es4<-recode(df$es4,'1'=5,'2'=4,'4'=2,'5'=1)
df$es6<-recode(df$es6,'1'=5,'2'=4,'4'=2,'5'=1)
df$es7<-recode(df$es7,'1'=5,'2'=4,'4'=2,'5'=1)
df$es8<-recode(df$es8,'1'=5,'2'=4,'4'=2,'5'=1)
df$es9<-recode(df$es9,'1'=5,'2'=4,'4'=2,'5'=1)

# For sense of coherence
df$soc1<-recode(df$soc1,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc2<-recode(df$soc2,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc3<-recode(df$soc3,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc7<-recode(df$soc7,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc10<-recode(df$soc10,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)

#For stress scale 4,5,7,8
df$pss4<-recode(df$pss4,'0'=4,'1'=3,'3'=1,'4'=0)
df$pss5<-recode(df$pss5,'0'=4,'1'=3,'3'=1,'4'=0)
df$pss7<-recode(df$pss7,'0'=4,'1'=3,'3'=1,'4'=0)
df$pss8<-recode(df$pss8,'0'=4,'1'=3,'3'=1,'4'=0)

#### Levene's test to see if we can join datasets.####

install.packages("lawstat")
library(lawstat)
levene.test(df$as1, df$School)
levene.test(df$cca1, df$School)
levene.test(df$ccm1, df$School)
levene.test(df$es1, df$School)
levene.test(df$soc1, df$School)

####CFA####
library(remotes)
remotes::install_github("yrosseel/lavaan")
library(lavaan)

#Final model5.
#One item was deleted to increase the model fit.
total_model_5<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                A =~ a1+a2+a3+a4
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                '
fit_total_5 <- cfa(total_model_5, data = df, estimator = "MLR")
summary(fit_total_5,fit.measures = TRUE)
standardizedSolution(fit_total_5)
AVE(fit_total_5)
compRelSEM(fit_total_5)
modindices(fit_total_5, sort = TRUE, maximum.number = 10)
reliability(fit_total_5)

###CMB###
model_CMB<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9 +
                        soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4
method=~es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10 +
soc4 + soc6 + soc8 + soc9 + soc11 + soc12 + soc13 +
ccm1 + ccm2 + ccm3 + ccm4 + ccm5 + cca1 + cca2 + cca3 + as1 + as2 + as3+
a1+a2+a3+a4
method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
'
fit_CMB <- cfa(model_CMB, data = df2, estimator = "MLR")

#The zero-constrained test ####

model_CMB_0<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4
method=~0*es1 + 0*es2 + 0*es3 + 0*es4 + 0*es5 + 0*es6 + 0*es7 + 0*es8 + 0*es9 + 0*es10 +
0*soc4 + 0*soc6 + 0*soc8 + 0*soc9 + 0*soc11 + 0*soc12 + 0*soc13 +
0*ccm1 + 0*ccm2 + 0*ccm3 + 0*ccm4 + 0*ccm5 + 0*cca1 + 0*cca2 + 0*cca3 + 0*as1 + 0*as2 + 0*as3 +
0*a1+0*a2+0*a3+0*a4

method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
method ~~ 1*method'
fit_CMB_0 <- cfa(model_CMB_0, data = df2, estimator = "MLR")


# To check if the zero-constrained chi-square difference test resulted in a significant result####
out<-compareFit(fit_CMB_0,  fit_CMB, nested = FALSE)
summary(out)

# (i.e., reject null, i.e., response bias is not zero), we should run an equal-constrained test. 
# This test determines whether the response bias is evenly distributed across factors.

#The equal-constrained test ####
model_CMB_1<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4

method=~1*es1 + 1*es2 + 1*es3 + 1*es4 + 1*es5 + 1*es6 + 1*es7 + 1*es8 + 1*es9 + 1*es10 +
1*soc4 + 1*soc6 + 1*soc8 + 1*soc9 + 1*soc11 + 1*soc12 + 1*soc13 +
1*ccm1 + 1*ccm2 + 1*ccm3 + 1*ccm4 + 1*ccm5 + 1*cca1 + 1*cca2 + 1*cca3 + 1*as1 + 1*as2 + 1*as3 +
1*a1+1*a2+1*a3+1*a4
method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
'
fit_CMB_1 <- cfa(model_CMB_1, data = df, estimator = "MLR")

#To check is the test of equal specific bias demonstrated unevenly distributed bias.####
out<-compareFit(fit_CMB_1,  fit_CMB, nested = FALSE)
summary(out)

compRelSEM(fit_CMB)
compRelSEM(fit_total_5)
reliability(fit_total_5)
reliability(fit_CMB)

#####clean the dataset from the rows with na is factor items of interest####
df2<-df
df2<-df2[!is.na(df2$a1),]
df2<-df2[!is.na(df2$a2),]
df2<-df2[!is.na(df2$a3),]
df2<-df2[!is.na(df2$a4),]
df2<-df2[!is.na(df2$as1),]
df2<-df2[!is.na(df2$as2),]
df2<-df2[!is.na(df2$as3),]
df2<-df2[!is.na(df2$cca1),]
df2<-df2[!is.na(df2$cca2),]
df2<-df2[!is.na(df2$cca3),]
df2<-df2[!is.na(df2$a1),]
df2<-df2[!is.na(df2$a2),]
df2<-df2[!is.na(df2$a3),]
df2<-df2[!is.na(df2$a4),]
df2<-df2[!is.na(df2$es1),]
df2<-df2[!is.na(df2$es2),]
df2<-df2[!is.na(df2$es3),]
df2<-df2[!is.na(df2$es4),]
df2<-df2[!is.na(df2$es5),]
df2<-df2[!is.na(df2$es6),]
df2<-df2[!is.na(df2$es7),]
df2<-df2[!is.na(df2$es8),]
df2<-df2[!is.na(df2$es9),]
df2<-df2[!is.na(df2$es10),]
df2<-df2[!is.na(df2$soc4),]
df2<-df2[!is.na(df2$soc6),]
df2<-df2[!is.na(df2$soc8),]
df2<-df2[!is.na(df2$soc9),]
df2<-df2[!is.na(df2$soc11),]
df2<-df2[!is.na(df2$soc12),]
df2<-df2[!is.na(df2$soc13),]

summary(df$School)

#Dummification
df2$School1_2<-ifelse(df2$School==2,1,0)
df2$School1_3<-ifelse(df2$School==3,1,0)
df2$School1_4<-ifelse(df2$School==4,1,0)

#Extract latent factors' values

fit_imp1 <- lavaan::cfa(fit_total_5, data = df2, estimator = "MLR")
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
df2<-cbind(df2,fscores)


#### RUN SEM####
####Mediation####
model_mediation <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender
A ~ b*ES + c*SOC + c13*gender + c33*ses
CCA ~ d*ES + e*SOC + f*A
+ c14*gender + c24*language + c34*ses + c44*cctraindue + c54*age
AS ~ g* ES + h*SOC + i*CCA + j*A  
+ c15*gender + c25*language + c35*ses + c45*cctraindue + c55*age + c65*School1_2+ 
c75*School1_3+c85*School1_4


#idirect via A
indirect_ES_CCA := b*f
indirect_SOC_CCA := c*f
#ind_ES_SOC_A_CCA := a*c*f
ind_ES_EdS := b*j
ind_SOC_EdS := c*j
#ind_ES_SOC_A_EdS := a*c*j
ser_ES_A_CCA_EdS := b*f*i
ser_SOC_A_CCA_EdS := c*f*i
ind_ES_SOC_A_CCA_EdS := a*c*f*i
'

sem_mediation <- sem(model_mediation, data = df2, estimator = "MLR")
summary(sem_mediation,fit.measures = TRUE )
standardizedsolution(sem_mediation)

# Check if we have the same path coefficients in our model
# lwt() function is taken from here: https://osf.io/5xaph/

constr <- 'b==c'
lwt(sem_mediation, constraints = constr, verbose = FALSE, std = TRUE)
constr <- 'f==j'
lwt(sem_mediation, constraints = constr, verbose = FALSE, std = TRUE)
constr <- 'indirect_ES_CCA==indirect_SOC_CCA'
lwt(sem_mediation, constraints = constr, verbose = FALSE, std = TRUE)
constr <- 'ind_ES_EdS==ind_SOC_EdS'
lwt(sem_mediation, constraints = constr, verbose = FALSE, std = TRUE)
constr <- 'ser_ES_A_CCA_EdS==ser_SOC_A_CCA_EdS'
lwt(sem_mediation, constraints = constr, verbose = FALSE, std = TRUE)



model_mediation_wo_controls <- '
# regression
SOC~a*ES
A ~ b*ES + c*SOC 
CCA ~ d*ES + e*SOC + f*A
AS ~ g* ES + h*SOC + i*CCA + j*A  

#idirect via A
indirect_ES_CCA := b*f
indirect_SOC_CCA := c*f
#ind_ES_SOC_A_CCA := a*c*f
ind_ES_EdS := b*j
ind_SOC_EdS := c*j
#ind_ES_SOC_A_EdS := a*c*j
ser_ES_A_CCA_EdS := b*f*i
ser_SOC_A_CCA_EdS := c*f*i
ind_ES_SOC_A_CCA_EdS := a*c*f*i
'
sem_mediation_wo_controls <- sem(model_mediation_wo_controls, data = df2, estimator = "MLR")
summary(sem_mediation_wo_controls,fit.measures = TRUE )
standardizedsolution(sem_mediation_wo_controls)



#Compare models with and without controls
out<-compareFit(sem_mediation,  sem_mediation_wo_controls, nested = FALSE)
summary(out)

