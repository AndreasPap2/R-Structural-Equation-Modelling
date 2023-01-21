# Load the packages 

### DATA MANIPULATION ###
library("haven")        
library("dplyr")      
library("psych")
library('stringr')

### MODELING ###
library("lavaan")       

### VISUALIZATION ###
library("corrplot")     
library("tidySEM")
library("ggplot2")              
library("patchwork")  
library("semPlot")


#importing data set
data.full <-read.csv(file = file.choose(), header = TRUE)

ess_df<-data.full
head(ess_df, 20)  # first 20 rows of the dataset
nrow(ess_df)      # number of subjects 
ncol(ess_df)      # number of variables 
names(ess_df)     # names of variables


###### Select the relevant variables#####

ess_df_selected <- ess_df %>% select(
  ## Confidence about politics 4 indicators ##
  psppsgva, #
  actrolga, #
  psppipla, #
  cptppola, #
  #polcmpl, # is N/a

  ## Trust in government mechanisms  7 indicators ##
  trstprl, #
  trstlgl, #
  trstplc,#
  trstplt,#
  trstprt,#
  trstun, #
  trstep,#
  ##    Human Values 7 indicators##
  #imprich, #
  ipeqopt, #
  impsafe,#
  #ipfrule,#
  impfree,#
  ipstrgv,#
  imptrad,#
)


# Discriptive statistics

descriptive_ess <- as.data.frame(psych::describe(ess_df_selected))

descriptive_ess <- dplyr::select(descriptive_ess,
                                 n,
                                 mean,
                                 sd,
                                 median,
                                 min,
                                 max,
                                 skew,
                                 kurtosis)

descriptive_ess

####  ######

ess_df2<-ess_df_selected





################# checking for non normality #####################

# First let's visually inspect our variables 


h_psppsgva <- ggplot(ess_df2, aes(psppsgva)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_actrolga <- ggplot(ess_df2, aes(actrolga)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_psppipla <- ggplot(ess_df2, aes(psppipla)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_cptppola <- ggplot(ess_df2, aes(cptppola)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

#### 2nd #####

h_trstprl <- ggplot(ess_df2, aes(trstprl)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_trstlgl <- ggplot(ess_df2, aes(trstlgl)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_trstplc <- ggplot(ess_df2, aes(trstplc)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_trstplt <- ggplot(ess_df2, aes(trstplt)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_trstprt <- ggplot(ess_df2, aes(trstprt)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_trstun <- ggplot(ess_df2, aes(trstun)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_trstep <- ggplot(ess_df2, aes(trstep)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)



############# 3rd ###########################


# h_imprich <- ggplot(ess_df2, aes(imprich)) +
#   geom_blank() +
#   geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_ipeqopt <- ggplot(ess_df2, aes(ipeqopt)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_impsafe <- ggplot(ess_df2, aes(impsafe)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

# h_ipfrule <- ggplot(ess_df2, aes(ipfrule)) +
#   geom_blank() +
#   geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_impfree <- ggplot(ess_df2, aes(impfree)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_ipstrgv <- ggplot(ess_df2, aes(ipstrgv)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)

h_imptrad <- ggplot(ess_df2, aes(imptrad)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", alpha=0.3)


###### plotting the univariate distrubution #######

h_psppsgva+h_actrolga+h_psppipla+h_cptppola+h_trstprl+h_trstlgl+h_trstplc+h_trstplt+h_trstprt+h_trstun+h_trstep+h_ipeqopt+h_impsafe+h_impfree+h_ipstrgv+h_imptrad
  





# Calculating variance-Covariance matrix for the items that we are going to use in the CFA
# 
# let's select the human values items
#########
###           CHECKED FOR ONE FACTOR, DOWN ARE 3-FACTOR CFA ETC...
#########
ess_df_human_values <- ess_df2 %>% select(
  ##    Human Values 7 indicators##
  #imprich, #
  ipeqopt, #
  impsafe,#
  #ipfrule,#
  impfree,#
  ipstrgv,#
  imptrad,#
)

# let's calculate the sample implied covariance matrix
human_values_cov <- cov(ess_df_human_values,          # data frame
                        use = "pairwise.complete.obs" # remove NAs
)

human_values_cov

#Correlation matrix

human_values_cor <- cov2cor( human_values_cov)
human_values_cor


corrplot::corrplot(human_values_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

##    trust in government mechanicms 7 indicators##

ess_df_gov_mech <- ess_df2 %>% select(
  ##    government mechanicms 7 indicators##
  trstprl, #
  trstlgl, #
  trstplc,#
  trstplt,#
  trstprt,#
  trstun, #
  trstep,#
)

# let's calculate the sample implied covariance matrix
ess_df_gov_mech_cov <- cov(ess_df_gov_mech,          # data frame
                        use = "pairwise.complete.obs" # remove NAs
)

ess_df_gov_mech_cov

#Correlation matrix

ess_df_gov_mech_cor <- cov2cor(ess_df_gov_mech_cov)
ess_df_gov_mech_cor


corrplot::corrplot(ess_df_gov_mech_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)


##    trust in confidence in politics  4 indicators##

ess_df_conf <- ess_df2 %>% select(
  ##   confidence in politics 4 indicators##
  psppsgva, #
  actrolga, #
  psppipla, #
  cptppola, #
)

# let's calculate the sample implied covariance matrix
ess_df_conf_cov <- cov(ess_df_conf,          # data frame
                           use = "pairwise.complete.obs" # remove NAs
)

ess_df_conf_cov

#Correlation matrix

ess_df_conf_cor <- cov2cor(ess_df_conf_cov)
ess_df_conf_cor


corrplot::corrplot(ess_df_conf_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)






################## conduct 3 CFA   ###################################

#CFA Human Values

cfa.hv<-'human_values =~  ipeqopt + impsafe + impfree + ipstrgv + imptrad
      impsafe ~~ imptrad
       ipeqopt ~~ ipstrgv
     
       '

fit_political_values<-cfa(cfa.hv,data=ess_df2, missing = "direct",estimator = "MLR"  )
summary(fit_political_values,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE,modindices= TRUE)
parameterEstimates(fit_political_values)


#path diagram of the model
library (semPlot)

semPaths(fit_political_values,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=,marc=c(10,1,2,1))

# CFA Confidence for politics
cfa.confidence<-'conf_pol =~ psppsgva + actrolga + psppipla + cptppola
                  psppsgva ~~ psppipla
                  actrolga ~~ cptppola'

fit_conf_pol <- cfa(cfa.confidence,data=ess_df2, missing = "direct",estimator = "MLR"  )
summary(fit_conf_pol,fit.measures=TRUE,standardized=TRUE,modindices= TRUE)
parameterEstimates(fit_conf_pol)


#path diagram of the model
semPaths(fit_conf_pol,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=,marc=c(10,1,2,1))


# CFA Trust in government mechanisms  6 indicators

cfa.trust<-'govern_mechs =~ trstprl + trstlgl + trstplc + trstplt + trstprt + trstun
            trstlgl ~~ trstplc
            trstplt ~~ trstprt
            trstprl ~~  trstun'

fit_govern_mechs <-cfa(cfa.trust,data=ess_df2, missing = "direct",estimator = "MLR" )
summary(fit_govern_mechs,fit.measures=TRUE,standardized=TRUE,modindices= TRUE)
parameterEstimates(fit_govern_mechs)


#path diagram of the model
semPaths(fit_govern_mechs,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=,marc=c(10,1,2,1))

################# STRUCTURAL EQUATION MODEL  ######################################

sem1 <- 'human_values =~  ipeqopt + impsafe + impfree + ipstrgv+imptrad
         conf_pol =~ psppsgva + actrolga +cptppola
        govern_mechs =~ trstprl + trstlgl + trstplc + trstplt + trstprt + trstun

       ipeqopt ~~ ipstrgv
       impsafe ~~ imptrad
        actrolga ~~ cptppola
         trstlgl ~~ trstplc
         trstplt ~~ trstprt
        trstprl ~~  trstun'

#  psppsgva ~~ psppipla  impsafe ~~ imptrad

fit.sem1<-sem(model=sem1,data=ess_df2, missing = "direct",estimator = "MLR")
semPaths(fit.sem1, layout = "spring", whatLabels = "std")
summary(fit.sem1,standardized=TRUE,fit.measures=TRUE)
modificationindices(fit.sem1, sort. = TRUE)


