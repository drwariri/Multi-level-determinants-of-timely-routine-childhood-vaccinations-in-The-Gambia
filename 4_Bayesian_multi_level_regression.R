library(tidyverse)
library(gtools)
library(INLA)
library(janitor)
library(questionr)
library(summarytools)
library(gmodels)
library(car)
library(dplyr)
library(pROC)
library(plotROC)

set.seed(500)

#Set working directory
setwd("C:/filepath")

### read data into R
outcome_data_obj3_final <- read_csv("data.csv")

# Subset the data to include only children aged 12 to 35 months
subsetted_data <- subset(outcome_data_obj3_final, age_in_month >= 12 & age_in_month <= 35)


# Recode categories "01-Mar" to "1-3" and "4 or more" to "≥4"
subsetted_data$parity_cat <- ifelse(subsetted_data$parity_cat == "01-Mar", "1-3", 
                                         ifelse(subsetted_data$parity_cat == "4 or more", "≥4", subsetted_data$parity_cat))

# Recode mat_age_cat based on the specified age ranges and overwrite the existing variable
subsetted_data$mat_age_cat <- cut(subsetted_data$mat_age,
                                  breaks = c(15, 20,30, 40, Inf),
                                  labels = c("<=19", "20-29", "30-39", "40-49"),
                                  include.lowest = TRUE, right = FALSE)

# Recode birth_order_cat based on the specified birth_order and overwrite the existing variable
subsetted_data$birth_order_cat <- cut(subsetted_data$birth_order,
                                      breaks = c(-Inf, 3, 6, Inf),
                                      labels = c("1-2", "3-5", ">5"),
                                      include.lowest = TRUE, right = FALSE)

# Recode mat_educ_cat based on the specified mat_educ and create a new variable
subsetted_data$mat_educ_cat <- ifelse(subsetted_data$mat_educ %in% c("No education"), "No education",
                                ifelse(subsetted_data$mat_educ %in% c("Primary"), "Primary",
                                       ifelse(subsetted_data$mat_educ %in% c("Secondary", "Higher"), "Secondary/Higher", NA)))

# Create ethnicity_cat based on the specified ethnicity
subsetted_data$ethnicity_cat <- ifelse(subsetted_data$ethnicty == "Fula/Tukulur/Lorobo", "Fula/Tukulur/Lorobo",
                                  ifelse(subsetted_data$ethnicty == "Mandinka/Jahanka", "Mandinka/Jahanka",
                                         ifelse(subsetted_data$ethnicty == "Non-Gambian", "Non-Gambian",
                                                ifelse(subsetted_data$ethnicty == "Sarahule", "Sarahule",
                                                       ifelse(subsetted_data$ethnicty == "Wollof", "Wollof",
                                                              ifelse(subsetted_data$ethnicty %in% c("Bambara", "Creole/Aku Marabout", "Jola/Karoninka", 
                                                                                               "Manjago", "Serere", "Other"), "Others", NA))))))
# Create region_cat based on the region variable
subsetted_data <- subsetted_data %>%
  mutate(region_cat = case_when(
    region %in% c("Banjul", "Kanifing") ~ "Greater Banjul",
    region %in% c("Brikama") ~ "Brikama",
    region %in% c("Kerewan") ~ "Kerewan",
    region %in% c("Mansakonko") ~ "Mansakonko",
    region %in% c("Kuntaur","Janjanbureh", "Basse") ~ "Other regions",
    TRUE ~ region))

# Recode tt_fixed_mixed_cat from 5 to 3 categories
subsetted_data <- subsetted_data %>%
  mutate(tt_fixed_mixed_cat_new = case_when(
    tt_fixed_mixed_cat %in% c("<15 mins", "15 - <30 mins") ~ "<30 mins",
    tt_fixed_mixed_cat %in% c("60 mins and above") ~ "60 mins and above",
    tt_fixed_mixed_cat %in% c("30 - <45 mins", "45 - <60 mins") ~ "30 - <60 mins",
    TRUE ~ tt_fixed_mixed_cat))


# Recode tt_fixed_walking_cat from 5 to 3 categories
subsetted_data <- subsetted_data %>%
  mutate(tt_fixed_walking_cat_new = case_when(
    tt_fixed_walking_cat %in% c("<15 mins", "15 - <30 mins") ~ "<30 mins",
    tt_fixed_walking_cat %in% c("60 mins and above") ~ "60 mins and above",
    tt_fixed_walking_cat %in% c("30 - <45 mins", "45 - <60 mins") ~ "30 - <60 mins",
    TRUE ~ tt_fixed_walking_cat))


# Recode years_residence_cat
subsetted_data <- subsetted_data %>%
  mutate(years_residence_cat = case_when(
      years_residence_cat %in% c("1-3 years") ~ "1-3 years",
      years_residence_cat %in% c("4-5 years") ~ "4-5 years",
      years_residence_cat %in% c(">5 years") ~ ">5 years",
      years_residence_cat %in% c("<1 year", "Visitor") ~ "<1 year",
      TRUE ~ years_residence_cat))

# Create wealth_index based on the wealth_index_urb_rural variable
subsetted_data <- subsetted_data %>%
  mutate(wealth_index = case_when(
    wealth_index_urb_rural %in% c("Poorer","Poorest") ~ "Poor",
    wealth_index_urb_rural %in% c("Middle") ~ "Middle",
    wealth_index_urb_rural %in% c("Richer","Richest") ~ "Rich",
    TRUE ~ wealth_index_urb_rural))

# Create a new variable 'all_timely'. Assign 1 if all five outcome variables have 1, NA is all five variables have NA and 0, if otherwise
subsetted_data$all_timely <- NA
subsetted_data$all_timely[rowSums(subsetted_data[c("hepB0_timely", "penta1_timely", "penta2_timely", "penta3_timely", "mcv1_timely")] == 1, na.rm = TRUE) == 5] <- 1
subsetted_data$all_timely[rowSums(is.na(subsetted_data[c("hepB0_timely", "penta1_timely", "penta2_timely", "penta3_timely", "mcv1_timely")]) == TRUE) == 5] <- NA
subsetted_data$all_timely[rowSums(subsetted_data[c("hepB0_timely", "penta1_timely", "penta2_timely", "penta3_timely", "mcv1_timely")] == 0, na.rm = TRUE) >= 1] <- 0

# Create a new variable 'return_timely'. Assign 1 if all five outcome variables have 1, NA is all five variables have NA and 0, if otherwise
subsetted_data$return_timely <- NA
subsetted_data$return_timely[rowSums(subsetted_data[c("penta1_timely", "penta2_timely", "penta3_timely")] == 1, na.rm = TRUE) == 3] <- 1
subsetted_data$return_timely[rowSums(is.na(subsetted_data[c("penta1_timely", "penta2_timely", "penta3_timely")]) == TRUE) == 3] <- NA
subsetted_data$return_timely[rowSums(subsetted_data[c("penta1_timely", "penta2_timely", "penta3_timely")] == 0, na.rm = TRUE) >= 1] <- 0


#select variables needed for the analysis
dhs_dat <- subsetted_data %>%
  dplyr::select(hepB0_timely, penta1_timely, penta2_timely, penta3_timely, mcv1_timely, all_timely, return_timely, strata, cluster, household, sex, 
                place_birth_cat, birth_order_cat, ethnicity_cat, religion, region_cat, rural_urban, mat_age_cat, parity_cat, mat_educ_cat, 
                wealth_index, media_exposure, season, motorized, sex_hh_head, hh_mobilephone, mat_bank_acc, hh_bednets, 
                dist_hc_far, mat_insurance, marital_status_cat, PNC_no, ANC_no_cat, years_residence_cat, hh_size_cat, tt_any_mixed_cat, 
                tt_fixed_mixed_cat_new, tt_any_walking_cat, tt_fixed_walking_cat_new, site_category, cold_store, open_weekly, vaccine_staff_cat, 
                catchment_pop_cat, service_avail_readiness)

##convert to dataframe
dhs_dat1 <- data.frame(dhs_dat )

# Convert columns 8:45 to factors
dhs_dat1[, 8:45] <- lapply(dhs_dat1[, 8:45], as.factor)

#Percentage of missing data
freq.na(dhs_dat1[,c(1:45)])


#Frequency for some variables
table(dhs_dat1$ethnicity); prop.table(table(dhs_dat1$ethnicity))
table(dhs_dat1$ethnicity_cat); prop.table(table(dhs_dat1$ethnicity_cat))
table(dhs_dat1$motorized); prop.table(table(dhs_dat1$motorized))
table(dhs_dat1$parity_cat); prop.table(table(dhs_dat1$parity_cat))
table(dhs_dat1$service_avail_readiness); prop.table(table(dhs_dat1$service_avail_readiness))
table(dhs_dat1$tt_fixed_mixed_cat_new); prop.table(table(dhs_dat1$tt_fixed_mixed_cat_new))
table(dhs_dat1$region_cat); prop.table(table(dhs_dat1$region_cat))
table(dhs_dat1$tt_fixed_walking_cat_new); prop.table(table(dhs_dat1$tt_fixed_walking_cat_new))
table(dhs_dat1$open_weekly); prop.table(table(dhs_dat1$open_weekly))
table(dhs_dat1$all_timely); prop.table(table(dhs_dat1$all_timely))
table(dhs_dat1$return_timely); prop.table(table(dhs_dat1$return_timely))
table(dhs_dat1$hepB0_timely); prop.table(table(dhs_dat1$hepB0_timely))


##Delete variables with >15% missing data - PNC_no 567(17%); ANC_no_cat 567(17%)
dhs_dat1 <- dhs_dat1[,-c(32, 33)]

#Re-level all the covariates to indicate which will be the reference category
dhs_dat1$sex <- relevel(dhs_dat1$sex, ref = "Female")
dhs_dat1$place_birth_cat <- relevel(dhs_dat1$place_birth_cat, ref = "home")
dhs_dat1$birth_order_cat <- relevel(dhs_dat1$birth_order_cat, ref = ">5")
dhs_dat1$ethnicity_cat <- relevel(dhs_dat1$ethnicity_cat, ref = "Non-Gambian")
dhs_dat1$religion <- relevel(dhs_dat1$religion, ref = "Christianity")
dhs_dat1$region_cat <- relevel(dhs_dat1$region, ref = "Other regions")
dhs_dat1$rural_urban <- relevel(dhs_dat1$rural_urban, ref = "Rural")
dhs_dat1$mat_age_cat <- relevel(dhs_dat1$mat_age_cat, ref = "<=19")
dhs_dat1$parity_cat <- relevel(dhs_dat1$parity_cat, ref = "≥4")
dhs_dat1$mat_educ_cat <- relevel(dhs_dat1$mat_educ_cat, ref = "No education")
dhs_dat1$wealth_index <- relevel(dhs_dat1$wealth_index, ref = "Poor")
dhs_dat1$media_exposure <- relevel(dhs_dat1$media_exposure, ref = "not exposed to media")
dhs_dat1$season <- relevel(dhs_dat1$season, ref = "wet")
dhs_dat1$motorized <- relevel(dhs_dat1$motorized, ref = "No")
dhs_dat1$sex_hh_head <- relevel(dhs_dat1$sex_hh_head, ref = "Female")
dhs_dat1$hh_mobilephone <- relevel(dhs_dat1$hh_mobilephone, ref = "No")
dhs_dat1$mat_bank_acc <- relevel(dhs_dat1$mat_bank_acc, ref = "No")
dhs_dat1$hh_bednets <- relevel(dhs_dat1$hh_bednets, ref = "No")
dhs_dat1$dist_hc_far <- relevel(dhs_dat1$dist_hc_far, ref = "Big problem")
dhs_dat1$mat_insurance <- relevel(dhs_dat1$mat_insurance, ref = "No")
dhs_dat1$marital_status_cat <- relevel(dhs_dat1$marital_status_cat, ref = "Never in union")
dhs_dat1$years_residence_cat <- relevel(dhs_dat1$years_residence_cat, ref = "<1 year")
dhs_dat1$hh_size_cat <- relevel(dhs_dat1$hh_size_cat, ref = "large (9 or more)")
dhs_dat1$tt_any_mixed_cat <- relevel(dhs_dat1$tt_any_mixed_cat, ref = "45 - <60 mins")
dhs_dat1$tt_fixed_mixed_cat_new <- relevel(dhs_dat1$tt_fixed_mixed_cat_new, ref = "60 mins and above")
dhs_dat1$tt_any_walking_cat <- relevel(dhs_dat1$tt_any_walking_cat, ref = "45 - <60 mins")
dhs_dat1$tt_fixed_walking_cat_new <- relevel(dhs_dat1$tt_fixed_walking_cat_new, ref = "60 mins and above")
dhs_dat1$site_category <- relevel(dhs_dat1$site_category, ref = "Outreach Site")
dhs_dat1$cold_store <- relevel(dhs_dat1$cold_store, ref = "No")
dhs_dat1$open_weekly <- relevel(dhs_dat1$open_weekly, ref = "no")
dhs_dat1$vaccine_staff_cat <- relevel(dhs_dat1$vaccine_staff_cat, ref = "only 1")
dhs_dat1$catchment_pop_cat <- relevel(dhs_dat1$catchment_pop_cat, ref = "High")
dhs_dat1$service_avail_readiness <- relevel(dhs_dat1$service_avail_readiness, ref = "low (0-1)")


#Full data has 3248 rows; the vaccine with highest missing data 667 (21%) records 
dhs_dat1_full <- dhs_dat1
dhs_dat1_hepB0 <- dhs_dat1[complete.cases(dhs_dat1$hepB0_timely), ]
dhs_dat1_penta1 <- dhs_dat1[complete.cases(dhs_dat1$penta1_timely), ]
dhs_dat1_penta2 <- dhs_dat1[complete.cases(dhs_dat1$penta2_timely), ]
dhs_dat1_penta3 <- dhs_dat1[complete.cases(dhs_dat1$penta3_timely), ]
dhs_dat1_mcv1 <- dhs_dat1[complete.cases(dhs_dat1$mcv1_timely), ]
dhs_dat1_all <- dhs_dat1[complete.cases(dhs_dat1$all_timely), ]
dhs_dat1_return <- dhs_dat1[complete.cases(dhs_dat1$return_timely), ]
#dhs_dat1 <- dhs_dat1[complete.cases(dhs_dat1),]

#Percentage of missing data
freq.na(dhs_dat)
freq.na(dhs_dat1_full)
freq.na(dhs_dat1_hepB0)
freq.na(dhs_dat1_mcv1)
freq.na(dhs_dat1_all)
freq.na(dhs_dat1_return)
#####################################################################################################################
##HEPB0
#Names of covariates
covnames <- names(dhs_dat1_hepB0)[c(11:43)]

#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_hepB0)==covnames[i])
  ct <- CrossTable(dhs_dat1_hepB0[,ll], dhs_dat1_hepB0$hepB0_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_hepB0[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("hepB0_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_hepB0, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_hepB0_timely.csv")


#Excluded tt_any_walking, tt_fixed walking, site_category, cold_store, open_weekly, open_weekly, & catchment_pop_cat due to multicollinearity
#Excldued "tt_any_walking_cat", "tt_fixed_mixed_cat_new" & "tt_any_any_mixed_cat" due to redundancy (mal_know already present in the model)

covnames1 <- c("sex", "place_birth_cat", "birth_order_cat", "ethnicity_cat", 
               "religion", "region_cat",  "rural_urban", "mat_age_cat", "parity_cat",
               "mat_educ_cat", "wealth_index", "media_exposure", "season", "motorized", "sex_hh_head",
               "hh_mobilephone", "mat_bank_acc", "hh_bednets", "dist_hc_far", "mat_insurance", "marital_status_cat",
               "years_residence_cat", "hh_size_cat", #"tt_any_mixed_cat",
               "tt_fixed_mixed_cat_new", "open_weekly", "cold_store", "site_category", "vaccine_staff_cat", "catchment_pop_cat")

#Check for multicollinearity
form <- paste("hepB0_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_hepB0, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_hepB0$strat_num <- as.numeric(as.factor(dhs_dat1_hepB0$strata))

#Note that the clusters are uniquely numbered

#Create unique labels for households within clusters
dhs_dat1_hepB0$clust_hh <- paste(dhs_dat1_hepB0$cluster, dhs_dat1_hepB0$household, sep=".")


##############MODEL FITTING FOR hepb0
#Re-convert hepB0_timely to numeric for model-fitting using inla
dhs_dat1_hepB0$hepB0_a <- as.numeric(dhs_dat1_hepB0$hepB0_timely)
#dhs_dat1_hepB0$hepB0_a <- recode(dhs_dat1_hepB0$hepB0_a, recodes="1=0; 2=1; else=NA")


#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
#see https://becarioprecario.bitbucket.io/inla-gitbook/ch-multilevel.html
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- hepB0_a ~ sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region_cat +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + #tt_any_mixed_cat +
  tt_fixed_mixed_cat_new + open_weekly + cold_store + site_category + vaccine_staff_cat + catchment_pop_cat + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.hepB0_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_hepB0, Ntrials=1,
                             control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                             control.compute=list(waic=TRUE, dic=TRUE))

summary(mod.fit.hepB0_timely)

fitted.mean <- mod.fit.hepB0_timely$summary.fitted.values$mean
fitted.median <- mod.fit.hepB0_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_hepB0$hepB0_a
out.hepB0_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.hepB0_timely.1, "hepB0_timely_fitted_obs.csv")

#odds ratios
out <- exp(mod.fit.hepB0_timely$summary.fixed[, c(1,3,4,5)])
out
out.hepB0_timely <- data.frame(out)
#write.csv(out.hepB0_timely, "hepB0_timely_out.csv")


####################################################################
#VPC analysis
var.strat <- 1/mod.fit.hepB0_timely$summary.hyperpar[1,1]
var.clust <- 1/mod.fit.hepB0_timely$summary.hyperpar[2,1]
var.hh    <- 1/mod.fit.hepB0_timely$summary.hyperpar[3,1]

var.strat/(var.strat + var.clust + var.hh + 3.29)
var.clust/(var.strat + var.clust + var.hh + 3.29)
var.hh/(var.strat + var.clust + var.hh + 3.29)
Tot2 <- (var.strat + var.clust + var.hh + 3.29)
####################################################################


#Covariate evaluation
k1.f <- as.numeric(auc(obs, fitted.mean)) #AUC for full model
k2.f <- mod.fit.hepB0_timely$waic$waic    #WAIC for full model
k3.f <- mod.fit.hepB0_timely$dic$dic      #DIC for full model

cov.eval <- matrix(0, ncol=3, nrow=length(covnames1))
for (i in 1:length(covnames1)){
  covnamesa <- covnames1[-i]
  forma <- paste("hepB0_a", "~", paste(covnamesa, collapse="+"))
  form <- as.formula(paste(forma, "+", "f(strat_num, model='iid', hyper = prec.prior1) + f(cluster, model='iid',  hyper = prec.prior1) + f(clust_hh, model='iid',  hyper = prec.prior1)"))
  
  mod.fit.hepB0_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_hepB0, Ntrials=1,
                               control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                               control.compute=list(waic=TRUE, dic=TRUE))
  
  fitted.mean <- mod.fit.hepB0_timely$summary.fitted.values$mean
  
  k1 <- ((k1.f - as.numeric(auc(obs, fitted.mean)))/ k1.f)*100
  k2 <- ((k2.f - mod.fit.hepB0_timely$waic$waic)/k2.f)*100
  k3 <- ((k3.f - mod.fit.hepB0_timely$dic$dic)/k3.f)*100
  
  cov.eval[i,] <- c(k1, k2, k3)
}
colnames(cov.eval) <- c("perc_change_AUC", "perc_change_WAIC", "perc_change_DIC")
rownames(cov.eval) <- covnames1
cov.eval <- as.data.frame(cov.eval)
cov.eval$rank_auc <- length(covnames1) - rank(cov.eval$perc_change_AUC) + 1
cov.eval$rank_WAIC <- length(covnames1) - rank(cov.eval$perc_change_WAIC) + 1
cov.eval$rank_DIC <- length(covnames1) - rank(cov.eval$perc_change_DIC) + 1

write.csv(cov.eval, "evaluation_hepB0_timely.csv")
##########################################################################################################################################################

##PENTA1
#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_penta1)==covnames[i])
  ct <- CrossTable(dhs_dat1_penta1[,ll], dhs_dat1_penta1$penta1_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_penta1[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("penta1_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_penta1, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_penta1_timely.csv")


#Check for multicollinearity
form <- paste("penta1_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_penta1, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_penta1$strat_num <- as.numeric(as.factor(dhs_dat1_penta1$strata))

#Create unique labels for households within clusters
dhs_dat1_penta1$clust_hh <- paste(dhs_dat1_penta1$cluster, dhs_dat1_penta1$household, sep=".")


##############MODEL FITTING FOR penta1
#Re-convert penta1_timely to numeric for model-fitting using inla
dhs_dat1_penta1$penta1_a <- as.numeric(dhs_dat1_penta1$penta1_timely)
#dhs_dat1_penta1$penta1_a <- recode(dhs_dat1_penta1$penta1_a, recodes="1=0; 2=1; else=NA")


#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- penta1_a ~ sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region_cat +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + #tt_any_mixed_cat +
  tt_fixed_mixed_cat_new + open_weekly + cold_store + site_category + vaccine_staff_cat + catchment_pop_cat + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.penta1_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta1, Ntrials=1,
                             control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                             control.compute=list(waic=TRUE, dic=TRUE))

summary(mod.fit.penta1_timely)

fitted.mean <- mod.fit.penta1_timely$summary.fitted.values$mean
fitted.median <- mod.fit.penta1_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_penta1$penta1_a
out.penta1_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.penta1_timely.1, "penta1_timely_fitted_obs.csv")

#odds ratios
out <- exp(mod.fit.penta1_timely$summary.fixed[, c(1,3,4,5)])
out
out.penta1_timely <- data.frame(out)
#write.csv(out.penta1_timely, "penta1_timely_out.csv")


####################################################################
#VPC analysis
var.strat <- 1/mod.fit.penta1_timely$summary.hyperpar[1,1]
var.clust <- 1/mod.fit.penta1_timely$summary.hyperpar[2,1]
var.hh    <- 1/mod.fit.penta1_timely$summary.hyperpar[3,1]

var.strat/(var.strat + var.clust + var.hh + 3.29)
var.clust/(var.strat + var.clust + var.hh + 3.29)
var.hh/(var.strat + var.clust + var.hh + 3.29)
Tot2 <- (var.strat + var.clust + var.hh + 3.29)
####################################################################


#############################################################
#Covariate evaluation
k1.f <- as.numeric(auc(obs, fitted.mean)) #AUC for full model
k2.f <- mod.fit.penta1_timely$waic$waic    #WAIC for full model
k3.f <- mod.fit.penta1_timely$dic$dic      #DIC for full model

cov.eval <- matrix(0, ncol=3, nrow=length(covnames1))
for (i in 1:length(covnames1)){
  covnamesa <- covnames1[-i]
  forma <- paste("penta1_a", "~", paste(covnamesa, collapse="+"))
  form <- as.formula(paste(forma, "+", "f(strat_num, model='iid', hyper = prec.prior1) + f(cluster, model='iid',  hyper = prec.prior1) + f(clust_hh, model='iid',  hyper = prec.prior1)"))
  
  mod.fit.penta1_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta1, Ntrials=1,
                               control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                               control.compute=list(waic=TRUE, dic=TRUE))
  
  fitted.mean <- mod.fit.penta1_timely$summary.fitted.values$mean
  
  k1 <- ((k1.f - as.numeric(auc(obs, fitted.mean)))/ k1.f)*100
  k2 <- ((k2.f - mod.fit.penta1_timely$waic$waic)/k2.f)*100
  k3 <- ((k3.f - mod.fit.penta1_timely$dic$dic)/k3.f)*100
  
  cov.eval[i,] <- c(k1, k2, k3)
}
colnames(cov.eval) <- c("perc_change_AUC", "perc_change_WAIC", "perc_change_DIC")
rownames(cov.eval) <- covnames1
cov.eval <- as.data.frame(cov.eval)
cov.eval$rank_auc <- length(covnames1) - rank(cov.eval$perc_change_AUC) + 1
cov.eval$rank_WAIC <- length(covnames1) - rank(cov.eval$perc_change_WAIC) + 1
cov.eval$rank_DIC <- length(covnames1) - rank(cov.eval$perc_change_DIC) + 1

write.csv(cov.eval, "evaluation_penta1_timely.csv")
##########################################################################################################################################################

##PENTA2
#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_penta2)==covnames[i])
  ct <- CrossTable(dhs_dat1_penta2[,ll], dhs_dat1_penta2$penta2_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_penta2[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("penta2_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_penta2, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_penta2_timely.csv")


#Check for multicollinearity
form <- paste("penta2_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_penta2, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_penta2$strat_num <- as.numeric(as.factor(dhs_dat1_penta2$strata))

#Note that the clusters are uniquely numbered

#Create unique labels for households within clusters
dhs_dat1_penta2$clust_hh <- paste(dhs_dat1_penta2$cluster, dhs_dat1_penta2$household, sep=".")


##############MODEL FITTING FOR PENTA2
#Re-convert penta2_timely to numeric for model-fitting using inla
dhs_dat1_penta2$penta2_a <- as.numeric(dhs_dat1_penta2$penta2_timely)
dhs_dat1_penta2$penta2_a <- recode(dhs_dat1_penta2$penta2_a, recodes="1=0; 2=1; else=NA")


#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- penta2_a ~ sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index_urb_rural + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + tt_any_mixed_cat +
  tt_fixed_mixed_cat + service_avail_readiness + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.penta2_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta2, Ntrials=1,
                              control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed)

summary(mod.fit.penta2_timely)

#odds ratios
out <- exp(mod.fit.penta2_timely$summary.fixed[, c(1,3,4,5)])
out
out.penta2_timely <- data.frame(out)

write.csv(out.penta2_timely, "penta2_timely_out.csv")


fitted.mean <- mod.fit.penta2_timely$summary.fitted.values$mean
fitted.median <- mod.fit.penta2_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_penta2$penta2_a
out.penta2_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.penta2_timely.1, "penta2_timely_fitted_obs.csv")


##PENTA2
# Convert penta1_timely to factor and relevel
dhs_dat1_penta2$penta1_timely <- as.factor(dhs_dat1_penta2$penta1_timely)
dhs_dat1_penta2$penta1_timely <- relevel(dhs_dat1_penta2$penta1_timely, ref = "0")

covnames1 <- c("penta1_timely", "sex", "place_birth_cat", "birth_order_cat", "ethnicity_cat", 
               "religion", "region_cat",  "rural_urban", "mat_age_cat", "parity_cat",
               "mat_educ_cat", "wealth_index", "media_exposure", "season", "motorized", "sex_hh_head",
               "hh_mobilephone", "mat_bank_acc", "hh_bednets", "dist_hc_far", "mat_insurance", "marital_status_cat",
               "years_residence_cat", "hh_size_cat", #"tt_any_mixed_cat",
               "tt_fixed_mixed_cat_new", "open_weekly", "cold_store", "site_category", "vaccine_staff_cat", "catchment_pop_cat")

#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_penta2)==covnames[i])
  ct <- CrossTable(dhs_dat1_penta2[,ll], dhs_dat1_penta2$penta2_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_penta2[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("penta2_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_penta2, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_penta2_timely.csv")


#Check for multicollinearity
form <- paste("penta2_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_penta2, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_penta2$strat_num <- as.numeric(as.factor(dhs_dat1_penta2$strata))

#Note that the clusters are uniquely numbered

#Create unique labels for households within clusters
dhs_dat1_penta2$clust_hh <- paste(dhs_dat1_penta2$cluster, dhs_dat1_penta2$household, sep=".")


##############MODEL FITTING FOR penta2
#Re-convert penta2_timely to numeric for model-fitting using inla
dhs_dat1_penta2$penta2_a <- as.numeric(dhs_dat1_penta2$penta2_timely)
#dhs_dat1_penta2$penta2_a <- recode(dhs_dat1_penta2$penta2_a, recodes="1=0; 2=1; else=NA")


#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- penta2_a ~ penta1_timely + sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region_cat +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + #tt_any_mixed_cat +
  tt_fixed_mixed_cat_new + open_weekly + cold_store + site_category + vaccine_staff_cat + catchment_pop_cat + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.penta2_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta2, Ntrials=1,
                             control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                             control.compute=list(waic=TRUE, dic=TRUE))

summary(mod.fit.penta2_timely)

fitted.mean <- mod.fit.penta2_timely$summary.fitted.values$mean
fitted.median <- mod.fit.penta2_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_penta2$penta2_a
out.penta2_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.penta2_timely.1, "penta2_timely_fitted_obs.csv")

#odds ratios
out <- exp(mod.fit.penta2_timely$summary.fixed[, c(1,3,4,5)])
out
out.penta2_timely <- data.frame(out)
#write.csv(out.penta2_timely, "penta2_timely_out.csv")

####################################################################
#VPC analysis
var.strat <- 1/mod.fit.penta2_timely$summary.hyperpar[1,1]
var.clust <- 1/mod.fit.penta2_timely$summary.hyperpar[2,1]
var.hh    <- 1/mod.fit.penta2_timely$summary.hyperpar[3,1]

var.strat/(var.strat + var.clust + var.hh + 3.29)
var.clust/(var.strat + var.clust + var.hh + 3.29)
var.hh/(var.strat + var.clust + var.hh + 3.29)
Tot2 <- (var.strat + var.clust + var.hh + 3.29)
####################################################################

#############################################################
#Covariate evaluation
k1.f <- as.numeric(auc(obs, fitted.mean)) #AUC for full model
k2.f <- mod.fit.penta2_timely$waic$waic    #WAIC for full model
k3.f <- mod.fit.penta2_timely$dic$dic      #DIC for full model

cov.eval <- matrix(0, ncol=3, nrow=length(covnames1))
for (i in 1:length(covnames1)){
  covnamesa <- covnames1[-i]
  forma <- paste("penta2_a", "~", paste(covnamesa, collapse="+"))
  form <- as.formula(paste(forma, "+", "f(strat_num, model='iid', hyper = prec.prior1) + f(cluster, model='iid',  hyper = prec.prior1) + f(clust_hh, model='iid',  hyper = prec.prior1)"))
  
  mod.fit.penta2_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta2, Ntrials=1,
                               control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                               control.compute=list(waic=TRUE, dic=TRUE))
  
  fitted.mean <- mod.fit.penta2_timely$summary.fitted.values$mean
  
  k1 <- ((k1.f - as.numeric(auc(obs, fitted.mean)))/ k1.f)*100
  k2 <- ((k2.f - mod.fit.penta2_timely$waic$waic)/k2.f)*100
  k3 <- ((k3.f - mod.fit.penta2_timely$dic$dic)/k3.f)*100
  
  cov.eval[i,] <- c(k1, k2, k3)
}
colnames(cov.eval) <- c("perc_change_AUC", "perc_change_WAIC", "perc_change_DIC")
rownames(cov.eval) <- covnames1
cov.eval <- as.data.frame(cov.eval)
cov.eval$rank_auc <- length(covnames1) - rank(cov.eval$perc_change_AUC) + 1
cov.eval$rank_WAIC <- length(covnames1) - rank(cov.eval$perc_change_WAIC) + 1
cov.eval$rank_DIC <- length(covnames1) - rank(cov.eval$perc_change_DIC) + 1

write.csv(cov.eval, "evaluation_penta2_timely.csv")
#########################################################################################################################################################

##PENTA3
# Convert penta3_timely to factor and relevel
dhs_dat1_penta3$penta2_timely <- as.factor(dhs_dat1_penta3$penta2_timely)
dhs_dat1_penta3$penta2_timely <- relevel(dhs_dat1_penta3$penta2_timely, ref = "0")

covnames1 <- c("penta2_timely", "sex", "place_birth_cat", "birth_order_cat", "ethnicity_cat", 
               "religion", "region_cat",  "rural_urban", "mat_age_cat", "parity_cat",
               "mat_educ_cat", "wealth_index", "media_exposure", "season", "motorized", "sex_hh_head",
               "hh_mobilephone", "mat_bank_acc", "hh_bednets", "dist_hc_far", "mat_insurance", "marital_status_cat",
               "years_residence_cat", "hh_size_cat", #"tt_any_mixed_cat",
               "tt_fixed_mixed_cat_new", "open_weekly", "cold_store", "site_category", "vaccine_staff_cat", "catchment_pop_cat")
#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_penta3)==covnames[i])
  ct <- CrossTable(dhs_dat1_penta3[,ll], dhs_dat1_penta3$penta3_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_penta3[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("penta3_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_penta3, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_penta3_timely.csv")


#Check for multicollinearity
form <- paste("penta3_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_penta3, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_penta3$strat_num <- as.numeric(as.factor(dhs_dat1_penta3$strata))

#Note that the clusters are uniquely numbered

#Create unique labels for households within clusters
dhs_dat1_penta3$clust_hh <- paste(dhs_dat1_penta3$cluster, dhs_dat1_penta3$household, sep=".")


##############MODEL FITTING FOR PENTA3
#Re-convert penta3_timely to numeric for model-fitting using inla
dhs_dat1_penta3$penta3_a <- as.numeric(dhs_dat1_penta3$penta3_timely)
#dhs_dat1_penta3$penta3_a <- recode(dhs_dat1_penta3$penta3_a, recodes="1=0; 2=1; else=NA")


#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- penta3_a ~ penta2_timely + sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region_cat +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + #tt_any_mixed_cat +
  tt_fixed_mixed_cat_new + open_weekly + cold_store + site_category + vaccine_staff_cat + catchment_pop_cat + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.penta3_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta3, Ntrials=1,
                             control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                             control.compute=list(waic=TRUE, dic=TRUE))

summary(mod.fit.penta3_timely)

fitted.mean <- mod.fit.penta3_timely$summary.fitted.values$mean
fitted.median <- mod.fit.penta3_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_penta3$penta3_a
out.penta3_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.penta3_timely.1, "penta3_timely_fitted_obs.csv")

#odds ratios
out <- exp(mod.fit.penta3_timely$summary.fixed[, c(1,3,4,5)])
out
out.penta3_timely <- data.frame(out)
#write.csv(out.penta3_timely, "penta3_timely_out.csv")


####################################################################
#VPC analysis
var.strat <- 1/mod.fit.penta3_timely$summary.hyperpar[1,1]
var.clust <- 1/mod.fit.penta3_timely$summary.hyperpar[2,1]
var.hh    <- 1/mod.fit.penta3_timely$summary.hyperpar[3,1]

var.strat/(var.strat + var.clust + var.hh + 3.29)
var.clust/(var.strat + var.clust + var.hh + 3.29)
var.hh/(var.strat + var.clust + var.hh + 3.29)
Tot2 <- (var.strat + var.clust + var.hh + 3.29)
####################################################################

#############################################################
#Covariate evaluation
k1.f <- as.numeric(auc(obs, fitted.mean)) #AUC for full model
k2.f <- mod.fit.penta3_timely$waic$waic    #WAIC for full model
k3.f <- mod.fit.penta3_timely$dic$dic      #DIC for full model

cov.eval <- matrix(0, ncol=3, nrow=length(covnames1))
for (i in 1:length(covnames1)){
  covnamesa <- covnames1[-i]
  forma <- paste("penta3_a", "~", paste(covnamesa, collapse="+"))
  form <- as.formula(paste(forma, "+", "f(strat_num, model='iid', hyper = prec.prior1) + f(cluster, model='iid',  hyper = prec.prior1) + f(clust_hh, model='iid',  hyper = prec.prior1)"))
  
  mod.fit.penta3_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_penta3, Ntrials=1,
                               control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                               control.compute=list(waic=TRUE, dic=TRUE))
  
  fitted.mean <- mod.fit.penta3_timely$summary.fitted.values$mean
  
  k1 <- ((k1.f - as.numeric(auc(obs, fitted.mean)))/ k1.f)*100
  k2 <- ((k2.f - mod.fit.penta3_timely$waic$waic)/k2.f)*100
  k3 <- ((k3.f - mod.fit.penta3_timely$dic$dic)/k3.f)*100
  
  cov.eval[i,] <- c(k1, k2, k3)
}
colnames(cov.eval) <- c("perc_change_AUC", "perc_change_WAIC", "perc_change_DIC")
rownames(cov.eval) <- covnames1
cov.eval <- as.data.frame(cov.eval)
cov.eval$rank_auc <- length(covnames1) - rank(cov.eval$perc_change_AUC) + 1
cov.eval$rank_WAIC <- length(covnames1) - rank(cov.eval$perc_change_WAIC) + 1
cov.eval$rank_DIC <- length(covnames1) - rank(cov.eval$perc_change_DIC) + 1

write.csv(cov.eval, "evaluation_penta3_timely.csv")
#########################################################################################################################################################

##MCV1

covnames1 <- c("sex", "place_birth_cat", "birth_order_cat", "ethnicity_cat", 
               "religion", "region_cat",  "rural_urban", "mat_age_cat", "parity_cat",
               "mat_educ_cat", "wealth_index", "media_exposure", "season", "motorized", "sex_hh_head",
               "hh_mobilephone", "mat_bank_acc", "hh_bednets", "dist_hc_far", "mat_insurance", "marital_status_cat",
               "years_residence_cat", "hh_size_cat", #"tt_any_mixed_cat",
               "tt_fixed_mixed_cat_new", "open_weekly", "cold_store", "site_category", "vaccine_staff_cat", "catchment_pop_cat")

#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_mcv1)==covnames[i])
  ct <- CrossTable(dhs_dat1_mcv1[,ll], dhs_dat1_mcv1$mcv1_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_mcv1[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("mcv1_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_mcv1, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_mcv1_timely.csv")


#Check for multicollinearity
form <- paste("mcv1_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_mcv1, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_mcv1$strat_num <- as.numeric(as.factor(dhs_dat1_mcv1$strata))

#Note that the clusters are uniquely numbered

#Create unique labels for households within clusters
dhs_dat1_mcv1$clust_hh <- paste(dhs_dat1_mcv1$cluster, dhs_dat1_mcv1$household, sep=".")


##############MODEL FITTING FOR PENTA3
#Re-convert penta3_timely to numeric for model-fitting using inla
dhs_dat1_mcv1$mcv1_a <- as.numeric(dhs_dat1_mcv1$mcv1_timely)
#dhs_dat1_mcv1$mcv1_a <- recode(dhs_dat1_mcv1$mcv1_a, recodes="1=0; 2=1; else=NA")


#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- mcv1_a ~ sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region_cat +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + #tt_any_mixed_cat +
  tt_fixed_mixed_cat_new + open_weekly + cold_store + site_category + vaccine_staff_cat + catchment_pop_cat + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.mcv1_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_mcv1, Ntrials=1,
                              control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                              control.compute=list(waic=TRUE, dic=TRUE))

summary(mod.fit.mcv1_timely)

fitted.mean <- mod.fit.mcv1_timely$summary.fitted.values$mean
fitted.median <- mod.fit.mcv1_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_mcv1$mcv1_a
out.mcv1_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.mcv1_timely.1, "mcv1_timely_fitted_obs.csv")

#odds ratios
out <- exp(mod.fit.mcv1_timely$summary.fixed[, c(1,3,4,5)])
out
out.mcv1_timely <- data.frame(out)
#write.csv(out.mcv1_timely, "mcv1_timely_out.csv")


####################################################################
#VPC analysis
var.strat <- 1/mod.fit.mcv1_timely$summary.hyperpar[1,1]
var.clust <- 1/mod.fit.mcv1_timely$summary.hyperpar[2,1]
var.hh    <- 1/mod.fit.mcv1_timely$summary.hyperpar[3,1]

var.strat/(var.strat + var.clust + var.hh + 3.29)
var.clust/(var.strat + var.clust + var.hh + 3.29)
var.hh/(var.strat + var.clust + var.hh + 3.29)
Tot2 <- (var.strat + var.clust + var.hh + 3.29)
####################################################################

#############################################################
#Covariate evaluation
k1.f <- as.numeric(auc(obs, fitted.mean)) #AUC for full model
k2.f <- mod.fit.mcv1_timely$waic$waic    #WAIC for full model
k3.f <- mod.fit.mcv1_timely$dic$dic      #DIC for full model

cov.eval <- matrix(0, ncol=3, nrow=length(covnames1))
for (i in 1:length(covnames1)){
  covnamesa <- covnames1[-i]
  forma <- paste("mcv1_a", "~", paste(covnamesa, collapse="+"))
  form <- as.formula(paste(forma, "+", "f(strat_num, model='iid', hyper = prec.prior1) + f(cluster, model='iid',  hyper = prec.prior1) + f(clust_hh, model='iid',  hyper = prec.prior1)"))
  
  mod.fit.mcv1_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_mcv1, Ntrials=1,
                                control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                                control.compute=list(waic=TRUE, dic=TRUE))
  
  fitted.mean <- mod.fit.mcv1_timely$summary.fitted.values$mean
  
  k1 <- ((k1.f - as.numeric(auc(obs, fitted.mean)))/ k1.f)*100
  k2 <- ((k2.f - mod.fit.mcv1_timely$waic$waic)/k2.f)*100
  k3 <- ((k3.f - mod.fit.mcv1_timely$dic$dic)/k3.f)*100
  
  cov.eval[i,] <- c(k1, k2, k3)
}
colnames(cov.eval) <- c("perc_change_AUC", "perc_change_WAIC", "perc_change_DIC")
rownames(cov.eval) <- covnames1
cov.eval <- as.data.frame(cov.eval)
cov.eval$rank_auc <- length(covnames1) - rank(cov.eval$perc_change_AUC) + 1
cov.eval$rank_WAIC <- length(covnames1) - rank(cov.eval$perc_change_WAIC) + 1
cov.eval$rank_DIC <- length(covnames1) - rank(cov.eval$perc_change_DIC) + 1

write.csv(cov.eval, "evaluation_mcv1_timely.csv")
######################################################################################################################################################

##Return_timely
#Chi-square
chisq.out <- data.frame(Covariate = character(), stat = numeric(), pvalue = numeric(), sig = character())

for (i in 1:length(covnames)){
  ll <- which(names(dhs_dat1_return)==covnames[i])
  ct <- CrossTable(dhs_dat1_return[,ll], dhs_dat1_return$return_timely, chisq = TRUE, prop.c=FALSE, prop.r=FALSE,
                   prop.t=FALSE, prop.chisq=FALSE)
  sig <- "no"; if (ct$chisq$p.value < 0.05) sig <- "yes"
  lev <- levels(dhs_dat1_return[,ll])
  if (length(lev) == 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
  if (length(lev) > 2) chisq.out[i,] <- c(covnames[i], as.numeric(ct$chisq$statistic), ct$chisq$p.value, sig)
}


#Logistic regression
logmod.out <- data.frame(Covariate = character(), min.pvalue = character())
logmod.out.OR <- data.frame()

for (i in 1:length(covnames)){
  print(i)
  form <- paste("return_timely", "~", covnames[i])
  log.mod <- glm(form, data = dhs_dat1_return, family="binomial")
  print(summary(log.mod))
  print(exp(cbind(OR = coef(log.mod), confint(log.mod))))
  ll <- exp(cbind(OR = coef(log.mod), confint(log.mod)))
  logmod.out.OR <- rbind(logmod.out.OR, ll)
  sig <- "no"
  if (min(summary(log.mod)$coefficients[-1,4]) < 0.05) sig <- "yes"
  logmod.out[i,] <- c(covnames[i], sig)
}

logmod.out 
logmod.out.OR
write.csv(logmod.out.OR, "unadjustedOR_return_timely.csv")


#Check for multicollinearity
form <- paste("return_timely", "~", paste(covnames1, collapse="+"))
log.mod <- glm(form, data = dhs_dat1_return, family="binomial")
summary(log.mod)
exp(cbind(OR = coef(log.mod), confint(log.mod))) ## odds ratios and 95% CI
vif(log.mod)


#Convert stratification variable to numbers
dhs_dat1_return$strat_num <- as.numeric(as.factor(dhs_dat1_return$strata))

#Create unique labels for households within clusters
dhs_dat1_return$clust_hh <- paste(dhs_dat1_return$cluster, dhs_dat1_return$household, sep=".")


##############MODEL FITTING FOR RETURN_TIMELY
#Re-convert penta3_timely to numeric for model-fitting using inla
dhs_dat1_return$return_a <- as.numeric(dhs_dat1_return$return_timely)

#NOTEs - we could have both strata and cluster as separate ID variables since these are unique. We'll then have clust_hh since hh numbering
#is repeated for each cluster.

#Gamma(0.001, 0.001) is a vague prior.
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#NOTE NEW PRIOR
prec.prior1 <- list(prec = list(param = c(0.1, 0.1)))

#prec.prior <- list(prec = list(prior="pc.prec", param = c(5,0.01))) #This prior gave the same result
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) #Priors on regression coefficients


####Model-fitting
form <- return_a ~ sex + place_birth_cat + birth_order_cat + ethnicity_cat + religion + region_cat +
  rural_urban + mat_age_cat + parity_cat + mat_educ_cat + wealth_index + media_exposure + season + motorized + sex_hh_head + hh_mobilephone +
  mat_bank_acc + hh_bednets + dist_hc_far + mat_insurance + marital_status_cat + years_residence_cat +  hh_size_cat + #tt_any_mixed_cat +
  tt_fixed_mixed_cat_new + open_weekly + cold_store + site_category + vaccine_staff_cat + catchment_pop_cat + f(strat_num, model="iid", hyper = prec.prior1) + #Unique stratum IDs
  f(cluster, model="iid",  hyper = prec.prior1) +   #Unique cluster IDs
  f(clust_hh, model="iid",  hyper = prec.prior1)  #Unique household IDs

mod.fit.return_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_return, Ntrials=1,
                            control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                            control.compute=list(waic=TRUE, dic=TRUE))

summary(mod.fit.return_timely)

fitted.mean <- mod.fit.return_timely$summary.fitted.values$mean
fitted.median <- mod.fit.return_timely$summary.fitted.values$"0.5quant"
obs <- dhs_dat1_return$return_a
out.return_timely.1 <- data.frame(obs=obs, fitted.mean=fitted.mean, fitted.median=fitted.median)
write.csv(out.return_timely.1, "return_timely_fitted_obs.csv")

#odds ratios
out <- exp(mod.fit.return_timely$summary.fixed[, c(1,3,4,5)])
out
out.return_timely <- data.frame(out)
#write.csv(out.return_timely, "return_timely_out.csv")



####################################################################
#VPC analysis
var.strat <- 1/mod.fit.return_timely$summary.hyperpar[1,1]
var.clust <- 1/mod.fit.return_timely$summary.hyperpar[2,1]
var.hh    <- 1/mod.fit.return_timely$summary.hyperpar[3,1]

var.strat/(var.strat + var.clust + var.hh + 3.29)
var.clust/(var.strat + var.clust + var.hh + 3.29)
var.hh/(var.strat + var.clust + var.hh + 3.29)
Tot2 <- (var.strat + var.clust + var.hh + 3.29)
####################################################################

#############################################################
#Covariate evaluation
k1.f <- as.numeric(auc(obs, fitted.mean)) #AUC for full model
k2.f <- mod.fit.return_timely$waic$waic    #WAIC for full model
k3.f <- mod.fit.return_timely$dic$dic      #DIC for full model

cov.eval <- matrix(0, ncol=3, nrow=length(covnames1))
for (i in 1:length(covnames1)){
  covnamesa <- covnames1[-i]
  forma <- paste("return_a", "~", paste(covnamesa, collapse="+"))
  form <- as.formula(paste(forma, "+", "f(strat_num, model='iid', hyper = prec.prior1) + f(cluster, model='iid',  hyper = prec.prior1) + f(clust_hh, model='iid',  hyper = prec.prior1)"))
  
  mod.fit.return_timely <- inla(as.formula(form), family="binomial", data=dhs_dat1_return, Ntrials=1,
                              control.predictor = list(compute = TRUE, link=1), control.fixed = control.fixed,
                              control.compute=list(waic=TRUE, dic=TRUE))
  
  fitted.mean <- mod.fit.return_timely$summary.fitted.values$mean
  
  k1 <- ((k1.f - as.numeric(auc(obs, fitted.mean)))/ k1.f)*100
  k2 <- ((k2.f - mod.fit.return_timely$waic$waic)/k2.f)*100
  k3 <- ((k3.f - mod.fit.return_timely$dic$dic)/k3.f)*100
  
  cov.eval[i,] <- c(k1, k2, k3)
}
colnames(cov.eval) <- c("perc_change_AUC", "perc_change_WAIC", "perc_change_DIC")
rownames(cov.eval) <- covnames1
cov.eval <- as.data.frame(cov.eval)
cov.eval$rank_auc <- length(covnames1) - rank(cov.eval$perc_change_AUC) + 1
cov.eval$rank_WAIC <- length(covnames1) - rank(cov.eval$perc_change_WAIC) + 1
cov.eval$rank_DIC <- length(covnames1) - rank(cov.eval$perc_change_DIC) + 1

write.csv(cov.eval, "evaluation_return_timely.csv")

##########################################################################################################################################################
######Write results
out.full <- data.frame(out.hepB0_timely, out.penta1_timely, out.penta2_timely, out.penta3_timely, out.mcv1_timely, out.return_timely)
colnames(out.full) <- c("hepB0_mean", "hepB0_low", "hepB0_median", "hepB0_high",
                        "penta1_mean", "penta1_low", "penta1_median", "penta1_high",
                        "penta2_mean", "penta2_low", "penta2_median", "penta2_high",
                        "penta3_mean", "penta3_low", "penta3_median", "penta3_high",
                        "mcv1_mean", "mcv1_low", "mcv1_median", "mcv1_high",
                        "return_mean", "return_low", "return_median", "return_high")


write.csv(out.full, "all_results_OR.csv")


###########################################################################################################################################################
#####################Multivariate analysis plots######################################################################################
##1. penta forest plot
### read data into R
dat <- read.csv("penta_timely_out_edited.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:48])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  facet_wrap(. ~ vaccine, scales = "free", ncol = 3) +
  xlab("Adjusted odds ratio and 95% credible intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(38.5, 42.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "penta_multivariate_plot.png", plot=p, height=12, width=18, units="in", device = "png", dpi=300)
######################################################################################################################################
##2. hepB0_mcv1 forest plot
### read data into R
dat <- read.csv("hepb0_mcv1_timely_out_edited.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:47])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  facet_wrap(. ~ vaccine, scales = "free", ncol = 3) +
  xlab("Adjusted odds ratio and 95% credible intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "hepb0_mcv1_multivariate_plot.png", plot=p, height=12, width=18, units="in", device = "png", dpi=300)
#################################################################################################################################################

##3. return_timely forest plot
### read data into R
dat <- read.csv("return_timely_out.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:47])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Adjusted odds ratio and 95% credible intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Timely 1st, 2nd, & 3rd doses of Pentavalent vaccine") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "return_multivariate_plot.png", plot=p, height=12, width=8.5, units="in", device = "png", dpi=300)
##################################################################################################################################################
##4. summary tile plot of significant variables

#read in melted data 
data 	<- read.csv("summary_determinants.csv", header = TRUE)

head(data)
data$Variable1 <- factor(data$variable, levels = data$variable[1:13])

# Convert columns 8:45 to factors
data[1:5] <- lapply(data[1:5], as.factor)

# Convert the ward variable to a factor so that they can appear with the desired order in the dataframe when plotted
data$vaccine <- factor(data$vaccine, levels = unique(data$vaccine))

#data$significant <- factor(data$significant, levels = c("NA", "no", "yes"))


# Define the color palette
color_palette <- c("#dadada", "#28778d", "#b30e38")  # grey, blue and red


# Create the heatmap
p <- ggplot(data, aes(x = vaccine, y = Variable1, fill = significant)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = color_palette, guide = guide_legend(title = NULL),
                    breaks = c("none", "no", "yes"), labels = c("NA", "Not significant", "Significant")) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11, face = "bold"),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 8, angle = 0, hjust = 0.5),
        legend.position = "right",
        legend.box = "vertical",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0))+
  geom_hline(yintercept = c(10.5, 11.5),
             color = "white", linetype = "solid", size = 1.5)

ggsave(filename= "summary_multivariate_plot.png", plot=p, height=4, width=6, units="in", device = "png", dpi=300)
#####################################################################################################################################################
 ##Make plots for unadjusted ORs######################################
##.1 HepB0 forest plot

### read data into R
dat <- read.csv("unadjustedOR_hepB0_timely.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:49])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Unadjusted odds ratio and 95% confidence intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Bivariate analysis of factors associated with timely HepB0") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "unadjusted_hepb0_plot.png", plot=p, height=12, width=9, units="in", device = "png", dpi=300)
##################################################################################################################################################

##.2 mcv1 forest plot

### read data into R
dat <- read.csv("unadjustedOR_mcv1_timely.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:49])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Unadjusted odds ratio and 95% confidence intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Bivariate analysis of factors associated with timely MCV1") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "unadjusted_mcv1_plot.png", plot=p, height=12, width=9, units="in", device = "png", dpi=300)
##################################################################################################################################################

##.3 penta 1 forest plot

### read data into R
dat <- read.csv("unadjustedOR_penta1_timely.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:49])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Unadjusted odds ratio and 95% confidence intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Bivariate analysis of factors associated with timely Penta 1") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),

        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "unadjusted_penta1_plot.png", plot=p, height=12, width=9, units="in", device = "png", dpi=300)
##################################################################################################################################################

##.4 penta 2 forest plot

### read data into R
dat <- read.csv("unadjustedOR_penta2_timely.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:49])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Unadjusted odds ratio and 95% confidence intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Bivariate analysis of factors associated with timely Penta 2") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "unadjusted_penta2_plot.png", plot=p, height=12, width=9, units="in", device = "png", dpi=300)
##################################################################################################################################################

##.5 penta 3 forest plot

### read data into R
dat <- read.csv("unadjustedOR_penta3_timely.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:49])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Unadjusted odds ratio and 95% confidence intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Bivariate analysis of factors associated with timely Penta 3") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "unadjusted_penta3_plot.png", plot=p, height=12, width=9, units="in", device = "png", dpi=300)
##################################################################################################################################################


##.6 penta 1,2,3 forest plot

### read data into R
dat <- read.csv("unadjustedOR_return_timely.csv", header = TRUE)

head(dat)
dat$Variable1 <- factor(dat$variable, levels = dat$variable[1:49])
ind1 <- which(dat$lower>1|dat$upper<1)
dat$ind <- rep(0, nrow(dat))
dat$ind[ind1] <- 1

p <- ggplot(dat, aes(x = mean, y = Variable1)) +
  geom_point(stat = "identity", shape = 15) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4) +
  geom_point(stat = "identity", data = dat[dat$ind == 1, ], colour = "#c72b23") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, size = 0.4, colour = "#c72b23", data = dat[dat$ind == 1, ]) +
  xlab("Unadjusted odds ratio and 95% confidence intervals") +
  ylab("") +
  xlim(-0.2, NA) +
  ggtitle("Bivariate analysis: factors associated with timely all Penta doses") +
  theme_bw() +
  geom_vline(xintercept = 1.0, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = c(37.5, 41.5), colour = "navyblue", size = 0.8) +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave(filename= "unadjusted_penta123_plot.png", plot=p, height=12, width=9.5, units="in", device = "png", dpi=300)
##################################################################################################################################################
