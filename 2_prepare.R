###
#
#
# Creat the masterfile for analysis
#
#
###


library(tidyverse)




## Load the various variables

# diagnoses
icd <- read.csv('AF_dementias_GP_inpatient.csv')
# date of birth and sex
age_sex <- read.csv('age_sex_formatted.csv', sep='|')
# test dates
test_date <- read.csv('test_date.csv'); colnames(test_date) <- c('id', 'test_date', 'date_2', 'date_3')
test_date <- subset(test_date, select=c(id, test_date))
# calculate age at recruitment
age_sex <- merge(age_sex, test_date, all.x = TRUE)
age_sex$birth_date <- as.Date(age_sex$birth_date,"%Y-%m-%d") # set column as class date
age_sex$test_date <- as.Date(age_sex$test_date,"%Y-%m-%d")
age_sex$age <- as.numeric(difftime(age_sex$test_date, age_sex$birth_date, units = 'weeks')/52.25)
# smoking status
smoking <- read.csv('tobacco.csv'); smoking$smoking_1[smoking$smoking_1==-3] <- NA
smoking <- subset(smoking, select=c(id, smoking_1)); colnames(smoking) <- c('id','smoking')
# education
education <- read.csv('education.csv')
education[education==2 | education==3 | education==4 | education==5 | education==6| education==-7] <- 0
education[education==-3] <- NA
education$education_1 <- NA
education$education_1[education$first_1==0 | education$first_2==0 | education$first_3==0 | education$first_4==0 | education$first_5==0] <- 0
education$education_1[education$first_1==1 | education$first_2==1 | education$first_3==1 | education$first_4==1 | education$first_5==1] <- 1
education <- subset(education, select=c(id, education_1)); colnames(education) <- c('id','education')
# deprivation
deprivation <- read.csv('deprivation.csv')
# alcohol consumption frequency
alc_freq <- read.csv('alcohol.csv')
alc_freq <- subset(alc_freq, select=c(id, alc_freq_1)); colnames(alc_freq) <- c('id','alc_freq')
alc_freq$alc_freq[alc_freq$alc_freq==-3] <- NA
# physical activity
activity_type <- read.csv('activity_type.csv')
activity_type[activity_type==-7 | activity_type==-3] <- NA # change 'none' or 'prefer not to answer' to NA
activity_type[activity_type==1 | activity_type==4] <- 1 # re-code as found in Hanlon et al. (2020)
activity_type[activity_type==2 | activity_type==5] <- 2 # re-code as found in Hanlon et al. (2020)
activity_type$activity[activity_type$first_1==1 | activity_type$first_2==1 | activity_type$first_3==1 | activity_type$first_4==1 | activity_type$first_5==1] <- 1
activity_type$activity[activity_type$first_1==2 | activity_type$first_2==2 | activity_type$first_3==2 | activity_type$first_4==2 | activity_type$first_5==2] <- 2
activity_type$activity[activity_type$first_1==3 | activity_type$first_2==3 | activity_type$first_3==3 | activity_type$first_4==3 | activity_type$first_5==3] <- 3
activity_type <- subset(activity_type, select=c(id, activity))
# BMI
bmi <- read.csv('bmi.csv')
bmi <- subset(bmi, select=c(id, bmi_1)); colnames(bmi) <- c('id', 'bmi')
# assessment centre
centre <- read.csv('assessment_centre.csv'); colnames(centre) <- c('id', 'ass_centre', 'ass_centre_2', 'ass_centre_3')
centre <- subset(centre, select=c(id, ass_centre))
# genetic (and genotyping) covariates
genetic_covs <- read.csv('gen_covs.csv')
genetic_covs$genotyping.array[genetic_covs$coding < 0] <- 'UKBL'; genetic_covs$genotyping.array[genetic_covs$coding > 0] <- 'UKBB'
gen_batch_coding <- read.csv('genotyping_batch_coding.csv')
genetic_covs <- merge(genetic_covs, gen_batch_coding, by='coding', all.x=TRUE)
# warfarin prescription history
meds <- read.csv('prescriptions_warfarin_people.csv', sep = '|')
meds$total_warfarin[is.na(meds$total_warfarin)] <- 0
meds <- subset(meds, select=-c(age, sex))
# family history of AD and VKORC1 genotype
AD <- readRDS('UKB_AD_FamHist_rs9923231.rds') # file with VKOR genotype and family history of AD
colnames(AD)[which(names(AD) == "IID")] <- "id"
AD <- subset(AD, select=c(id, rs9923231_T, AD_mum, AD_dad, age_dad, age_mum, AD_parent))
# create variable on whether warfarin had been taken by a participant at any point
meds$binary_warfarin <- 0
meds$binary_warfarin[meds$total_warfarin > 0] <- 1




## Create the masterfile
warfarin <- merge(icd, age_sex, all.x = TRUE)
warfarin <- merge(warfarin, smoking, all.x = TRUE)
warfarin <- merge(warfarin, education, all.x = TRUE)
warfarin <- merge(warfarin, deprivation, all.x = TRUE)
warfarin <- merge(warfarin, alc_freq, all.x = TRUE)
warfarin <- merge(warfarin, activity_type, all.x = TRUE)
warfarin <- merge(warfarin, bmi, all.x = TRUE)
warfarin <- merge(warfarin, centre, all.x = TRUE)
warfarin <- merge(warfarin, genetic_covs, all.x = TRUE)
warfarin <- merge(warfarin, AD, all.x = TRUE)
warfarin <- merge(warfarin, meds, all.x = TRUE)

# merge with the unrelated cohort to exclude related individuals
load("UKB_famhistAD_PCs_unrel_17May2020.RData")
colnames(d1)[which(names(d1) == "IID")] <- "id"
warfarin <- filter(warfarin, id %in% d1$id)

# remove id's that may have opted out of the study
opt_out <- read.csv('participant opt-out.csv')
warfarin <- filter(warfarin, !id %in% opt_out$id)

# change coding so that any number of parents with AD will count the same
warfarin$AD_parent[warfarin$AD_parent == 2] <- 1

# create a new column that has the average warfarin dose for each participant and check the distribution
warfarin$dose_avg <- warfarin$dose_sum/warfarin$total_warfarin

# create new variable for VKOR carriers
warfarin$vkor_carrier <- 0
warfarin$vkor_carrier[warfarin$rs9923231_T=='1'] <- 1
warfarin$vkor_carrier[warfarin$rs9923231_T=='2'] <- 1

# transform to correct type
warfarin$AD_parent <- as.factor(warfarin$AD_parent)
warfarin$rs9923231_T <- as.numeric(warfarin$rs9923231_T)
warfarin$AF <- as.factor(warfarin$AF)
warfarin$AD <- as.factor(warfarin$AD)
warfarin$dem_any <- as.factor(warfarin$dem_any)
warfarin$dem_vascular <- as.factor(warfarin$dem_vascular)
warfarin$dem_other <- as.factor(warfarin$dem_other)
warfarin$sex <- as.factor(warfarin$sex)
warfarin$education <- as.factor(warfarin$education)
warfarin$smoking <- as.factor(warfarin$smoking)
warfarin$alc_freq <- as.factor(warfarin$alc_freq)
warfarin$activity <- as.factor(warfarin$activity)
warfarin$vkor_carrier <- as.factor(warfarin$vkor_carrier)
warfarin$Batch <- as.factor(warfarin$Batch)
warfarin$genotyping.array <- as.factor(warfarin$genotyping.array)
warfarin$ass_centre <- as.factor(warfarin$ass_centre)

# remove participants that were 60 or younger on the last day of sampling
mortality <- read.csv("death.txt", sep='\t') # mortality data
mortality <- subset(mortality, select=c(eid, date_of_death)); colnames(mortality) <- c('id', 'last_date') # filter out the first two columns
warfarin$last_date <- NULL
warfarin <- merge(warfarin, mortality, all.x = TRUE, by = 'id')
warfarin$last_date <- as.Date(warfarin$last_date, "%d/%m/%Y")
warfarin$last_date[is.na(warfarin$last_date)] <- "2020-06-30" # if they haven't died, set the last sampling day as the "last date"
warfarin$age_cens <- as.numeric(difftime(warfarin$last_date, warfarin$birth_date, units='weeks')/52.25) # calculate age at end of sampling
warfarin <- filter(warfarin, age_cens > 60)

# export
saveRDS(warfarin, file = 'warfarin_data_NEW.rds')
