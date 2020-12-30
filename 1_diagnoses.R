###
#
#
# This code imports the diagnoses and the dates when those diagnoses were made, and creates a data frame with columns for each relevant diagnosis.
#
#
###



library(tidyverse)




## Import the codes for each disorder

codes <- read.csv('codes in sample.csv', header=TRUE)
# remove potential white space and convert to lower case
codes <- data.frame(sapply(codes, trimws))
codes$disorder <- tolower(codes$disorder) 
codes$source <- tolower(codes$source)
codes$code <- as.character(codes$code)
codes$n <- as.numeric(codes$n)

# add "priority" column, which indicates that codes that refer to both general dementia as well as a specific dementia should preferentialy refer to the specific one
codes$priority <- 1; codes$priority[codes$disorder=='other dementia'] <- 2

# separate codes into inpatient and primary care
codes_gp <- filter(codes, source=='gp')
codes_inpatient <- filter(codes, source=='inpatient')

# arrange by priority and in case of duplicates, keep only the first ('high priority"; i.e., specific dementia) occurrence
codes_inpatient <- codes_inpatient %>% arrange(priority)
codes_gp <- codes_gp %>% arrange(priority)
codes_inpatient <- distinct(codes_inpatient, code, .keep_all = TRUE)
codes_gp <- distinct(codes_gp, code, .keep_all = TRUE)





### Get the inpatient diagnoses ###


## Prepare files

# import files
icd9 <- readRDS('icd9.rds')
icd9_dates <- readRDS('icd9_dates.rds')
icd10 <- readRDS('icd10.rds')
icd10_dates <- readRDS('icd10_dates.rds')

# transform blank cells to NA's
icd9[icd9==""]  <- NA 
icd9_dates[icd9_dates==""]  <- NA 
icd10[icd10==""]  <- NA 
icd10_dates[icd10_dates==""]  <- NA 

# rename columns
colnames(icd9) <- as.character(c('id', seq(1,ncol(icd9)-1))) 
colnames(icd9_dates) <- as.character(c('id', seq(1,ncol(icd9_dates)-1)))
colnames(icd10) <- as.character(c('id', seq(1,ncol(icd10)-1)))
colnames(icd10_dates) <- as.character(c('id', seq(1,ncol(icd10_dates)-1)))

# remove rows that contain only NA's
icd9 <- icd9[rowSums(is.na(icd9))!=ncol(icd9)-1,]
icd9_dates <- icd9_dates[rowSums(is.na(icd9_dates))!=ncol(icd9_dates)-1,]
icd10 <- icd10[rowSums(is.na(icd10))!=ncol(icd10)-1,]
icd10_dates <- icd10_dates[rowSums(is.na(icd10_dates))!=ncol(icd10_dates)-1,]

# change class of all columns to character
icd9 <- as.data.frame(sapply(icd9, as.character))
icd9_dates <- as.data.frame(sapply(icd9_dates, as.character))
icd10 <- as.data.frame(sapply(icd10, as.character))
icd10_dates <- as.data.frame(sapply(icd10_dates, as.character))

# change to long format
icd9 <- icd9 %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd9) <- c('id', 'column', 'diagnosis');
icd9_dates <- icd9_dates %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd9_dates) <- c('id', 'column', 'date');
icd10 <- icd10 %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd10) <- c('id', 'column', 'diagnosis');
icd10_dates <- icd10_dates %>%  pivot_longer(-id, names_to = "diagnosis", values_drop_na=TRUE); colnames(icd10_dates) <- c('id', 'column', 'date');

# merge diagnoses with dates
icd9 <- merge(icd9, icd9_dates, by=c('id', 'column'))
icd9$version <- '9'
icd9$date <- as.Date(icd9$date, '%Y-%m-%d')
icd10 <- merge(icd10, icd10_dates, by=c('id', 'column'))
icd10$version <- '10'
icd10$date <- as.Date(icd10$date, '%Y-%m-%d')


## Add the most recently published diagnoses.
# import and merge with dates
diagnoses_dates <- read.csv('hesin.txt', sep="\t")
diagnoses <- read.csv('hesin_diag.txt', sep="\t")
diagnoses_dates <- subset(diagnoses_dates, select=c(eid, ins_index, admidate))
diagnoses <- subset(diagnoses, select=c(eid, ins_index, diag_icd9, diag_icd10))
diagnoses <- merge(diagnoses, diagnoses_dates, by=c('eid', 'ins_index'), all.x = TRUE)
diagnoses$eid <- as.factor(diagnoses$eid)
diagnoses$admidate <- as.Date(diagnoses$admidate, '%d/%m/%Y')
# retain the relevant columns
icd9_new <- subset(filter(diagnoses, diag_icd9!=''), select=c(eid, diag_icd9, admidate))
colnames(icd9_new) <- c('id', 'diagnosis','date')
icd9_new$version <- '9'
icd9_new <- data.frame(sapply(icd9_new, trimws))
icd10_new <- subset(filter(diagnoses, diag_icd10!=''), select=c(eid, diag_icd10, admidate))
colnames(icd10_new) <- c('id', 'diagnosis', 'date')
icd10_new$version <- '10'
icd10_new <- data.frame(sapply(icd10_new, trimws))
# merge with the old diagnoses and delete duplicates
icd9$column <- NULL
icd9_update <- rbind(icd9, icd9_new)
icd9_update <- icd9_update %>% arrange(date)
icd9_update <- distinct(icd9_update, id,diagnosis, .keep_all = TRUE)
icd10$column <- NULL
icd10_update <- rbind(icd10, icd10_new)
icd10_update <- icd10_update %>% arrange(date)
icd10_update <- distinct(icd10_update, id,diagnosis, .keep_all = TRUE)


## Merge ICD-9 and ICD-10.
icd <- rbind(icd9_update, icd10_update)




## Add a column for each relevant diagnosis

# ischaemic stroke
icd$stroke_i <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='stroke_i' & codes_inpatient$code_system=='ICD9']){
  icd$stroke_i[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='stroke_i' & codes_inpatient$code_system=='ICD10']){
  icd$stroke_i[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_stroke_i <- icd$date; icd$date_stroke_i[icd$stroke_i==0] <- NA

# haemorrhagic stroke
icd$stroke_h <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='stroke_h' & codes_inpatient$code_system=='ICD9']){
  icd$stroke_h[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='stroke_h' & codes_inpatient$code_system=='ICD10']){
  icd$stroke_h[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_stroke_h <- icd$date; icd$date_stroke_h[icd$stroke_h==0] <- NA

# diabetes
icd$diabetes <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='diabetes' & codes_inpatient$code_system=='ICD9']){
  icd$diabetes[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='diabetes' & codes_inpatient$code_system=='ICD10']){
  icd$diabetes[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_diabetes <- icd$date; icd$date_diabetes[icd$diabetes==0] <- NA

# hypercholesterolemia
icd$hyperchol <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='hypercholesterolemia' & codes_inpatient$code_system=='ICD9']){
  icd$hyperchol[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='hypercholesterolemia' & codes_inpatient$code_system=='ICD10']){
  icd$hyperchol[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_hyperchol <- icd$date; icd$date_hyperchol[icd$hyperchol==0] <- NA

# hypertension
icd$hypertension <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='hypertension' & codes_inpatient$code_system=='ICD9']){
  icd$hypertension[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='hypertension' & codes_inpatient$code_system=='ICD10']){
  icd$hypertension[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_hypertension <- icd$date; icd$date_hypertension[icd$hypertension==0] <- NA

# AF
icd$AF <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='af' & codes_inpatient$code_system=='ICD9']){
  icd$AF[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='af' & codes_inpatient$code_system=='ICD10']){
  icd$AF[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_AF <- icd$date; icd$date_AF[icd$AF==0] <- NA

# AD
icd$AD <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='adem' & codes_inpatient$code_system=='ICD9']){
  icd$AD[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='adem' & codes_inpatient$code_system=='ICD10']){
  icd$AD[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_AD <- icd$date; icd$date_AD[icd$AD==0] <- NA

# VaD
icd$dem_vascular <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='vad' & codes_inpatient$code_system=='ICD9']){
  icd$dem_vascular[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='vad' & codes_inpatient$code_system=='ICD10']){
  icd$dem_vascular[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_dem_vascular <- icd$date; icd$date_dem_vascular[icd$dem_vascular==0] <- NA

# Other dementia
icd$dem_other <- 0
# icd9
for (c in codes_inpatient$code[codes_inpatient$disorder=='other dementia' & codes_inpatient$code_system=='ICD9']){
  icd$dem_other[icd$diagnosis==c & icd$version==9] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
# icd10
for (c in codes_inpatient$code[codes_inpatient$disorder=='other dementia' & codes_inpatient$code_system=='ICD10']){
  icd$dem_other[icd$diagnosis==c & icd$version==10] <- 1 # label diagnosis as present
  codes_inpatient$n[codes_inpatient$code==c] <- length(unique(icd$id[icd$diagnosis==c])) # count the number of occurrences
}
icd$date_dem_other <- icd$date; icd$date_dem_other[icd$dem_other==0] <- NA




## Final cleaning of inpatient diagnoses

# create a data frame with patients diagnosed with ischaemic stroke and arrange ascending by date of diagnosis 
stroke_i <- filter(subset(icd, select=c(id, stroke_i, date_stroke_i)), stroke_i==1) %>% arrange(date_stroke_i)
# create a data frame with patients diagnosed with haemorrhagic stroke and arrange ascending by date of diagnosis 
stroke_h <- filter(subset(icd, select=c(id, stroke_h, date_stroke_h)), stroke_h==1) %>% arrange(date_stroke_h)
# create a data frame with patients diagnosed with diabetes and arrange ascending by date of diagnosis 
diabetes <- filter(subset(icd, select=c(id, diabetes, date_diabetes)), diabetes==1) %>% arrange(date_diabetes)
# create a data frame with patients diagnosed with hypercholesterolaemia and arrange ascending by date of diagnosis 
hyperchol <- filter(subset(icd, select=c(id, hyperchol, date_hyperchol)), hyperchol==1) %>% arrange(date_hyperchol)
# create a data frame with patients diagnosed with hypertension and arrange ascending by date of diagnosis 
hypertension <- filter(subset(icd, select=c(id, hypertension, date_hypertension)), hypertension==1) %>% arrange(date_hypertension)
# create a data frame with patients diagnosed with AF and arrange ascending by date of diagnosis 
AF <- filter(subset(icd, select=c(id, AF, date_AF)), AF==1) %>% arrange(date_AF)
# create a data frame with patients diagnosed with AD and arrange ascending by date of diagnosis
AD <- filter(subset(icd, select=c(id, AD, date_AD)), AD==1) %>% arrange(date_AD)
# create a data frame with patients diagnosed with vascular dementia and arrange ascending by date of diagnosis
dem_vascular <- filter(subset(icd, select=c(id, dem_vascular, date_dem_vascular)), dem_vascular==1) %>% arrange(date_dem_vascular)
# same for other dementias
dem_other <- filter(subset(icd, select=c(id, dem_other, date_dem_other)), dem_other==1) %>% arrange(date_dem_other) 

# for each disorder, remove duplicates (retain the diagnosis on the earliest available date)
stroke_i <- stroke_i[!duplicated(stroke_i[,c('id')]),]
stroke_h <- stroke_h[!duplicated(stroke_h[,c('id')]),]
diabetes <- diabetes[!duplicated(diabetes[,c('id')]),]
hyperchol <- hyperchol[!duplicated(hyperchol[,c('id')]),]
hypertension <- hypertension[!duplicated(hypertension[,c('id')]),]
AF <- AF[!duplicated(AF[,c('id')]),]
AD <- AD[!duplicated(AD[,c('id')]),]
dem_vascular <- dem_vascular[!duplicated(dem_vascular[,c('id')]),]
dem_other <- dem_other[!duplicated(dem_other[,c('id')]),]

# merge 
icd_diagnosed <- merge(stroke_i, stroke_h, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, diabetes, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, hyperchol, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, hypertension, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, AF, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, AD, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, dem_vascular, by='id', all=TRUE)
icd_diagnosed <- merge(icd_diagnosed, dem_other, by='id', all=TRUE)

# merge with original dataset that contains all participants (also those without the relevant diagnoses)
icd <- distinct(icd, id, .keep_all = TRUE) # keep only one entry for each participant
icd <- subset(icd, select=c(id)) # remove unnecessary columns
icd <- merge(icd, icd_diagnosed, all = TRUE)

# create column indicating presence of any dementia
icd$dem_any <- rowSums(subset(icd, select=c(AD, dem_vascular, dem_other)), na.rm = TRUE)
icd$dem_any[icd$dem_any > 0] <- 1

# change NA's to 0's (indicating that the participant was not diagnosed with the relevant disorder)
icd$stroke_i[is.na(icd$stroke_i)] <- 0 
icd$stroke_h[is.na(icd$stroke_h)] <- 0 
icd$diabetes[is.na(icd$diabetes)] <- 0 
icd$hyperchol[is.na(icd$hyperchol)] <- 0 
icd$hypertension[is.na(icd$hypertension)] <- 0 
icd$AF[is.na(icd$AF)] <- 0 
icd$AD[is.na(icd$AD)] <- 0 
icd$dem_vascular[is.na(icd$dem_vascular)] <- 0 
icd$dem_other[is.na(icd$dem_other)] <- 0 
icd$dem_any[is.na(icd$dem_any)] <- 0 

# export
write.csv(icd, 'AF_dementias_inpatient.csv', row.names = FALSE)






### Add primary care diagnoses ###

## Import and prepare
meds_diagnoses <- read.csv('gp_clinical.txt', sep="\t", header=TRUE, quote="")

# rename columns
colnames(meds_diagnoses)[which(names(meds_diagnoses) == "eid")] <- "id"
colnames(meds_diagnoses)[which(names(meds_diagnoses) == "event_dt")] <- "date"
colnames(meds_diagnoses)[which(names(meds_diagnoses) == "read_3")] <- "diagnosis"
meds_diagnoses$read_2 <- NULL
meds_diagnoses$value1 <- NULL
meds_diagnoses$value2 <- NULL
meds_diagnoses$value3 <- NULL

# change class of all columns to character
meds_diagnoses <- as.data.frame(sapply(meds_diagnoses, as.character))




## Add a column for each relevant diagnosis

# ischaemic stroke
meds_diagnoses$stroke_i <- 0
for (c in codes_gp$code[codes_gp$disorder=='stroke_i']){
  meds_diagnoses$stroke_i[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_stroke_i <- meds_diagnoses$date; meds_diagnoses$date_stroke_i[meds_diagnoses$stroke_i==0] <- NA

# haemorrhagic stroke
meds_diagnoses$stroke_h <- 0
for (c in codes_gp$code[codes_gp$disorder=='stroke_h']){
  meds_diagnoses$stroke_h[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_stroke_h <- meds_diagnoses$date; meds_diagnoses$date_stroke_h[meds_diagnoses$stroke_h==0] <- NA

# diabetes
meds_diagnoses$diabetes <- 0
for (c in codes_gp$code[codes_gp$disorder=='diabetes']){
  meds_diagnoses$diabetes[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_diabetes <- meds_diagnoses$date; meds_diagnoses$date_diabetes[meds_diagnoses$diabetes==0] <- NA

# hypercholesterolemia
meds_diagnoses$hyperchol <- 0
for (c in codes_gp$code[codes_gp$disorder=='hypercholesterolemia']){
  meds_diagnoses$hyperchol[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_hyperchol <- meds_diagnoses$date; meds_diagnoses$date_hyperchol[meds_diagnoses$hyperchol==0] <- NA

# hypertension
meds_diagnoses$hypertension <- 0
for (c in codes_gp$code[codes_gp$disorder=='hypertension']){
  meds_diagnoses$hypertension[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_hypertension <- meds_diagnoses$date; meds_diagnoses$date_hypertension[meds_diagnoses$hypertension==0] <- NA

# AF
meds_diagnoses$AF <- 0
for (c in codes_gp$code[codes_gp$disorder=='af']){
  meds_diagnoses$AF[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_AF <- meds_diagnoses$date; meds_diagnoses$date_AF[meds_diagnoses$AF==0] <- NA

# AD
meds_diagnoses$AD <- 0
for (c in codes_gp$code[codes_gp$disorder=='adem']){
  meds_diagnoses$AD[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_AD <- meds_diagnoses$date; meds_diagnoses$date_AD[meds_diagnoses$AD==0] <- NA

# VaD
meds_diagnoses$dem_vascular <- 0
for (c in codes_gp$code[codes_gp$disorder=='vad']){
  meds_diagnoses$dem_vascular[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_dem_vascular <- meds_diagnoses$date; meds_diagnoses$date_dem_vascular[meds_diagnoses$dem_vascular==0] <- NA

# Other dementia
meds_diagnoses$dem_other <- 0
for (c in codes_gp$code[codes_gp$disorder=='dementia']){
  meds_diagnoses$dem_other[meds_diagnoses$diagnosis==c] <- 1 # label diagnosis as present
  codes_gp$n[codes_gp$code==c] <- length(unique(meds_diagnoses$id[meds_diagnoses$diagnosis==c])) # count the number of occurrences
}
meds_diagnoses$date_dem_other <- meds_diagnoses$date; meds_diagnoses$date_dem_other[meds_diagnoses$dem_other==0] <- NA

# diagnosis of any dementia
meds_diagnoses$dem_any <- rowSums(subset(meds_diagnoses, select=c(AD, dem_vascular, dem_other)), na.rm = TRUE)
meds_diagnoses$dem_any[meds_diagnoses$dem_any > 0] <- 1

# export primary care codes
write.csv(meds_diagnoses, 'AF_dementias_GP.csv', row.names = FALSE)

# combine inpatient- and GP- codes and their counts and export
codes_new <- rbind(codes_gp, codes_inpatient)
codes_new <- filter(codes_new, n!=0)
write.csv(codes_new, 'diagnoses_counts.csv', row.names = FALSE)







### Combine inpatient and GP files ###



gp <- read.csv('AF_dementias_GP.csv')
inpatient <- read.csv('AF_dementias_inpatient.csv') #load ICD-diagnoses
gp$date <- as.Date(gp$date,"%d/%m/%Y")

## create a data frame with patients diagnosed with a given disorder in the primary care or inpatient setting

# AF
# for both GP- and inpatient diagnoses subset only AF
AF_gp <- filter(subset(gp, select=c(id, AF, date)), AF==1) %>% arrange(date)
colnames(AF_gp) <- c('id', 'AF', 'date_AF')
AF_inpatient <- subset(inpatient %>% filter(AF==1), select=(c(id, AF, date_AF)))
colnames(AF_inpatient) <- c('id', 'AF', 'date_AF')
# add the inpatient diagnoses to the GP-diagnoses
AF <- rbind(AF_gp, AF_inpatient)
# remove invalid dates
AF <- filter(AF, date_AF!="1901-01-01" & date_AF!="1902-02-02" & date_AF!="1903-03-03" & date_AF!="2037-07-07")
# remove duplicate id's and keep the earliest date of diagnosis
AF <- AF %>% arrange(date_AF)
AF <- distinct(AF, id, .keep_all = TRUE)

# repeat for ischaemic stroke
stroke_i_gp <- filter(subset(gp, select=c(id, stroke_i, date)), stroke_i==1) %>% arrange(date)
colnames(stroke_i_gp) <- c('id', 'stroke_i', 'date_stroke_i')
stroke_i_inpatient <- subset(inpatient %>% filter(stroke_i==1), select=(c(id, stroke_i, date_stroke_i)))
colnames(stroke_i_inpatient) <- c('id', 'stroke_i', 'date_stroke_i')
stroke_i <- rbind(stroke_i_gp, stroke_i_inpatient)
stroke_i <- filter(stroke_i, date_stroke_i!="1901-01-01" & date_stroke_i!="1902-02-02" & date_stroke_i!="1903-03-03" & date_stroke_i!="2037-07-07")
stroke_i <- stroke_i %>% arrange(date_stroke_i)
stroke_i <- distinct(stroke_i, id, .keep_all = TRUE)

# repeat for haemorrhagic stroke
stroke_h_gp <- filter(subset(gp, select=c(id, stroke_h, date)), stroke_h==1) %>% arrange(date)
colnames(stroke_h_gp) <- c('id', 'stroke_h', 'date_stroke_h')
stroke_h_inpatient <- subset(inpatient %>% filter(stroke_h==1), select=(c(id, stroke_h, date_stroke_h)))
colnames(stroke_h_inpatient) <- c('id', 'stroke_h', 'date_stroke_h')
stroke_h <- rbind(stroke_h_gp, stroke_h_inpatient)
stroke_h <- filter(stroke_h, date_stroke_h!="1901-01-01" & date_stroke_h!="1902-02-02" & date_stroke_h!="1903-03-03" & date_stroke_h!="2037-07-07")
stroke_h <- stroke_h %>% arrange(date_stroke_h)
stroke_h <- distinct(stroke_h, id, .keep_all = TRUE)

# repeat for diabetes
diabetes_gp <- filter(subset(gp, select=c(id, diabetes, date)), diabetes==1) %>% arrange(date)
colnames(diabetes_gp) <- c('id', 'diabetes', 'date_diabetes')
diabetes_inpatient <- subset(inpatient %>% filter(diabetes==1), select=(c(id, diabetes, date_diabetes)))
colnames(diabetes_inpatient) <- c('id', 'diabetes', 'date_diabetes')
diabetes <- rbind(diabetes_gp, diabetes_inpatient)
diabetes <- filter(diabetes, date_diabetes!="1901-01-01" & date_diabetes!="1902-02-02" & date_diabetes!="1903-03-03" & date_diabetes!="2037-07-07")
diabetes <- diabetes %>% arrange(date_diabetes)
diabetes <- distinct(diabetes, id, .keep_all = TRUE)

# repeat for hypercholesterolaemia
hyperchol_gp <- filter(subset(gp, select=c(id, hyperchol, date)), hyperchol==1) %>% arrange(date)
colnames(hyperchol_gp) <- c('id', 'hyperchol', 'date_hyperchol')
hyperchol_inpatient <- subset(inpatient %>% filter(hyperchol==1), select=(c(id, hyperchol, date_hyperchol)))
colnames(hyperchol_inpatient) <- c('id', 'hyperchol', 'date_hyperchol')
hyperchol <- rbind(hyperchol_gp, hyperchol_inpatient)
hyperchol <- filter(hyperchol, date_hyperchol!="1901-01-01" & date_hyperchol!="1902-02-02" & date_hyperchol!="1903-03-03" & date_hyperchol!="2037-07-07")
hyperchol <- hyperchol %>% arrange(date_hyperchol)
hyperchol <- distinct(hyperchol, id, .keep_all = TRUE)

# repeat for hypertension
hypertension_gp <- filter(subset(gp, select=c(id, hypertension, date)), hypertension==1) %>% arrange(date)
colnames(hypertension_gp) <- c('id', 'hypertension', 'date_hypertension')
hypertension_inpatient <- subset(inpatient %>% filter(hypertension==1), select=(c(id, hypertension, date_hypertension)))
colnames(hypertension_inpatient) <- c('id', 'hypertension', 'date_hypertension')
hypertension <- rbind(hypertension_gp, hypertension_inpatient)
hypertension <- filter(hypertension, date_hypertension!="1901-01-01" & date_hypertension!="1902-02-02" & date_hypertension!="1903-03-03" & date_hypertension!="2037-07-07")
hypertension <- hypertension %>% arrange(date_hypertension)
hypertension <- distinct(hypertension, id, .keep_all = TRUE)

# repeat for AD
AD_gp <- filter(subset(gp, select=c(id, AD, date)), AD==1) %>% arrange(date)
colnames(AD_gp) <- c('id', 'AD', 'date_AD')
AD_inpatient <- subset(inpatient %>% filter(AD==1), select=(c(id, AD, date_AD)))
colnames(AD_inpatient) <- c('id', 'AD', 'date_AD')
AD <- rbind(AD_gp, AD_inpatient)
AD <- filter(AD, date_AD!="1901-01-01" & date_AD!="1902-02-02" & date_AD!="1903-03-03" & date_AD!="2037-07-07")
AD <- AD %>% arrange(AD)
AD <- distinct(AD, id, .keep_all = TRUE)

# repeat for vascular dementia
dem_vascular_gp <- filter(subset(gp, select=c(id, dem_vascular, date)), dem_vascular==1) %>% arrange(date)
colnames(dem_vascular_gp) <- c('id', 'dem_vascular', 'date_dem_vascular')
dem_vascular_inpatient <- subset(inpatient %>% filter(dem_vascular==1), select=(c(id, dem_vascular, date_dem_vascular)))
colnames(dem_vascular_inpatient) <- c('id', 'dem_vascular', 'date_dem_vascular')
dem_vascular <- rbind(dem_vascular_gp, dem_vascular_inpatient)
dem_vascular <- filter(dem_vascular, date_dem_vascular!="1901-01-01" & date_dem_vascular!="1902-02-02" & date_dem_vascular!="1903-03-03" & date_dem_vascular!="2037-07-07")
dem_vascular <- dem_vascular %>% arrange(date_dem_vascular)
dem_vascular <- distinct(dem_vascular, id, .keep_all = TRUE)

# repeat for other dementias
dem_other_gp <- filter(subset(gp, select=c(id, dem_other, date)), dem_other==1) %>% arrange(date)
colnames(dem_other_gp) <- c('id', 'dem_other', 'date_dem_other')
dem_other_inpatient <- subset(inpatient %>% filter(dem_other==1), select=(c(id, dem_other, date_dem_other)))
colnames(dem_other_inpatient) <- c('id', 'dem_other', 'date_dem_other')
dem_other <- rbind(dem_other_gp, dem_other_inpatient)
dem_other <- filter(dem_other, date_dem_other!="1901-01-01" & date_dem_other!="1902-02-02" & date_dem_other!="1903-03-03" & date_dem_other!="2037-07-07")
dem_other <- dem_other %>% arrange(date_dem_other)
dem_other <- distinct(dem_other, id, .keep_all = TRUE)

# repeat for dem_any
dem_any_inpatient <- subset(filter(inpatient, dem_any==1), select=c(id, dem_any, date_AD, date_dem_vascular, date_dem_other))
dem_any_inpatient$date_AD <- as.Date(dem_any_inpatient$date_AD,"%Y-%m-%d") # set date column as class date
dem_any_inpatient$date_dem_vascular <- as.Date(dem_any_inpatient$date_dem_vascular,"%Y-%m-%d") # set date column as class date
dem_any_inpatient$date_dem_other <- as.Date(dem_any_inpatient$date_dem_other,"%Y-%m-%d")
# get the earliest date for any inpatient dementia
dem_any_inpatient$date_dem_any <- apply(subset(dem_any_inpatient, select=c(date_AD, date_dem_vascular, date_dem_other)), 1, function(x) min(x, na.rm = TRUE))
dem_any_inpatient$date_AD <- NULL; dem_any_inpatient$date_dem_vascular <- NULL; dem_any_inpatient$date_dem_other <- NULL
dem_any_gp <- filter(subset(gp, select=c(id, dem_any, date)), dem_any==1) %>% arrange(date)
colnames(dem_any_gp) <- c('id', 'dem_any', 'date_dem_any')
dem_any <- rbind(dem_any_gp, dem_any_inpatient)
dem_any <- filter(dem_any, date_dem_any!="1901-01-01" & date_dem_any!="1902-02-02" & date_dem_any!="1903-03-03" & date_dem_any!="2037-07-07")
dem_any <- dem_any %>% arrange(date_dem_any)
dem_any <- distinct(dem_any, id, .keep_all = TRUE)

# merge all relevant disorders; this will create a data frame of participants that were diagnosed in either inpatient- or GP setting
AF_dementia_all <- merge(AF, hyperchol, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, stroke_i, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, stroke_h, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, diabetes, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, hypertension, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, AD, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, dem_vascular, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, dem_other, by='id', all = TRUE)
AF_dementia_all <- merge(AF_dementia_all, dem_any, by='id', all = TRUE)

# from the datasets with all participants, extract the id's and set the values of those that had not been diagnosed to 0
gp <- distinct(gp, id, .keep_all = TRUE)
gp <- subset(gp, select=c(id))
inpatient <- distinct(inpatient, id, .keep_all = TRUE)
inpatient <- subset(inpatient, select=c(id))
# create a data frame with all participants with either GP- or inpatient- records and supplement it with the diagnoses extracted above
non_diagnosed <- merge(gp, inpatient, all = TRUE)
diagnoses <- merge(AF_dementia_all, non_diagnosed, all = TRUE)

# replace NA's with 0's
diagnoses$stroke_i[is.na(diagnoses$stroke_i)] <- 0
diagnoses$stroke_h[is.na(diagnoses$stroke_h)] <- 0
diagnoses$AF[is.na(diagnoses$AF)] <- 0
diagnoses$diabetes[is.na(diagnoses$diabetes)] <- 0
diagnoses$hyperchol[is.na(diagnoses$hyperchol)] <- 0
diagnoses$hypertension[is.na(diagnoses$hypertension)] <- 0
diagnoses$AD[is.na(diagnoses$AD)] <- 0
diagnoses$dem_any[is.na(diagnoses$dem_any)] <- 0
diagnoses$dem_vascular[is.na(diagnoses$dem_vascular)] <- 0
diagnoses$dem_other[is.na(diagnoses$dem_other)] <- 0

# remove unnecessary columnsdiagnoses$data_provider <- NULL

# export
write.csv(diagnoses, 'AF_dementias_GP_inpatient.csv', row.names = FALSE)