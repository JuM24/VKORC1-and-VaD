library(tidyverse)
library(broom)
library(HardyWeinberg)




load("warfarin_data.RData")




## Descriptive stats

# HWE
hw <- HWChisq(c(AA=82712,AB=99117,BB=29594), verbose=T)

# covariates
min(warfarin$age, na.rm=TRUE); max(warfarin$age, na.rm=TRUE)
median(warfarin$age, na.rm=TRUE); IQR(warfarin$age, na.rm=TRUE)
table(warfarin$sex); prop.table(table(warfarin$sex))
table(warfarin$education); prop.table(table(warfarin$education))
median(warfarin$deprivation, na.rm=TRUE); IQR(warfarin$deprivation, na.rm=TRUE)
table(warfarin$alc_freq); prop.table(table(warfarin$alc_freq))
table(warfarin$smoking); prop.table(table(warfarin$smoking))
table(warfarin$activity); prop.table(table(warfarin$activity))
median(warfarin$bmi, na.rm=TRUE); IQR(warfarin$bmi, na.rm=TRUE)

# AF diagnosi & warfarin
table(warfarin$AF); prop.table(table(warfarin$AF))
table(warfarin$binary_warfarin); prop.table(table(warfarin$binary_warfarin))
table(warfarin$AF, warfarin$binary_warfarin)
median(warfarin$age[warfarin$AF=='1' & warfarin$binary_warfarin=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$AF=='1' & warfarin$binary_warfarin=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$AF=='0' & warfarin$binary_warfarin=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$AF=='0' & warfarin$binary_warfarin=='0'], na.rm = TRUE)

# VKORC1 polymorphism and dementias
table(warfarin$vkor_carrier); prop.table(table(warfarin$vkor_carrier))
table(warfarin$rs9923231_T); prop.table(table(warfarin$rs9923231_T))
table(warfarin$AD); prop.table(table(warfarin$AD))
table(warfarin$dem_vascular); prop.table(table(warfarin$dem_vascular))
table(warfarin$AD, warfarin$dem_vascular)
table(warfarin$rs9923231_T, warfarin$dem_any)
median(warfarin$age[warfarin$rs9923231_T=='0' & warfarin$dem_any=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='0' & warfarin$dem_any=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='1' & warfarin$dem_any=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='1' & warfarin$dem_any=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='2' & warfarin$dem_any=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='2' & warfarin$dem_any=='0'], na.rm = TRUE)
table(warfarin$rs9923231_T, warfarin$AD)
median(warfarin$age[warfarin$rs9923231_T=='0' & warfarin$AD=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='0' & warfarin$AD=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='1' & warfarin$AD=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='1' & warfarin$AD=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='2' & warfarin$AD=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='2' & warfarin$AD=='0'], na.rm = TRUE)
table(warfarin$rs9923231_T, warfarin$dem_vascular)
median(warfarin$age[warfarin$rs9923231_T=='0' & warfarin$dem_vascular=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='0' & warfarin$dem_vascular=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='1' & warfarin$dem_vascular=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='1' & warfarin$dem_vascular=='0'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='2' & warfarin$dem_vascular=='1'], na.rm = TRUE)
median(warfarin$age[warfarin$rs9923231_T=='2' & warfarin$dem_vascular=='0'], na.rm = TRUE)

# prevalence of vascular risk factors
table(warfarin$hypertension)
table(warfarin$hyperchol)
table(warfarin$dem_vascular, warfarin$hypertension)
table(warfarin$dem_vascular, warfarin$hyperchol)




## Modelling

# does dementia diagnosis in parents predict dementia in patient
m <- glm(data=warfarin, AD ~ AD_parent + age + sex + age_dad + age_mum + smoking + alc_freq + education + deprivation + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AD_parent + age + sex + age_dad + age_mum + smoking + alc_freq + education + deprivation + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# association between average warfarin dose and VKORC1 polymorphism
m <- lm(data=warfarin, dose_avg ~ rs9923231_T + sex + age + education + deprivation + smoking + alc_freq + activity + bmi +
          ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
          PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
          PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40, na.action = "na.exclude")
# association between allele of interest and AD in parents
m <- glm(data=warfarin, AD_parent ~ rs9923231_T + age + sex + age_dad + age_mum + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# association between allele of interest and ADem/VaD in patient
# ADem
m <- glm(data=warfarin, AD ~ rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# Any dementia
m <- glm(data=warfarin, dem_any ~ rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# VaD
m <- glm(data=warfarin, dem_vascular ~ rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# is there an association between ADem/VaD in participants and AF
m <- glm(data=warfarin, AD ~ AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + hypertension + hyperchol,
         family="binomial", na.action = "na.exclude")


# are patients with the VKOR allele that receive warfarin more prone to develop dementia?
m <- glm(data=warfarin, dem_vascular ~ AF*rs9923231_T*total_warfarin+ age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, AD_parent ~ AF*rs9923231_T*total_warfarin + age_mum + age_dad + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")



## Suppl. analyses

# VaD
m <- glm(data=warfarin, dem_vascular ~ rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ total_warfarin + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ rs9923231_T + total_warfarin + AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# ADem
m <- glm(data=warfarin, AD ~ rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, AD ~ total_warfarin + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, AD ~ AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, AD ~ rs9923231_T + total_warfarin + AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
# any dementia
m <- glm(data=warfarin, dem_any ~ rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_any ~ total_warfarin + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_any ~ AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_any ~ rs9923231_T + total_warfarin + AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")

# two-way interactions
m <- glm(data=warfarin, dem_vascular ~ AF*rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ rs9923231_T*total_warfarin+ age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AF*total_warfarin+ age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AF*rs9923231_T + total_warfarin + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ rs9923231_T*total_warfarin + AF + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
m <- glm(data=warfarin, dem_vascular ~ AF*total_warfarin + rs9923231_T + age + sex + education + deprivation + smoking + alc_freq + activity + bmi +
           ass_centre + Batch + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
           PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + 
           PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40,
         family="binomial", na.action = "na.exclude")
