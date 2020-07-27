# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 11:18:18 2019

@author: JMur

This code (1) extracts and homogenizes the formatting of the prescribed doses of warfarin, (2) creates a data frame with a row for each paricipant, 
and (3) calculates the total number of warfarin prescriptions for each participant.

"""

import pandas as pd
import numpy as np
import re


   
## Homogenize the formatting of warfarin doses

# read in the cleaned file with prescriptions
meds = pd.read_csv('3_aa_scales.csv', header=0, sep="|", dtype = str, encoding = 'cp1252')

# function to find the desired pattern
def extractDose(prescription, pattern):   
    dosage = re.search(pattern, prescription)
    if dosage:       
        return dosage.group()
    else: return 'unknown'

# regex patterns to look for different formats/spellings of warfarin dose
# all of them take into account variations (divided by '|'), e.g., amount followed by 'mg' (11mg) or amount followed by a space and then by 'mg' (1 mg)
micrograms_ml = r'([0-9.]+[m][i][c][r][o][g][r][a][m][s][/][m][l])|([0-9.]+\s[m][i][c][r][o][g][r][a][m][s][/][m][l])'
microgram_ml = r'([0-9.]+[m][i][c][r][o][g][r][a][m][/][m][l])|([0-9.]+\s[m][i][c][r][o][g][r][a][m][/][m][l])'
mg_ml = r'([0-9.]+[m][g][/][m][l])|([0-9.]+\s[m][g][/][m][l])'
microgram = r'([0-9.]+[m][i][c][r][o][g][r][a][m])|([0-9.]+\s[m][i][c][r][o][g][r][a][m])'    
mg = r'([0-9.]+[m][g])|([0-9.]+\s[m][g])'
mcg = r'([0-9.]+[m][c][g])|([0-9.]+\s[m][c][g])'
g = r'([0-9.]+[g])|([0-9.]+\s[g])'
ml = r'([0-9.]+[m][l])|([0-9.]+\s[m][l])'
micrograms_dose = r'([0-9.]+[m][i][c][r][o][g][r][a][m][s][/][d][o][s][e])|([0-9.]+\s[m][i][c][r][o][g][r][a][m][s][/][d][o][s][e])'
microgram_dose = r'([0-9.]+[m][i][c][r][o][g][r][a][m][/][d][o][s][e])|([0-9.]+\s[m][i][c][r][o][g][r][a][m][/][d][o][s][e])'
units_ml = r'([0-9.]+[u][n][i][t][s][/][m][l])|([0-9.]+\s[u][n][i][t][s][/][m][l])'
unit_ml = r'([0-9.]+[u][n][i][t][/][m][l])|([0-9.]+\s[u][n][i][t][/][m][l])'
unit = r'([0-9.]+[u][n][i][t])|([0-9.]+\s[u][n][i][t])'
dose = r'([0-9.]+[d][o][s][e])|([0-9.]+\s[d][o][s][e])'
cap = r'(\s[c][a][p][0-9.]+)|(\s[c][a][p][0-9.]+)'
tab = r'(\s[t][a][b][0-9.]+)|(\s[t][a][b][0-9.]+)'
per = r'([0-9.]+[%])|([0-9.]+\s[%])'    

patterns = [micrograms_ml, microgram_ml, mg_ml, microgram, mg, mcg, g, ml, micrograms_dose, microgram_dose, units_ml, unit_ml, unit, dose, cap, tab, per] # create a list of patterns defined above
meds['dose'] = 'unknown' #create a column with dose
# the code below gives precedence to patterns listed earlier in the list, as it searches only among prescriptions with 'unknown' dosage
patterns_left = len(patterns)
for pattern in patterns:
    print ('Working on pattern ' + str(pattern) + '. ' + '\n' + 'Patterns left: ' + str(patterns_left) + '.')
    pattern_rows = meds.loc[(meds['dose']=='unknown'), 'prescription'].apply(lambda x: extractDose(x,pattern)) # find rows with pattern (exact word match)
    pattern_indices = list((pattern_rows[pattern_rows!='unknown']).index) # mark the rows/indices of the data frame where the pattern was found
    meds.loc[pattern_indices,'dose'] = pattern_rows[pattern_indices] # supplement dose with found pattern for rows that contain the pattern
    # repeat for 'quantity' column, as some prescriptions have the dosage entered there
    pattern_rows = meds.loc[(~meds['quantity'].isna()) & (meds['dose']=='unknown'), 'quantity'].apply(lambda x: extractDose(x,pattern)) # find rows with pattern (exact word match) VERY SLOW BECAUSE OF REGEX
    pattern_indices = list((pattern_rows[pattern_rows!='unknown']).index) # mark the rows/indices of the data frame where the pattern was found
    meds.loc[pattern_indices,'dose'] = pattern_rows[pattern_indices] # supplement dose with found pattern for rows that contain the pattern
    patterns_left -= 1

# with some prescriptions that only have a number for dose (and no measure/unit), it is clear what the number is referring to; so fill those in
meds.loc[meds['dose'] == 'unknown', 'dose'] = meds.loc[meds['dose'] == 'unknown', 'prescription'].apply(lambda x: extractDose(x, r'[1-9]'))
 
# identify the dose measure/units and potentially convert to milligram
# create a new column for numeric dosage
meds['dose_numeric'] = meds['dose']
# extract numbers from prescriptions that are in milligram
meds.loc[meds['prescription'].str.contains('mg', na=False, regex=True), 'dose_numeric'] = \
    meds.loc[meds['prescription'].str.contains('mg', na=False, regex=True), 'dose'].apply(lambda x: extractDose(x, r'[0-9.]+'))
# extract numbers from prescriptions that are in microgram
meds.loc[meds['prescription'].str.contains('microgram', na=False, regex=True), 'dose_numeric'] = \
    meds.loc[meds['prescription'].str.contains('microgram', na=False, regex=True), 'dose'].apply(lambda x: extractDose(x, r'[0-9]+'))
# convert to milligram
meds.loc[meds['scale_name']!='warfarin', 'dose_numeric'] = 0
meds['dose_numeric'] = meds.loc[meds['dose_numeric'] != 'unknown', 'dose_numeric'].astype(float)
meds.loc[(meds['dose_numeric'] != 'unknown') & (meds['prescription'].str.contains('microgram', na=False, regex=True)), 'dose_numeric'] = \
    meds.loc[(meds['dose_numeric'] != 'unknown') & (meds['prescription'].str.contains('microgram', na=False, regex=True)), 'dose_numeric']/1000




## Create a participant/id-based dataset.

# change impossible dates to NA
before = len(meds); before_p = len(set(meds.id))
meds.loc[(meds['date']=="01/01/1901") | (meds['date']=="02/02/1902") | (meds['date']=="03/03/1903")] = np.NaN 
meds.loc[meds['date']=="07/07/2037"] = np.NaN 

# remove duplicates
meds = meds.drop_duplicates()

#get id's
ids_index = meds.groupby('id').agg(total_prescriptions=('prescription','count')) 
ids = meds.groupby('id', as_index=False).agg(total_prescriptions=('prescription','count')) 
ids['id'] = pd.Series(ids_index.index, index=ids.index)

# create columns that flag different dosages
meds['warfarin_present'] = 0
meds.loc[meds['scale_name']=='warfarin', 'warfarin_present'] = 1
meds['warfarin_present'] = meds['warfarin_present'].astype(int)
meds.loc[(meds['dose_numeric']=='unknown') | (meds['dose_numeric']==0), 'dose_numeric'] = np.nan
meds['dose_numeric'] = meds['dose_numeric'].astype(float)
meds['dose_05'] = 0
meds['dose_1'] = 0
meds['dose_3'] = 0
meds['dose_5'] = 0
meds.loc[meds['dose_numeric']==0.5, 'dose_05'] = 1
meds.loc[meds['dose_numeric']==1, 'dose_1'] = 1
meds.loc[meds['dose_numeric']==3, 'dose_3'] = 1
meds.loc[meds['dose_numeric']==5, 'dose_5'] = 1

#create new data frame
meds_person = meds.groupby('id', as_index=True).agg(total_warfarin=('warfarin_present','sum'), dose_sum =('dose_numeric','sum'), 
                           dose_05=('dose_05','sum'), dose_1=('dose_1','sum'), dose_3=('dose_3','sum'), dose_5=('dose_5','sum'))
meds_person = pd.merge(ids, meds_person, on='id', how='left')

#export to .csv
prescriptions = meds_person.to_csv('prescriptions_warfarin_people.csv',index=False, header=True, sep='|')


