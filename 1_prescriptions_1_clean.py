# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 12:02:08 2019

@author: jurem

The code below:
    1. Supplements the rows that are missing drug codes with codes from the Read_v2-reader.
    2. Removes from the sample (a) participants that have opted out of the study, (b) rows without prescription content, (c) rows without dates

The columns retained in the exported data frame are:
    - 'id': participant id
    - 'data_provider': 1= England(Vision), 2= Scotland, 3 = England (TPP), 4 = Wales
    - 'date': when the prescription was issued
    - 'prescription': full title of the prescription
    - 'quantity': quantity/dose of the prescribed drug, usually in mg
    
"""
import pandas as pd
import numpy as np




## Read in the data and prepare them
meds = pd.read_csv('gp_scripts_python.csv', header=0, sep=",", dtype = str, encoding = 'cp1252') # read in the prescriptions
meds.drop(['Unnamed: 0'], axis=1, inplace=True) # drop weird column
meds.columns = ['id', 'data_provider', 'date', 'read_code', 'bnf', 'dmd', 'prescription', 'quantity'] # re-name the columns
meds.loc[:,'prescription'] = meds.loc[:,'prescription'].str.lower() # convert prescription name to lowercase

codes = pd.read_csv('read-codes.csv', sep=",", dtype = str, encoding = "cp1252") # read in the codes
codes.columns = ['code', 'drug', 'status_flag'] # re-name the columns
codes['code'] = codes.code.astype(str) # change codes to strings
codes['code'] = codes.loc[:,'code'].apply(str.strip) # remove leading and trailing white spaces from read-codes in the read-code data frame
codes = codes.drop_duplicates(subset = 'code') # drop duplicate rows

meds.loc[meds['read_code'].isna(), 'read_code'] = 'unknown' # change NA values in read_code column into 'unknown'
meds['read_code'] = meds.loc[:,'read_code'].apply(str.strip) # remove white spaces from read-codes in the prescriptions data frame




## Some read-codes contain two 0s at the end; remove those.
# helper function to remove the additional 0's in some read-codes
def remove_00(code):
    if (code != 'unknown') & (len(code)==7) & (code[-2:len(code)]=='00'): # do not change the 'unknown'-strings, change only those with two 0s at the end
        new_code = code[0:-2] # retain everything but the last two characters of the string
        return new_code
    else:
        return code

meds['read_code'] = meds['read_code'].apply(remove_00) # run the helper function to remove the 00s





## Use the read-code list to supplement the data frame with prescription information.
# create a series with read code as index and drug name as column; then transform to dictionary 
read_code_dict = (codes.groupby('code')['drug'].apply(lambda x: x.tolist())).to_dict()
# the prescriptions are individual lists; transform them to strings
for key in read_code_dict.keys():
    read_code_dict[key] = ''.join(read_code_dict[key])

# helper code to fill read-codes into the 'meds' dataset
def find_read_code(code):
    try:
        read_code = read_code_dict[code]
    except: # if the code doesn't exist, flag as "unknown"
        read_code = 'unknown'
    return read_code

# create a column for read-code-supplemented information
meds['prescription_read'] = np.nan 
# plug each read-code in our sample into the dictionary as a key and create an additional column from the values
meds['prescription_read'] = meds.loc[:,'read_code'].apply(lambda x: find_read_code(x))
#convert to lowercase
meds.loc[:,'prescription_read'] = meds.loc[:,'prescription_read'].str.lower() 

# put read-code-supplied drugs into the drug column
meds.loc[meds['prescription'].isna(), 'prescription'] = meds.loc[meds['prescription'].isna(), 'prescription_read']
# change 'unknown' in prescription column back to NaN
meds.loc[meds['prescription']=='unknown', 'prescription'] = np.nan 




## misc. cleaning
# remove participants that have opted out of the study
opt_out = pd.read_csv('participant opt-out.csv')
opt_out.columns = ['id']
meds['id'] = meds['id'].astype(str)
opt_out['id'] = opt_out['id'].astype(str)
meds = meds.loc[~meds['id'].isin(opt_out.id)]
meds = meds.reset_index(drop=True)

# remove rows that have and empty "prescription" column
meds = meds.loc[meds['prescription'].notnull()]
# remove rows without a date
meds = meds.loc[~meds['date'].isna(), :]
#remove unnecessary columns
meds.drop(['read_code','bnf','dmd','prescription_read'], axis=1, inplace=True)
# change all prescriptions to strings
meds['prescription'] = meds.prescription.astype(str) 
# convert to lowercase
meds.loc[:,'prescription'] = meds['prescription'].str.lower() 
#remove potential white-space from the front of prescription names
meds['prescription'] = meds.loc[:,'prescription'].apply(str.strip)

#remove all '|' characters, as it will be used as a column separator
meds['prescription'] = meds['prescription'].str.replace('|', ' ')
#export to .csv
prescriptions = meds.to_csv('prescriptions_readv2.csv',index=False, header=True, sep='|')