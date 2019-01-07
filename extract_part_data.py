import pandas as pd
import numpy as np
import os


"""
Takes a coventure dataset csv file, parses each participant keeping only
the action and outcome and outputs a text file with those two columns for each trial

"""

mainDir = "/Users/spinz/Projects/QlearnPalp/"
# Path to csv file
#path = "/Users/spinz/Projects/VirtualEnvs/RL_palp/COVENTURE-COVENTURE_PALP_EN-BASIC_DIGEST.csv"

paths = [os.path.join(mainDir,'data','COVENTURE_PALP_Y{}.csv'.format(i)) for i in range(1,6)]
ex_path = os.path.join(mainDir,'data','PALP_EXCLUDE_LIST.csv')
outpaths = [os.path.join(mainDir,'data','extractedY{}/'.format(i)) for i in range(1,6)]

dataframes = [pd.read_csv(path) for path in paths]

# Get exclude list as dataframe
excdf = pd.read_csv(ex_path)
# Merge all columns together
allexcludes = excdf['Baseline'].tolist()
allexcludes.extend(excdf['Year.2'].tolist())
allexcludes.extend(excdf['Year.3'].tolist())
allexcludes.extend(excdf['Year.4'].tolist())
allexcludes.extend(excdf['Year.5'].tolist())
excludeset = set(allexcludes)
#allset = set(data['User code'].tolist())

for data, outpath in zip(dataframes,outpaths):

    if not os.path.exists(outpath):
      os.makedirs(outpath)

    data = data[~data['User code'].isin(excludeset)]

    # We don't care about practice trials, those are for the parts:
    data = data[~data['Block'].str.contains('practice')]
    data = data[~data['Block'].str.contains('counter_balance')]
    data = data[~data['User code'].str.contains('TEST')]
    data = data[~data['Trial'].str.contains('route|summary')]


    #### END OF CLEANING ####\
    #### BEGIN INDICATOR VARIABLES ####

    # Make new binary block variable
    data['block'] = 0
    data['block'][data['Block'].str.contains('RR')]= 1

    # Make pretreatment cues variable
    data['pretreat'] = 0
    data['pretreat'][data['Block'].str.contains('pretreat')] = 1

    # Make cue variable
    data['cue'] = data['Trial']

    # Make response numerical
    data['Response'] = data['Response'].map({'space':1, np.nan:0})
    data['choice'] = data['Response']

    # Make rewards numerical based on condition
    data['outcome'] = data['Trial result']
    data['outcome'] = data['outcome'].map({'CORRECT_REWARD': 2, 'CORRECT_NONE': 1, 'INCORRECT_NONE': -1, 'INCORRECT_PUNISH': -2})

    data['id'] = data['User code']
    ##### END OF VARIABLE CREATION #####

    # Keep only certain cols:
    data = data[['id', 'block', 'cue', 'choice', 'outcome', 'pretreat']]

    # Groupby participants:
    grouped = data.groupby(by='id')

    all_scores={'Subject': [], 'Score': []}

    # Get part data, and save to text file:
    for part in grouped.groups.keys():
        partdata = grouped.get_group(part)
        fname = os.path.join(outpath, ('PALP_'+ part + '.txt'))
        all_scores['Subject'].append(part)
        all_scores['Score'].append(partdata['outcome'].sum())
        #all_scores.append(partdata['Trial result'].sum())
        partdata.to_csv(fname, header=None, index=None)

    asdf = pd.DataFrame.from_dict(all_scores)
print("Done.")
