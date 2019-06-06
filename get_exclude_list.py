import os
import pandas as pd


# Project directory
main_dir = "/Users/sean/Projects/QlearnPalp"

f = os.path.join(main_dir,'data','COVENTURE_COMPLETE.csv')

df = pd.read_csv(f)

skip_dict = {'Baseline':[],
             'Year.2':[],
             'Year.3':[],
             'Year.4':[],
             'Year.5':[]}

trans_dict = {'Baseline': ['PALP_QC_PALP_TOOFAST_Y1', 'PALP_QC_PALP_WORSE_THAN_CHANCE_Y1'],
             'Year.2': ['PALP_QC_PALP_TOOFAST_Y2', 'PALP_QC_PALP_WORSE_THAN_CHANCE_Y2'],
             'Year.3': ['PALP_QC_PALP_TOOFAST_Y3', 'PALP_QC_PALP_WORSE_THAN_CHANCE_Y3'],
             'Year.4': ['PALP_QC_PALP_TOOFAST_Y4', 'PALP_QC_PALP_WORSE_THAN_CHANCE_Y4'],
             'Year.5': ['PALP_QC_PALP_TOOFAST_Y5', 'PALP_QC_PALP_WORSE_THAN_CHANCE_Y5']}

for i in skip_dict.keys():
    flags = trans_dict[i]
    curr_list = df[i][(df[flags[0]] == 1) | (df[flags[1]] == 1)].tolist()
    skip_dict[i].extend(curr_list)


skipdf = pd.DataFrame().from_dict(skip_dict, orient='index')
skipdf = skipdf.transpose()
skipdf.to_csv(os.path.join(main_dir,'data','PALP_EXCLUDE_LIST.csv'), index=None)
