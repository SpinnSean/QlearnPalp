import pandas as pd


# Coventure matrix
covdat = pd.read_csv("/Volumes/Storage/Work/Data/Coventure/COVENTURE_COMPLETE.csv",usecols=['Year.5', 'Year.4', 'Year.3', 'Year.2', 'Baseline'])

# PALP data
palpEN = pd.read_csv("/Volumes/Storage/Work/Data/Coventure/COVENTURE-COVENTURE_PALP_EN-BASIC_DIGEST.csv")
palpFR = pd.read_csv("/Volumes/Storage/Work/Data/Coventure/COVENTURE-COVENTURE_PALP_FR-BASIC_DIGEST.csv")

# Combine both french and english
palp = pd.concat([palpEN, palpFR], ignore_index=True)


# Add another column to palp df which represents the year
palp['time'] = 'NaN'
palp['time'][palp['User code'].isin(covdat['Baseline'])] = 1
palp['time'][palp['User code'].isin(covdat['Year.2'])] = 2
palp['time'][palp['User code'].isin(covdat['Year.3'])] = 3
palp['time'][palp['User code'].isin(covdat['Year.4'])] = 4
palp['time'][palp['User code'].isin(covdat['Year.5'])] = 5

# Groupby year, and save as seperate files
gp = palp.groupby(by='time')
palp_Y1 = gp.get_group(1)
palp_Y2 = gp.get_group(2)
palp_Y3 = gp.get_group(3)
palp_Y4 = gp.get_group(4)
palp_Y5 = gp.get_group(5)

# Save
palp_Y1.to_csv("/Users/spinz/Projects/QlearnPalp/COVENTURE_PALP_Y1.csv", index=None)
palp_Y2.to_csv("/Users/spinz/Projects/QlearnPalp/COVENTURE_PALP_Y2.csv", index=None)
palp_Y3.to_csv("/Users/spinz/Projects/QlearnPalp/COVENTURE_PALP_Y3.csv", index=None)
palp_Y4.to_csv("/Users/spinz/Projects/QlearnPalp/COVENTURE_PALP_Y4.csv", index=None)
palp_Y5.to_csv("/Users/spinz/Projects/QlearnPalp/COVENTURE_PALP_Y5.csv", index=None)
