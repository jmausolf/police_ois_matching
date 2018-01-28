import pandas as pd
from glob import glob
import re



def loadCSVs():

    #company = str(company).replace(" ", "_")
    police_files = glob('*{}*.csv'.format('police'))
    crowdsource_files = glob('*{}*.csv'.format('crowdsource'))
    #assert len(filenames) == 1, "Only one file expected, check files and perform merge first..."
    print(police_files)
    for pf in police_files:
    	print(pf)
    	df = pd.read_csv(pf)
    	print(df.head(5))


def remove_non_ascii_2(text):
    import re
    return re.sub(r'[^\x00-\x7F]+', "", text)

def remove_punct(text):
    import re
    return re.sub(r'[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]', "", text)


#df = pd.read_csv('dfw_police_ois_report_2018-01-24.csv')

def clean_cols(df):
    df.columns = [remove_punct(x) for x in df.columns]
    df.columns = [x.lower().replace(' ', '') for x in df.columns]
    return(df)

def split_subjects(var, df):
    #Split on Subjects by ';'
    s = df[var].str.replace(', ', '_').str.replace('; ', ';').str.split(';')
    s.name = var
    df = df.drop(var, axis=1).join(s)

    #Convert Multiple Subjects to Multiple Rows
    s = df.apply(lambda x: pd.Series(x[var]),axis=1).stack().reset_index(level=1, drop=True)
    s.name = var
    df = df.drop(var, axis=1).join(s)
    return(df)

def split_vars(ovar, nvar1, nvar2, delim, df):
    df[nvar1], df[nvar2] = df[ovar].str.split(delim, 1).str
    df.drop([ovar], axis=1, inplace=True)
    return(df)

def split_race_gender(df):
    #Split Race/Gender to New Vars
    df['name'], df['race_gender'] = df['subjects'].str.split(' ', 1).str
    df['race'], df['gender'] = df['race_gender'].str.split('/', 1).str
    df.drop(['subjects', 'race_gender'], axis=1, inplace=True)
    return(df)

def reverse_names(var, df, lower=True):
    if lower is True:
        s = df[var].apply(lambda x: '_'.join(x.split('_')[::-1])).str.replace('_', ' ').str.lower()
    else:
        s = df[var].apply(lambda x: '_'.join(x.split('_')[::-1])).str.replace('_', ' ')   
    df = df.drop(var, axis=1).join(s)
    return(df)

def lower_var(var, df):
	s = df[var].str.lower()
	df = df.drop(var, axis=1).join(s)
	return(df)

def ren(invar, outvar, df):
    df.rename(columns={invar:outvar}, inplace=True)
    return(df)


#police data frame
infile = 'dfw_police_ois_report_2018-01-28'
df = pd.read_csv('{}.csv'.format(infile))
df = clean_cols(df)
df = split_subjects('subjects', df)
df = split_race_gender(df)
df = reverse_names('name', df)
ren('subjectdeceasedinjuredorshootandmiss', 'outcome', df)
ren('attorneygeneralformsurl', 'ag_url', df)
df = lower_var('outcome', df)
print(df.head(10))
df.to_csv('{}_cleaned.csv'.format(infile), index=False)

#wp df
#police data frame
infile = 'wp_crowdsource_ois_report_2018-01-28'
df = pd.read_csv('{}.csv'.format(infile))
df = clean_cols(df)
df = lower_var('name', df)
#print(cdf.head(10))
df.to_csv('{}_cleaned.csv'.format(infile), index=False)



