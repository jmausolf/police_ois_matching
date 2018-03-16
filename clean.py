import pandas as pd
from glob import glob
import re
import unicodedata
import sys
import json
import subprocess


#Clear cleaned files to allow running-script numerous times
def rm_cleaned_files():
    subprocess.call("rm downloads/*_cleaned.csv", shell=True)


#Some Utility Functions for Cleaning the Data
def loadCSVs():
    police_files = glob('downloads/*{}*.csv'.format('police'))
    crowdsource_files = glob('downloads/*{}*.csv'.format('crowdsource'))
    return(police_files, crowdsource_files)


def remove_non_ascii_2(text):
    import re
    return re.sub(r'[^\x00-\x7F]+', "", text)


def remove_punct(text):
    import re
    return re.sub(r'[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]', "", text)


def clean_cols(df):
    df.columns = [remove_punct(x) for x in df.columns]
    df.columns = [x.lower().replace(' ', '') for x in df.columns]
    return(df)


def split_subjects(var, df):
    #Make a Copy
    df['original'] = df[var]

    #Split on Subjects by ';'
    s = df[var].str.replace(', ', '_').str.replace('; ', ';').str.split(';')
    s.name = var
    df = df.drop(var, axis=1).join(s)

    #Convert Multiple Subjects to Multiple Rows
    s = df.apply(lambda x: pd.Series(x[var]),axis=1).stack().reset_index(level=1, drop=True)
    s.name = var
    df = df.drop(var, axis=1).join(s)
    return(df)


def split_subjects_gv(var, df):
    #Make a Copy
    df['original'] = df[var]

    #Split on Subjects by '}, {', will not allow replace '[{' or '}]'
    s = df[var].str.replace('}, {', 'x_x, x_x')
    s = s.str.replace('{', '').str.replace('}', '')
    s = s.str.replace('[', '').str.replace(']', '').str.split("x_x, x_x")
    s.name = var
    df = df.drop(var, axis=1).join(s)

    #Convert Multiple Subjects to Multiple Rows
    s = df.apply(lambda x: pd.Series(x[var]),axis=1).stack().reset_index(level=1, drop=True)
    s.name = var
    df = df.drop(var, axis=1).join(s)

    #Turn back into a dictionary
    df[var] = ('{' + df[var] + '}')
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


def reverse_names(var, df, lower=True, delim='_'):
    d = delim
    if lower is True:
        s = df[var].apply(lambda x: d.join(x.split(d)[::-1])).str.replace(d, ' ').str.lower()
    else:
        s = df[var].apply(lambda x: d.join(x.split(d)[::-1])).str.replace(d, ' ')   
    df = df.drop(var, axis=1).join(s)
    return(df)

def lower_var(var, df):
    s = df[var].str.lower()
    df = df.drop(var, axis=1)
    df = pd.concat([df, s], axis=1)
    return(df)

def title_var(var, df):
    s = df[var].str.title()
    df = df.drop(var, axis=1)
    df = pd.concat([df, s], axis=1)
    return(df)


def lower_var_rm_nonascii(var, df):
    print("remove_non_ascii_2")
    s = df[var].str.lower().replace('\u201c', '')
    df = df.drop(var, axis=1).join(s)
    return(df)


def ren(invar, outvar, df):
    df.rename(columns={invar:outvar}, inplace=True)
    return(df)


def map_dict_col(var, df):
    """
    ## Maps a col containing dict's to seperate columns
    ## Expected variable cell: '{u'key': u'value', u'key': u'value'}
    """
    s = df[var].map(eval)
    df = pd.concat([df.drop([var], axis=1), s.apply(pd.Series)], axis=1)
    return df


#Clean police data frame
def clean_dfw_police_ois():
    print("[*] cleaning police ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('dfw', 'police'))[0].replace('.csv', '')
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


def clean_den_police_ois():
    print("[*] cleaning police ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('den', 'police'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = split_vars('citystate', 'city', 'state', ',', df)
    df = reverse_names('lastfirstname', df, delim=',')
    df = ren('lastfirstname', 'name', df)
    df = ren('incidentdate', 'date', df)
    df = lower_var('casualty', df)
    print(df.head(10))
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)




#Clean crowdsource data frame
def clean_wp_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('wp', 'crowdsource'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = lower_var('name', df)
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)


def clean_gv_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('downloads/*{}*.tsv'.format('crowdsource'))[0].replace('.tsv', '')
    print(infile)
    df = pd.read_csv('{}.tsv'.format(infile), delimiter='\t', encoding='utf-8')
    df = clean_cols(df)

    df = split_subjects_gv('infoaboutparticipants', df)
    df = map_dict_col('infoaboutparticipants', df)
    df = lower_var('name', df)

    #TODO remove unicode punct
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)


def clean_gd_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infiles = glob('downloads/{}*{}*.csv'.format('gd', 'crowdsource'))
    infiles = [file.replace('.csv', '') for file in infiles]
    outfile = infiles[0].rsplit('-', 1)[0]

    df1 = pd.read_csv('{}.csv'.format(infiles[0]))
    df2 = pd.read_csv('{}.csv'.format(infiles[1]))
    df = df1.append(df2, ignore_index=True)
    df = ren('raceethnicity', 'race', df)
    df = lower_var('name', df)
    df.to_csv('{}_cleaned.csv'.format(outfile), index=False)



def clean_ds_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('ds', 'crowdsource'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = ren('name', 'other_name', df)
    df = ren('victimname', 'name', df)
    df = lower_var('name', df)
    df = title_var('city', df)
    df = split_vars('state', 'state_abv', 'state_name', '-', df)
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)







