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


def replace_char(var, df, inchar=',', outchar='_'):

    s = df[var].str.replace(inchar, outchar)
    s.name = var
    df = df.drop(var, axis=1)
    df = pd.concat([df, s], axis=1)
    return(df)


def rm_mid_initials_suffixes(var, df):

    #replace text preceding ".", e.g. jr. sen.
    s = df[var].str.replace(r"[a-z]+\.", '')

    #replace text following a comma
    #ensure names are reversed previously
    s = s.str.replace(r"\,\s[a-z]+", '')


    s = s.str.replace(' jr', ' ').str.replace(' junior', ' ')
    s = s.str.replace(' sr', ' ').str.replace(' senior', ' ')
    s = s.str.replace(' iii', ' ').str.replace(' third', ' ').str.replace(' the third', ' ')
    s = s.str.replace(' iv', ' ').str.replace(' fourth', ' ').str.replace(' the fourth', ' ')
    s = s.str.strip()

    #replace more than one white space with a space
    s = s.str.replace(r"\s{2,}", ' ')

    s.name = var
    df = df.drop(var, axis=1)
    df = pd.concat([df, s], axis=1)
    return(df)



def keep_first_last(namelist):
    try:
        if len(namelist) >= 3:
            first = namelist[0]
            last = namelist[-1]
            first_last = [first, last]
            name = " ".join(first_last).strip()

        else:
            name = " ".join(namelist).strip()

        return name

    except Exception as e:
        print("[*] Error: {}, exception when removing middle name...".format(e))
        return namelist

def rm_middle_name(var, df):
    s = df[var].fillna('unknown')
    s = s.str.split(r'\s')
    s = s.apply(lambda name: keep_first_last(name))
    s.name = var
    df = df.drop(var, axis=1)
    df = pd.concat([df, s], axis=1)
    return(df)


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

def split_subjects_2vars(var1, sep1, var2, sep2, df):

    s1 = df[var1].str.split(sep1, expand=True).stack().str.strip().reset_index(level=1, drop=True)
    s2 = df[var2].str.split(sep2, expand=True).stack().str.strip().reset_index(level=1, drop=True)
    
    print(s1.shape)
    print(s2.shape)
    df1 = pd.concat([s1,s2], axis=1, keys=[var1, var2])

    df = df.drop([var1, var2], axis=1).join(df1).reset_index(drop=True)
    return df


def split_sep_var(var, sep, df):
    return df[var].str.split(sep, expand=True).stack().str.strip().reset_index(level=1, drop=True)
    

def split_subjects_nvars(vslist, df):

    varlist = []
    seplist = []
    series_list = []

    for vs in vslist:
        var = vs[0]
        sep = vs[1]

        varlist.append(var)
        seplist.append(sep)

        s = split_sep_var(var, sep, df)
        series_list.append(s)

    df1 = pd.concat(series_list, axis=1, keys=varlist)
    df = df.drop(varlist, axis=1).join(df1).reset_index(drop=True)
    return df



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
    #df = df.drop(var, axis=1).join(s)
    df = df.drop(var, axis=1)
    df = pd.concat([df, s], axis=1)
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
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    ren('subjectdeceasedinjuredorshootandmiss', 'outcome', df)
    ren('attorneygeneralformsurl', 'ag_url', df)
    df = lower_var('outcome', df)
    print(df.head(5))
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)


def clean_den_police_ois():
    print("[*] cleaning police ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('den', 'police'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = split_vars('citystate', 'city', 'state', ',', df)
    df = reverse_names('lastfirstname', df, delim=',')
    df = ren('lastfirstname', 'name', df)
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    df = ren('incidentdate', 'date', df)
    df = lower_var('casualty', df)
    print(df.head(5))
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)


def clean_jax_police_ois():
    print("[*] cleaning police ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('jax', 'police'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = reverse_names('suspectname', df, delim=',')
    df = ren('suspectname', 'name', df)
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    df = ren('incidentnbr', 'incidentnumber', df)
    df = split_vars('incidentdate', 'date', 'time', ' ', df)
    df = lower_var('suspectshot', df)
    df = lower_var('fatal', df)
    df['outcome'] = ''
    df.loc[(df.fatal=='yes'), 'outcome'] = 'deceased'
    df.loc[((df.fatal=='no') & (df.suspectshot=='yes')), 'outcome'] = 'shot_alive'
    df.loc[((df.fatal=='no') & (df.suspectshot=='no')), 'outcome'] = 'not_shot_alive'  
    print(df.head(5))
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)


def clean_mco_police_ois():
    print("[*] cleaning police ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('mco', 'police'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = replace_char('location', df, '\n', ' ')
    df = split_subjects_nvars([['suspectname', ';'], ['fatal', ','], ['suspecthit', ','], ['suspectrace', ',']], df)
    df = replace_char('suspectname', df, ',', '_')
    df = reverse_names('suspectname', df, delim='_')
    df = ren('suspectname', 'name', df)
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    df = ren('suspectrace', 'race', df)
    df = lower_var('suspecthit', df)
    df = lower_var('fatal', df)
    df = replace_char('fatal', df, 'n', 'no')
    df = replace_char('fatal', df, 'noo', 'no')
    df['outcome'] = ''
    df.loc[(df.fatal=='yes'), 'outcome'] = 'deceased'
    df.loc[((df.fatal=='no') & (df.suspecthit=='yes')), 'outcome'] = 'shot_alive'
    df.loc[((df.fatal=='no') & (df.suspecthit=='no')), 'outcome'] = 'not_shot_alive' 
    print(df.head(5))
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)

clean_mco_police_ois()

#Clean crowdsource data frame
def clean_wp_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('wp', 'crowdsource'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = lower_var('name', df)
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    print(df.head(5))
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
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    print(df.head(5))
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
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    print(df.head(5))
    df.to_csv('{}_cleaned.csv'.format(outfile), index=False)



def clean_ds_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('downloads/{}*{}*.csv'.format('ds', 'crowdsource'))[0].replace('.csv', '')
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = ren('name', 'other_name', df)
    df = ren('victimname', 'name', df)
    df = lower_var('name', df)
    df = rm_mid_initials_suffixes('name', df)
    df = rm_middle_name('name', df)
    df = title_var('city', df)
    df = split_vars('state', 'state_abv', 'state_name', '-', df)
    print(df.head(5))
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)







