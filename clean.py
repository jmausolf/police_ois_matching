import pandas as pd
from glob import glob
import re
import unicodedata
import sys
import json


#Some Utility Functions for Cleaning the Data
def loadCSVs():
    police_files = glob('*{}*.csv'.format('police'))
    crowdsource_files = glob('*{}*.csv'.format('crowdsource'))
    return(police_files, crowdsource_files)


def remove_non_ascii_2(text):
    import re
    return re.sub(r'[^\x00-\x7F]+', "", text)

def remove_punct(text):
    import re
    return re.sub(r'[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]', "", text)


def remove_unicode_punct(text):
    #tbl = dict.fromkeys(i for i in xrange(sys.maxunicode)
    #                      if unicodedata.category(unichr(i)).startswith('P'))
    #return text.translate(tbl)
    #return re.sub(u'\p{P}+', "", text)
    #return unicodedata.normalize('NFKD', text).encode('ascii','ignore')
    #return unicodedata.normalize('NFKD', text).decode('utf-8')
    #return text.encode('ascii','ignore')
    #return unicodedata.decomposition(text)
    #return re.sub(u'[\u201c-\u201d]+', "", text)
    return text.replace('\u201c', '')
    #return text

s = "u'David \u201cKrockett\u201d Krieger', u'hospitalized': "
print(remove_unicode_punct(s))

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

def lower_var_rm_nonascii(var, df):
    print("remove_non_ascii_2")
    #s = df[var].str.lower().apply(lambda x: remove_unicode_punct(x))
    s = df[var].str.lower().replace('\u201c', '')
    df = df.drop(var, axis=1).join(s)
    return(df)

def ren(invar, outvar, df):
    df.rename(columns={invar:outvar}, inplace=True)
    return(df)


#Clean police data frame
def clean_dfw_police_ois():
    print("[*] cleaning police ois report...")
    infile = glob('*{}*.csv'.format('police'))[0].replace('.csv', '')
    #infile = 'dfw_police_ois_report_2018-01-28'
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

#Clean crowdsource data frame
def clean_wp_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('*{}*.csv'.format('crowdsource'))[0].replace('.csv', '')
    #infile = 'wp_crowdsource_ois_report_2018-01-28'
    df = pd.read_csv('{}.csv'.format(infile))
    df = clean_cols(df)
    df = lower_var('name', df)
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)


def map_dict_col(var, df):
    """
    ## Maps a col containing dict's to seperate columns
    ## Expected variable cell: '{u'key': u'value', u'key': u'value'}
    """
    s = df[var].map(eval)
    df = pd.concat([df.drop([var], axis=1), s.apply(pd.Series)], axis=1)
    return df

def clean_gv_crowdsource():
    print("[*] cleaning crowdsource ois report...")
    infile = glob('*{}*.tsv'.format('crowdsource'))[0].replace('.tsv', '')
    print(infile)
    df = pd.read_csv('{}.tsv'.format(infile), delimiter='\t', encoding='utf-8')
    df = clean_cols(df)

    df = split_subjects_gv('infoaboutparticipants', df)
    df = map_dict_col('infoaboutparticipants', df)
    df = lower_var('name', df)

    #TODO remove unicode punct
    #df = lower_var_rm_nonascii('name', df)
    df.to_csv('{}_cleaned.csv'.format(infile), index=False)

    print(df)

clean_gv_crowdsource()




