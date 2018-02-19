import wget
import datetime
import subprocess
import pandas as pd
from glob import glob
import re
import zipfile
import os

#Get Date for Filenames
now = datetime.datetime.now()
date = now.strftime("%Y-%m-%d")

#Specify File Key (Source) and Value [report type, URL]
#Crowd Source OIS Reports
crowdsource_ois_reports = {
	'wp' : ['crowdsource', 'https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv'],
	'gd' : ['crowdsource', 'https://interactive.guim.co.uk/2015/the-counted/thecounted-data.zip'],
	'gv' : ['crowdsource', 'http://gun-violence.org/portal/download/'],
	'ds' : ['crowdsource', 'https://docs.google.com/spreadsheets/d/1cEGQ3eAFKpFBVq1k2mZIy5mBPxC6nBTJHzuSWtZQSVw/export?format=csv&id=1cEGQ3eAFKpFBVq1k2mZIy5mBPxC6nBTJHzuSWtZQSVw&gid=1144428085']
}

#Police OIS Reports
police_ois_reports = {
	'dfw' : ['police', 'https://www.dallasopendata.com/api/views/4gmt-jyx2/rows.csv?accessType=DOWNLOAD', ['deceased', 'injured', 'other', 'shootmiss', 'all']],
	#'oha' : ['police', 'url', ['deceased', 'injured', 'all']]
}

#All Reports
ois_reports = [crowdsource_ois_reports, police_ois_reports]

def add_list(*args):
	output = []
	for item in args:
		output.append(item)
	#print(output)
	return output

def unlist(listoflist):
	return [x for y in listoflist for x in y]


def make_report_profiles(police_ois_reports, crowdsource_ois_reports):
	profiles = []

	pk = [[k, v[2]] for k, v in police_ois_reports.items()]
	ck = [k for k, v in crowdsource_ois_reports.items()]
	merge_profiles = [[p, c] for p in pk for c in ck]

	for mp in merge_profiles:
		pd = mp[0][0]
		ois_types = mp[0][1]
		cs = mp[1]
		pf = [[pd, ois, cs] for ois in ois_types]

		for report in pf:
			profiles.append(report)

	return profiles


#Download and Rename Files
def wget_download_rename(key, value):
	report_type = value[0]
	tmp = wget.download(value[1])
	ext = tmp.rsplit(".", 1)[1]
	filename = "{}_{}_ois_report_{}.{}".format(key, report_type, date, ext)
	subprocess.call("mv {} {}".format(tmp, filename), shell=True)


def unzip(zipfilename, subfilename="", rename=""):
	with zipfile.ZipFile(zipfilename,"r") as zip_ref:
	    zip_ref.extract(subfilename, ".")
	    if rename !="":
	    	os.rename(subfilename, rename)


def unzip_rename(globstem, ext, req_subfiles):
	zip_files = glob('{}*.{}'.format(globstem, ext))	
	name_stem = [n.split(".")[0] for n in zip_files]
	sub = req_subfiles
	[unzip(z, s, '{}_{}'.format(n, s.lower())) for z in zip_files for n in name_stem for s in sub ]


def download(ois_reports):
	print("[*] downloading files...")
	[wget_download_rename(k, v) for d in ois_reports for k, v in d.items()]
	print("[*] unzipping downloaded files...")
	unzip_rename('gv_crowdsource_ois_report', 'wget', ['Events.tsv'])
	#unzip_rename('gd_crowdsource_ois_report', 'zip', ['the-counted-2015.csv', 'the-counted-2016.csv'])
	subprocess.call("bash collect_files.sh", shell=True)

