import wget
import datetime
import os
import subprocess

#Get Date for Filenames
now = datetime.datetime.now()
date = now.strftime("%Y-%m-%d")

#Specify File Source (Key) and URL (Value)
crowd_source_ois_reports = {
	"wp" : "https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv",
	"gd" : "https://interactive.guim.co.uk/2015/the-counted/thecounted-data.zip"
}

#Download
for k, v in crowd_source_ois_reports.items():

	tmp = wget.download(v)
	ext = tmp.split(".")[1]
	filename = "{}_crowdsource_ois_report_{}.{}".format(k, date, ext)
	subprocess.call("mv {} {}".format(tmp, filename), shell=True)
	
