# Police OIS Matching

# Requirements

*	Python 3: wget, datetime, subprocess, pandas, glob, re, os, argparse, zipfile
* R: tidyverse, lubridate, forcats, stargazer, ggpubr, gridExtra

---

# Crowd Source Platforms

* [Washington Post](https://www.washingtonpost.com/graphics/2018/national/police-shootings-2018/)
* [Guardian - The Counted](https://interactive.guim.co.uk/2015/the-counted)
* [Gun Violence](http://gun-violence.org)
* [Deadspin](https://deadspin.com/deadspin-police-shooting-database-update-were-still-go-1627414202)

# Police Departments

* [Dallas Police Department](https://www.dallasopendata.com/api/views/4gmt-jyx2/rows.csv?accessType=DOWNLOAD)
* [Denver Police Department](https://www.denvergov.org/media/gis/DataCatalog/denver_police_officer_involved_shootings/csv/denver_police_officer_involved_shootings.csv)
* [Jacksonville Sheriff's Office](http://transparency.jaxsheriff.org/OIS/Export)
* [Orlando Police Department](https://data.cityoforlando.net/api/views/7xrj-vc8d/rows.csv?accessType=DOWNLOAD)
* [Knoxville Police Department](http://knoxvilletn.gov/UserFiles/Servers/Server_109478/File/Police/OpenRecords/OfficerInvolvedShootings2010-2015.xlsx)


# Usage

## Download, Clean, and Merge the Data

Using the terminal, git clone and navigate to the repository and run:

```
git clone https://github.com/jmausolf/police_ois_matching
cd police_ois_matching
python run.py -d True -c True -m True
```

After entering this command, you will be prompted to confirm which profiles you would like to download, clean, and merge. By default, the merge is run on "all" the data. Switching police-ois profiles to `police_ois_reports_verbose` will create discrete merged datasets for specific types of data, such as 'fatal' and 'non-fatal' shootings. Alternatively, these can be filtered from the collective 'all' dataset. 

To continue executing the merge, enter `y` into the terminal when prompted.

```
[*] to confirm, enter [y], to deny enter [n]: y
```

Running the above commands will download the original data sources, clean those datasets, and make a series of matched reports for each combination of crowd source, police department, and type of officer involved shooting reported by each police department.

## Analysis

To reproduce the analysis of this data, open the Rmarkdown document, `tables_figures.Rmd` and `knit` the results. Knitting will run the Rscipt, `analysis.R` and compile the tables_figures.pdf document. 

Alternatively, the following code can be entered into the terminal to compile the report, assuming you are within the local copy of the police_ois_matching repositiory:

`Rscript -e 'library(rmarkdown); rmarkdown::render("tables_figures.Rmd", "pdf_document")'`
