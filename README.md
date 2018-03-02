# Police OIS Matching

# Requirements

*	Python 3: wget, datetime, subprocess, pandas, glob, re, os, argparse, zipfile
* R: tidyverse, lubridate, forcats

# Supported Platforms

Currently the code is in active development. Below are the current crowd source platforms and police departments with their status.

# Crowd Source Platforms

* [Washington Post](https://www.washingtonpost.com/graphics/2018/national/police-shootings-2018/) - Working
* [Guardian - The Counted](https://interactive.guim.co.uk/2015/the-counted) - In Development
* [Gun Violence](http://gun-violence.org) - Working

# Police Departments

* [Dallas Police](https://www.dallasopendata.com/Public-Safety/Dallas-Police-Officer-Involved-Shootings/4gmt-jyx2) - Working


# Usage

Using the terminal, git clone and navigate to the repository and run:

```
git clone https://github.com/jmausolf/police_ois_matching
cd police_ois_matching
python run.py -d True -c True -m True
```
Running the above command will download the original data sources, clean those datasets, and make a series of matched reports for each combination of crowd source, police department, and types of officer involved shooting reported by each police department.
