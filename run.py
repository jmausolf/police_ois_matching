from download import *
from clean import *
import argparse
import re



def provide_merge_sources(profile, script="merge_profile.R"):
	"""
	Provides the police department and crowd source codes to merge.
	## pd: code for the desired police dept, e.g. 'dfw'
	## cs: code for the desired crowd source, e.g. 'wp'
	"""
	code = open(script, 'w')
	code.write('pd <- "{}"\n'.format(profile[0]))
	code.write('ois_type <- "{}"\n'.format(profile[1]))
	code.write('cs <- "{}"\n'.format(profile[2]))
	code.close()


def download_files():
	subprocess.call("rm downloads/* 2>/dev/null", shell=True)
	download(ois_reports)

def clean_files():
	rm_cleaned_files()
	clean_dfw_police_ois()
	clean_den_police_ois()
	clean_jax_police_ois()
	clean_wp_crowdsource()
	#clean_gv_crowdsource()
	clean_gd_crowdsource()
	clean_ds_crowdsource()

def merge_report(profile):
	provide_merge_sources(profile)
	subprocess.call("Rscript merge.R", shell=True)


def run_tasks(d, c, m, profiles):

	if d is True and c is True and m is True:
		download_files()
		clean_files()
		[merge_report(p) for p in profiles]
	elif d is True and c is True:
		download_files()
		clean_files()
	elif d is True and m is True:
		download_files()
		[merge_report(p) for p in profiles]
	elif d is True:
		download_files()

	elif d is False:
		if c is True and m is True:
			clean_files()
			[merge_report(p) for p in profiles]
		elif c is True and m is False:
			clean_files()
		elif c is False and m is True:
			[merge_report(p) for p in profiles]
		else:
			pass 



if __name__=='__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument("-d", "--download", default=False, type=bool, help="download files")
	parser.add_argument("-c", "--clean", default=False, type=bool, help="clean files")
	parser.add_argument("-m", "--merge", default=False, type=bool, help="merge files")
	args = parser.parse_args()

	if not (args.download or args.clean or args.merge):
	    parser.error('No action requested, add --download True or --clean True or --merge True')

	#Load Profiles and Run
	profiles = make_report_profiles(police_ois_reports, crowdsource_ois_reports)
	print("[*] Running requested tasks...")
	run_tasks(args.download, args.clean, args.merge, profiles)
	print("[*] Done.")


