from download import *
from clean import *
import argparse
import re



def provide_merge_sources(pd, cs, script="merge_profile.R"):
	"""
	Provides the police department and crowd source codes to merge.
	## pd: code for the desired police dept, e.g. 'dfw'
	## cs: code for the desired crowd source, e.g. 'wp'
	"""
	code = open(script, 'w')
	code.write('pd <- "{}"\n'.format(pd))
	code.write('cs <- "{}"\n'.format(cs))
	code.close()


def run_tasks(d, c, m):

	if d is True and c is True and m is True:
		download(ois_reports)
		rm_cleaned_files()
		clean_dfw_police_ois()
		clean_wp_crowdsource()
		subprocess.call("Rscript merge.R", shell=True)
	elif d is True and c is True:
		download(ois_reports)
		rm_cleaned_files()
		clean_dfw_police_ois()
		clean_wp_crowdsource()
	elif d is True and m is True:
		download(ois_reports)
		subprocess.call("Rscript merge.R", shell=True)
	elif d is True:
		download(ois_reports)

	elif d is False:
		if c is True and m is True:
			rm_cleaned_files()
			clean_dfw_police_ois()
			clean_wp_crowdsource()
			subprocess.call("Rscript merge.R", shell=True)
		elif c is True and m is False:
			rm_cleaned_files()
			clean_dfw_police_ois()
			clean_wp_crowdsource()
		elif c is False and m is True:
			subprocess.call("Rscript merge.R", shell=True)
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
	
	#provide_merge_sources('dfw', 'gv')
	provide_merge_sources('dfw', 'wp')
	#TODO (eventually)
	#for profile in merge_profiles:
	#	pd = profile[0]
	#	cs = profile[1]
	#	provide_merge_sources(pd, cs)

	print("[*] Running requested tasks...")
	run_tasks(args.download, args.clean, args.merge)
	print("[*] Done.")


