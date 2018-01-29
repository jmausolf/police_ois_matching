from download import *
from clean import *
import argparse


def run_tasks(d, c, m):

	if d is True and c is True and m is True:
		download(ois_reports)
		clean_dfw_police_ois()
		clean_wp_crowdsource()
		subprocess.call("Rscript merge.R", shell=True)
	elif d is True and c is True:
		download(ois_reports)
		clean_dfw_police_ois()
		clean_wp_crowdsource()
	elif d is True and m is True:
		download(ois_reports)
		subprocess.call("Rscript merge.R", shell=True)
	elif d is True:
		download(ois_reports)

	elif d is False:
		if c is True and m is True:
			clean_dfw_police_ois()
			clean_wp_crowdsource()
			subprocess.call("Rscript merge.R", shell=True)
		elif c is True and m is False:
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
	
	print("[*] Running requested tasks...")
	run_tasks(args.download, args.clean, args.merge)
	print("[*] Done.")

