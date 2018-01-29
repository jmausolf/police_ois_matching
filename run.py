from download import *
from clean import *
import argparse

#subprocess.call("Rscript merge.R", shell=True)

def run_tasks(d, c, m):
	if d is True and c is True and m is True:
		print("All true")
		#run all
		download(ois_reports)
		clean_dfw_police_ois()
		clean_wp_crowdsource()
		subprocess.call("Rscript merge.R", shell=True)
	elif d is True and c is True:
		print("download and clean")
	elif d is True and m is True:
		print("download and merge, no clean")
	elif d is True:
		print("download only")

	elif d is False:
		print("download is false")
		if c is True and m is True:
			print("clean and merge only")
			clean_dfw_police_ois()
			clean_wp_crowdsource()
			subprocess.call("Rscript merge.R", shell=True)
		elif c is True and m is False:
			print("clean only")
		elif c is False and m is True:
			print("merge only")
		else:
			print("pass")
			pass 



if __name__=='__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument("-d", "--download", default=False, type=bool, help="download files")
	parser.add_argument("-c", "--clean", default=False, type=bool, help="clean files")
	parser.add_argument("-m", "--merge", default=False, type=bool, help="merge files")	
	args = parser.parse_args()

	
	#for arg in vars(args):
	#     print(arg, getattr(args, arg))

	
	#print("[*] Calculating the result...")
	run_tasks(args.download, args.clean, args.merge)
	#lower_return_refs(args.textfile, args.replace)
