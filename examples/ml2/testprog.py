import sys

def usage (pname):
	sys.stderr.write("Usage:  python testprog.py STRING [STRING...]\n");
	sys.exit(1)

def main (args):

	multi_threaded = 0

	i = 1	# skip arg 0, it's the program name

	while (i < len(args)):
		if (args[i][0] != '-'):
			break;
		elif (args[i] == '-mt'):
			multi_threaded = 1
		else:
			usage(args[0]);
		i = i + 1

	if len(args[i:]) < 1:
		usage(args[0])

	if multi_threaded:
		import ilu
		ilu.ThreadedOperation()

	import strlen
	for arg in args[i:]:
		l = strlen.strlen(arg)
		if l == len(arg):
			print 'The length of "' + arg + '" is ' + str(l)
		else:
			raise 'bad length ' + str(l) + ' returned for arg "' + arg + '"'

	sys.exit(0);

if __name__ == "__main__":
	main (sys.argv)
		
