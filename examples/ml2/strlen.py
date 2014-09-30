import ilu, sys
import strlenGlue, test

theStrlen = ilu.ObjectOfSBH(test.Strlen, strlenGlue.strlen_sbh)

def strlen(s):
	return theStrlen.len(s)
