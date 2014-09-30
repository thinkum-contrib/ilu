from ILUTesting import *

tests = [SimpleSingleProgramTest("multlang C/Python program", "multlang")]
if iludefs_dict.has_key("ILU_PYTHON_HAS_THREADS"):
	tests.append(SimpleSingleProgramTest("multlang C/Python program, threaded", "multlang -mt"))

if __name__ == "__main__":
	run_tests(tests)
