class testprog {

  private static void usage () {
    System.err.println("Usage:  java testprog STRING [STRING...]");
    System.exit(1);
  }

  public static void main (String args[]) {
    int len;

    if (args.length < 1)
      usage();
    for (int i = 0;  i < args.length;  i++) {
      try {
	len = (int) strlen.len(args[i]);
	if (len != args[i].length()) {
	  System.err.println("Incorrect length " + len + " returned for argument \"" + args[i] + "\"");
	  System.exit(1);
	} else {
	  System.out.println("length of \"" + args[i] + "\" is " + len);
	}
      }
      catch (java.lang.Exception e) {
	System.err.println("strlen raises " + e);
	System.exit(1);
      }
    }
    System.exit(0);
  }
}
