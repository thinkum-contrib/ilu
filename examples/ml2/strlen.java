// strlen.java
// link to C++ strlen `library'
// $Id: strlen.java,v 1.2 1999/06/02 21:54:38 janssen Exp $

class strlen {
    
  private static test.Strlen theStrlen;

  native static String initImpl(String sid); 

  static {
    System.out.println("loading ILU runtime for Java...");
    xerox.ilu.IluDebug.init();
    System.out.println("loading strlen library...");
    System.loadLibrary("strlen"); 
    String sbh = initImpl(null);
    try {
      theStrlen = (test.Strlen) xerox.ilu.Ilu.objectFromSBH(sbh, test.StrlenStub.iluClass());
      System.out.println("successfully accessed Strlen object " + theStrlen + "...");
    }
    catch (org.omg.CORBA.SystemException e) {
      System.err.println("ILU Exception <" + e + "> raised from objectFromSBH(\"" + sbh + "\".");
    }
  }

  public static long len (String str) {
    return theStrlen.len(str);
  }

  strlen() {
  };
}


