/*
 BeginILUCopyright
 
 Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
 
 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.
 
 EndILUCopyright
*/
/* IluORB.java */
/* Chris Jacobi, January 6, 1999 4:49 pm PST */
/* $Id: IluORB.java,v 1.7 1999/08/03 01:54:01 janssen Exp $ */

 
package xerox.ilu;

/**
 * Makes Ilu look like it were a CORBA ORB<p>
 * The purpose of this module is to be CORBA compatible; it will change
 * whenever necessary.
 */
public final class IluORB extends org.omg.CORBA.ORB {
    
    /*friendly*/ java.util.Properties orbProps = null;
    /*friendly*/ java.lang.String[] orbArgs = null;
    /*friendly*/ java.applet.Applet orbApp = null;
    /*friendly*/ xerox.ilu.IluServer orbDefaultServer = null;
   
   
    public IluORB() {
        xerox.ilu.Ilu.init();
    } //constructor
        
    
    static private IluORB iluTheOrb = null;
    /*package*/ static IluORB iluOrb(org.omg.CORBA.Object obj) {
        if (iluTheOrb == null) {
            iluTheOrb = new IluORB();
        }
        return iluTheOrb;
    } //iluOrb
    
    
    protected void set_parameters(
       java.lang.String[] args, 
       java.util.Properties props)
    {
        this.orbProps = props;
        this.orbArgs = args;
    } //set_parameters
    
    
    protected void set_parameters(
        java.applet.Applet app, 
        java.util.Properties props)
    {
        this.orbProps = props;
        this.orbApp = app;
    } //set_parameters
    
    
    public void connect(org.omg.CORBA.Object obj) {
        if (xerox.ilu.IluRT0.known(obj)) {
            //connecting an already connected object should have
            //no effect.
            return;
        }
        int defaultLifetime = xerox.ilu.IluLifetimeArgs.iluLifetimeRemember;
            //utterly bogus lifetime but this is used like standard corba...
        if (this.orbDefaultServer == null) {
            this.orbDefaultServer = xerox.ilu.IluServer.createServer(null);
        }
        xerox.ilu.Ilu.registerTrueObject(
            null, //ih 
            obj, 
            this.orbDefaultServer, 
            null, //iluClass  
            defaultLifetime
            );
    } //connect


    public void disconnect(org.omg.CORBA.Object obj) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public java.lang.String[] list_initial_services() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    

    /*friendly*/ static java.lang.String fromArgList(
        java.lang.String[] args, java.lang.String key)
    {
        if (args != null) {
            java.lang.String match = "-" + key;
            int cPos = match.length(); 
            for (int i = 0; i < args.length; i++) {
                java.lang.String arg = args[i];
                if ((arg != null) && (arg.startsWith(match))) {
                    if (arg.length() == cPos) {
                        //exact match; return next argument
                        if ((i+1)<args.length) {
                            return args[i+1];
                        }
                    } else if (arg.length() > (cPos+1)) {
                        //match with cont sign; return rest of argument
                        if (arg.charAt(cPos)=='#') {
                            return arg.substring(cPos+1);
                        }
                    }
                } 
            }
        }
        return null;
    } //fromArgList
    
    
    /*friendly*/ static java.lang.String fromAppEnv(
        java.applet.Applet app, java.lang.String key)
    {
        java.lang.String s = null;
        if (app != null) {
            s = app.getParameter(key);
        }
        return s;
    } //fromAppEnv
    

    /**  
     * Non corba-compatible utility procedure
     * to read an sbh from a file <p>.
     * Assumes sbh either to be first line of the file, or,
     * second line if the first line is recognized as redirect
     * from the output of Sun's Java IDL name server. 
     */
    public java.lang.String readSBHFromFile(java.lang.String filename) {
        try {
            java.io.FileReader fr = new java.io.FileReader(filename);
            java.io.BufferedReader br = new java.io.BufferedReader(fr);
            java.lang.String line = br.readLine();
            if (line.startsWith("Initial Naming Context:")) {
                line = br.readLine();
            }
            br.close();
            return line;
        } catch (java.io.IOException e) {
            throw new org.omg.CORBA.DATA_CONVERSION(
                "From " + e
                );
        }
    } //readSBHFromFile
    
    /**  
     * Non corba-compatible utility procedure
     * to read an sbh from an URL <p>.
     * Assumes sbh either to be first line of the file, or,
     * second line if the first line is recognized as redirect
     * from the output of Sun's Java IDL name server. 
     */
    public java.lang.String readSBHFromURL(java.lang.String filename) {
        try {
            java.net.URL url = new java.net.URL(filename);
            java.io.InputStream s = url.openStream();
            java.io.InputStreamReader r = new java.io.InputStreamReader(s);
            java.io.BufferedReader br = new java.io.BufferedReader(r);
            java.lang.String line = br.readLine();
            if (line.startsWith("Initial Naming Context:")) { 
                line = br.readLine();
            }
            br.close();
            return line;
        } catch (java.lang.Exception e) {
            throw new org.omg.CORBA.DATA_CONVERSION(
                "From " + e
                );
        }
    } //readSBHFromURL
    
    
    /**
     * Internal, non-corba compatible utility to find an initial
     * sbh to start your application; semantics is first step of
     * resolve_initial_references.
     *
     * Argument is the name of an initial service.  (Well it is 
     * a property key). This key will be used for a property look up 
     * and the found string is returned to be used as an sbh.
     *
     * If the found "sbh" starts with "file:" an additional
     * look up using the file system is performed.  This is usefull
     * to deal with foreign name services which can write the "root" 
     * name service object into a file. 
     *
     * @param serviceKey The name of an initial service
     * @return The "sbh" which could be used for initial service.
     */
    public java.lang.String     
        resolve_initial_sbh(java.lang.String serviceKey)
    {
        java.lang.String sbh = fromArgList(this.orbArgs, serviceKey);
        if (sbh == null) {
            sbh = fromAppEnv(this.orbApp, serviceKey);
        }
        if (sbh == null) {
            java.util.Properties props = this.orbProps;
            if (props != null) {
                sbh = props.getProperty(serviceKey);
            }
        }
        if (sbh == null) {
            sbh = xerox.ilu.IluEnvironment.getStringPropX(serviceKey);
        }
        if (sbh != null) {
            if (sbh.regionMatches(true, 0, "url:", 0, 4)) {
                java.lang.String fn = sbh.substring(4).trim();
                sbh = readSBHFromURL(fn);
            } else if (sbh.regionMatches(true, 0, "file:", 0, 5)) {
                java.lang.String fn = sbh.substring(5).trim();
                sbh = readSBHFromFile(fn);
            }
        }
        if (sbh == null) {
            throw new org.omg.CORBA.INITIALIZE(
                "can't resolve Ilu initial references"
                ); 
        }
        return sbh;
    } //resolve_initial_sbh
    
    
    public org.omg.CORBA.Object resolve_initial_references(
            java.lang.String name /*serviceKey*/) 
            throws org.omg.CORBA.ORBPackage.InvalidName
    {
        java.lang.String sbh = resolve_initial_sbh(name);
        org.omg.CORBA.Object ob = string_to_object(sbh);
        return ob;;
    } //resolve_initial_references
    
    
    public java.lang.String object_to_string(org.omg.CORBA.Object obj) {
        java.lang.String s = xerox.ilu.Ilu.iorOfObject(obj);
        return s;
    } //object_to_string
    
    
    public org.omg.CORBA.Object string_to_object(java.lang.String s)     {
        return (org.omg.CORBA.Object) 
            xerox.ilu.Ilu.objectFromSBH(
                s, 
                xerox.ilu.CORBA_ObjectStub.iluClass()
                );
    } //string_to_object
    
    
    public org.omg.CORBA.NVList create_list(int count) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    /* has been changed since jdk1.2-rc2
     * public org.omg.CORBA.NVList 
     *         create_operation_list(org.omg.CORBA.OperationDef oper) {
     *     throw new org.omg.CORBA.NO_IMPLEMENT();
     * }
     */ 
    
    public org.omg.CORBA.NVList 
            create_operation_list(org.omg.CORBA.Object oper) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public org.omg.CORBA.NamedValue create_named_value(
        String s, org.omg.CORBA.Any any, int flags)
    {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.ExceptionList create_exception_list() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.ContextList create_context_list() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.Context get_default_context() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.Environment create_environment() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }


    public org.omg.CORBA.portable.OutputStream create_output_stream() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }


    public void send_multiple_requests_oneway(org.omg.CORBA.Request[] req) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public void send_multiple_requests_deferred(org.omg.CORBA.Request[] req) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public boolean poll_next_response() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.Request get_next_response() 
            throws org.omg.CORBA.WrongTransaction {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode get_primitive_tc(
        org.omg.CORBA.TCKind tcKind)
    {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_struct_tc(
            String id, 
            String name,
            org.omg.CORBA.StructMember[] members) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_union_tc(
            String id, 
            String name,
            org.omg.CORBA.TypeCode discriminator_type,
            org.omg.CORBA.UnionMember[] members) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_enum_tc(
            String id, String name, String[] members) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_alias_tc(
            String id, 
            String name,
            org.omg.CORBA.TypeCode original_type) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_exception_tc(String id, String name,
            org.omg.CORBA.StructMember[] members) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_interface_tc(String id, String name) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_string_tc(int bound) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_wstring_tc(int bound) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_sequence_tc(
            int bound, org.omg.CORBA.TypeCode element_type) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }


    public org.omg.CORBA.TypeCode create_recursive_sequence_tc(
            int bound, int offset) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.TypeCode create_array_tc(
            int length, org.omg.CORBA.TypeCode element_type) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    public org.omg.CORBA.Any create_any() {
        xerox.ilu.IluAny any = new xerox.ilu.IluAny();
        return any;
    } //create_any


} // IluORB

