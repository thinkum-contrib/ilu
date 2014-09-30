interface test-server;

type Bytes = sequence of byte;

type server = object
  methods

    get-output () : ilu.CString,	(* get current contents of log file *)
    kill (),				(* kill server process *)
    close ()				(* delete server object *)

  end;

type tcpdump-server = object supertypes server end
  methods

    get-tcpdump () : Bytes		(* get tcpdump data *)

  end;

type factory = object
  methods

    start-server (stamp : ilu.CString, command : ilu.CString) : server,
    finish ()

  end;
