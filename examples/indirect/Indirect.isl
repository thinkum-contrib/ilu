INTERFACE Indirect;

TYPE PInfo = ilu.CString;

TYPE TInfoLayer = ilu.CString;

TYPE TInfo = SEQUENCE OF TInfoLayer;

TYPE ServerID = ilu.CString;

TYPE CInfo = RECORD p: PInfo, t: TInfo END;

TYPE Indirecter = OBJECT METHODS
	register(sid: ServerID,
		pi: PInfo,
		ti: TInfo): CInfo
	  (* Registers a new server; returns cinfo for indirecter for that server. *)
	END;

TYPE CInfos = SEQUENCE OF CInfo;
