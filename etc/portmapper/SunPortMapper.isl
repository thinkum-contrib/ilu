(* SunPortMapper.isl *)
(* Copyright 1993, 1996, 1997 by Xerox Corporation.  All rights reserved. *)

INTERFACE SunPortMapper;

CONSTANT TCP-Protocol : CARDINAL = 6;
CONSTANT UDP-Protocol : CARDINAL = 17;

TYPE Mapping = RECORD
  program-number: CARDINAL,
  version-number: CARDINAL,
  protocol-type: CARDINAL,	(* either TCP-Protocol or UDP-Protocol *)
  unix-port: CARDINAL
END;

TYPE MapList = OPTIONAL MapCons;
TYPE MapCons = RECORD map: Mapping, next: MapList END;

TYPE OPAQUE = SEQUENCE OF BYTE;

TYPE Call-Result = RECORD unix-port: CARDINAL, res: OPAQUE END;

TYPE T = OBJECT SINGLETON "sunrpc_2_100000_2" (* tcp_0_111 *)
  METHODS
    Null () = 0,
    Set (m: Mapping): BOOLEAN = 1,
    Unset (m: Mapping): BOOLEAN = 2,
    GetPort (m: Mapping): CARDINAL = 3,
    Dump (): MapList = 4,
    CallIt (prog: CARDINAL, vers: CARDINAL, proc: CARDINAL, args: OPAQUE): Call-Result = 5
  END;
