(** $Id: TestServer.m3,v 1.6 1999/08/03 01:52:12 janssen Exp $
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
*)
(* Last edited by Mike Spreitzer October 9, 1998 11:25 am PDT *)

MODULE TestServer EXPORTS Main;
IMPORT Fmt, Ilu, IluBasics, IluSimpleBind, Stdio, Test1, Test2, Test3,
       Thread, Time, Wr;

EXCEPTION CantHappen;

<*FATAL Wr.Failure, IluBasics.Failed, Thread.Alerted, CantHappen*>

PROCEDURE Say (t: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout, t & "\n");
    Wr.Flush(Stdio.stdout);
    RETURN
  END Say;

TYPE
  TestObjTab = Ilu.ObjectTable OBJECT
               OVERRIDES
                 ObjectToHandle := ObjectToHandle;
                 HandleToObject := HandleToObject
               END;

PROCEDURE ObjectToHandle (<*UNUSED*> self: Ilu.ObjectTable;
                                     o   : Ilu.Object       ):
  Ilu.ObjectHandle =
  BEGIN
    IF o = uc THEN
      RETURN "Test1_Initial_Object"
    ELSE
      RETURN Fmt.Ref(o)
    END;
  END ObjectToHandle;

PROCEDURE HandleToObject (<*UNUSED*> self: Ilu.ObjectTable;
                          <*UNUSED*> h   : Ilu.ObjectHandle ):
  Ilu.Object =
  BEGIN
    RETURN NIL;
  END HandleToObject;

VAR
  theServer := Ilu.InitTrueServer(
                 NEW(Ilu.TrueServer), "Test1-Server", NEW(TestObjTab));

TYPE
  T1O1Svr = Test1.O1 OBJECT
            OVERRIDES
              ILU_Get_Server := Get_Server;
              ILU_Qua_Type   := T1O1Svr_Qua_Type;
              U_CSS_to_U     := Test1_O1_U_CSS_to_U;
              f_CSS_to_RO    := Test1_O1_f_CSS_to_RO;
              R_ScS_to_F     := Test1_O1_R_ScS_to_F;
              a_RO           := Test1_O1_a_RO;
              get_O2         := Test1_O1_get_O2;
              get_O3         := Test1_O1_get_O3
            END;

PROCEDURE Get_Server (<*UNUSED*> self: Ilu.Object): Ilu.Server =
  BEGIN
    RETURN theServer;
  END Get_Server;

PROCEDURE T1O1Svr_Qua_Type (self: T1O1Svr; ot: Ilu.ObjectType):
  Ilu.Object =
  BEGIN
    IF ot = Test1.ILU_Get_Type_O1(self) THEN
      RETURN self
    ELSE
      RETURN NIL
    END;
  END T1O1Svr_Qua_Type;

PROCEDURE Test1_O1_U_CSS_to_U (<*UNUSED*> self: T1O1Svr;
                                          u   : Test1.U;
                               <*UNUSED*> css : Test1.CSS ): Test1.U
  RAISES {IluBasics.Failed, Thread.Alerted, Test1.E1, Test1.E2} =
  BEGIN
    Say("Test1.O1.U-CSS-to-U");
    RETURN u;
  END Test1_O1_U_CSS_to_U;

PROCEDURE Test1_O1_f_CSS_to_RO (<*UNUSED*> self: T1O1Svr;
                                <*UNUSED*> css : Test1.CSS ): Test1.RO
  RAISES {IluBasics.Failed, Thread.Alerted, Test1.E1} =
  VAR ans := NEW(Test1.RO);
  BEGIN
    ans.i := 9;
    ans.css := NEW(Test1.CSS, 0);
    ans.a := Test1.A1{"hi", "hi", "hi"};
    Say("Test1.O1.f-CSS-to-R0");
    RETURN ans;
  END Test1_O1_f_CSS_to_RO;

PROCEDURE Test1_O1_R_ScS_to_F (<*UNUSED*> self: T1O1Svr;
                               <*UNUSED*> r   : Test1.R;
                               <*UNUSED*> s   : TEXT     ):
  Ilu.ShortReal RAISES {IluBasics.Failed, Thread.Alerted} =
  BEGIN
    Say("Test1.O1.R-ScS-to-F");
    RETURN 39.7;
  END Test1_O1_R_ScS_to_F;

PROCEDURE Test1_O1_a_RO (<*UNUSED*> self: T1O1Svr;
                         <*UNUSED*> ro  : Test1.RO ) RAISES {} =
  BEGIN
    Say("Test1.O1.a_RO");
    RETURN;
  END Test1_O1_a_RO;

PROCEDURE Test1_O1_get_O2 (<*UNUSED*> self: T1O1Svr): Test1.O2
  RAISES {IluBasics.Failed, Thread.Alerted, Test1.CantCreate} =
  BEGIN
    IF theO2 = NIL THEN theO2 := NEW(T1O2Svr) END;
    RETURN theO2;
  END Test1_O1_get_O2;

PROCEDURE Test1_O1_get_O3 (<*UNUSED*> self: T1O1Svr; subclass: BOOLEAN):
  Test1.O3 RAISES {IluBasics.Failed, Thread.Alerted, Test1.CantCreate} =
  BEGIN
    Say("Test1.O1.get-O3");
    IF subclass THEN
      WITH l = NEW(T3OSvrLead) DO
        l.follower := NEW(T3OSvrFolw, leader := l);
        RETURN l
      END
    ELSIF flop THEN
      flop := NOT flop;
      RETURN NEW(T1O4Svr)
    ELSE
      flop := NOT flop;
      RETURN NEW(T1O3Svr)
    END;
  END Test1_O1_get_O3;

VAR flop := FALSE;
VAR theO2: T1O2Svr := NIL;

TYPE
  T1O2Svr = Test1.O2 OBJECT
            OVERRIDES
              ILU_Get_Server := Get_Server;
              ILU_Qua_Type   := T1O2Svr_Qua_Type;
              OO_A0_to_CSS   := Test1_O2_OO_A0_to_CSS;
              R_I_A1_to_I_A0 := Test1_O2_R_I_A1_to_I_A0
            END;

PROCEDURE T1O2Svr_Qua_Type (self: T1O2Svr; ot: Ilu.ObjectType):
  Ilu.Object =
  BEGIN
    IF ot = Test1.ILU_Get_Type_O2(self) THEN
      RETURN self
    ELSE
      RETURN NIL
    END;
  END T1O2Svr_Qua_Type;

PROCEDURE Test1_O2_OO_A0_to_CSS (<*UNUSED*> self: T1O2Svr;
                                            o   : Test1.OO;
                                 <*UNUSED*> a   : Test1.A0  ): Test1.CSS
  RAISES {IluBasics.Failed, Thread.Alerted, Test1.E2} =
  BEGIN
    Say("Test1.o2.OO-A0-to-CSS");
    IF o = NIL THEN
      RAISE Test1.E2(7)
    ELSE
      RETURN NEW(Test1.CSS, 0);
    END;
  END Test1_O2_OO_A0_to_CSS;

PROCEDURE Test1_O2_R_I_A1_to_I_A0 (<*UNUSED*>     self: T1O2Svr;
                                   <*UNUSED*>     r   : Test1.R;
                                   <*UNUSED*> VAR i   : Test1.I;
                                   <*UNUSED*>     a   : Test1.A1 ):
  Test1.A0 RAISES {IluBasics.Failed, Thread.Alerted} =
  VAR ans := Test1.A0{0, ..};
  BEGIN
    Say("Test1.o2.R-I-A1-to-I-A0");
    RETURN ans;
  END Test1_O2_R_I_A1_to_I_A0;

TYPE
  T1O3Svr = Test1.O3 OBJECT
            OVERRIDES
              ILU_Get_Server := Get_Server;
              ILU_Qua_Type   := T1O3Svr_Qua_Type;
              RS_R_to_R_IS   := Test1_O3_RS_R_to_R_IS;
              O1_U_to_U      := Test1_O3_O1_U_to_U;
              BS_to_I        := Test1_O3_BS_to_I
            END;

PROCEDURE T1O3Svr_Qua_Type (self: T1O3Svr; ot: Ilu.ObjectType):
  Ilu.Object =
  BEGIN
    IF ot = Test1.ILU_Get_Type_O3(self) THEN
      RETURN self
    ELSE
      RETURN NIL
    END;
  END T1O3Svr_Qua_Type;

PROCEDURE Test1_O3_RS_R_to_R_IS (<*UNUSED*>     self: Test1.O3;
                                 <*UNUSED*>     r   : Test1.RS;
                                            VAR r2  : Test1.R   ):
  Test1.IS RAISES {IluBasics.Failed, Thread.Alerted} =
  BEGIN
    Say("Test1.O3.RS-R-to-R-IS");
    r2.i := 3;
    r2.css := NEW(Test1.CSS, 0);
    r2.a := Test1.A1{"just", "a", "string"};
    RETURN NEW(Test1.IS, 0);
  END Test1_O3_RS_R_to_R_IS;

PROCEDURE Test1_O3_O1_U_to_U (<*UNUSED*>     self: Test1.O3;
                                             o   : Test1.O1;
                                         VAR u   : Test1.U   )
  RAISES {IluBasics.Failed, Thread.Alerted, Test1.E2} =
  BEGIN
    Say("Test1.O3.O1-U-to-U");
    u := NEW(Test1.U_O1, d:= Test1.U_O1__Tag, v := o);
  END Test1_O3_O1_U_to_U;

PROCEDURE Test1_O3_BS_to_I (<*UNUSED*> self: Test1.O3; b: Test1.BS):
  Test1.I RAISES {IluBasics.Failed, Thread.Alerted} =
  BEGIN
    Say("Test1.O3.BS-to-I");
    RETURN NUMBER(b^) * NUMBER(b^);
  END Test1_O3_BS_to_I;

TYPE
  T1O4Svr = Test1.O4 OBJECT
            OVERRIDES
              ILU_Get_Server := Get_Server;
              ILU_Qua_Type   := T1O4Svr_Qua_Type;
              RS_R_to_R_IS   := Test1_O4_RS_R_to_R_IS;
              O1_U_to_U      := Test1_O3_O1_U_to_U;
              BS_to_I        := Test1_O3_BS_to_I;
              R_to_R         := Test1_O4_R_to_R
            END;

PROCEDURE T1O4Svr_Qua_Type (self: T1O4Svr; ot: Ilu.ObjectType):
  Ilu.Object =
  BEGIN
    IF ot = Test1.ILU_Get_Type_O3(self)
         OR ot = Test1.ILU_Get_Type_O4(self) THEN
      RETURN self
    ELSE
      RETURN NIL
    END;
  END T1O4Svr_Qua_Type;

PROCEDURE Test1_O4_RS_R_to_R_IS (<*UNUSED*>     self: Test1.O3;
                                 <*UNUSED*>     r   : Test1.RS;
                                            VAR r2  : Test1.R   ):
  Test1.IS RAISES {IluBasics.Failed, Thread.Alerted} =
  BEGIN
    Say("Test1.O4.RS-R-to-R-IS");
    r2.i := 25719;
    r2.css := NEW(Test1.CSS, 0);
    r2.a := Test1.A1{"from", "p", "string"};
    RETURN NEW(Test1.IS, 0);
  END Test1_O4_RS_R_to_R_IS;

PROCEDURE Test1_O4_R_to_R (<*UNUSED*> self: T1O4Svr; r: Ilu.Real):
  Ilu.Real RAISES {IluBasics.Failed, Thread.Alerted} =
  VAR r2: Ilu.Real := 1020304.05060708D0;
  BEGIN
    Say(
      Fmt.F(
        "Test1.O4.R-to-R(%s) => %s\n",
        Fmt.LongReal(r, 10, Fmt.Style.Flo),
        Fmt.LongReal(r2, 10, Fmt.Style.Flo)));
    RETURN r2;
  END Test1_O4_R_to_R;

TYPE
  T3OSvrLead = Test3.O OBJECT
                 follower: T3OSvrFolw;
               OVERRIDES
                 ILU_Get_Server := Get_Server;
                 ILU_Qua_Type   := T3OSvrLead_Qua_Type;
                 RS_R_to_R_IS   := Test3_O_RS_R_to_R_IS;
                 O1_U_to_U      := Test1_O3_O1_U_to_U;
                 BS_to_I        := Test1_O3_BS_to_I;
                 I_to_Test1U    := Test3_O_I_to_Test1U
               END;

TYPE
  T3OSvrFolw = Test2.P OBJECT
                 leader: T3OSvrLead;
               OVERRIDES
                 ILU_Get_Server := Get_Server;
                 ILU_Qua_Type   := T3OSvrFolw_Qua_Type;
                 SR_to_I        := Test3_O_SR_to_I;
               END;

PROCEDURE T3OSvrLead_Qua_Type (self: T3OSvrLead; ot: Ilu.ObjectType):
  Ilu.Object =
  BEGIN
    IF ot = Test1.ILU_Get_Type_O3(self)
         OR ot = Test3.ILU_Get_Type_O(self) THEN
      RETURN self
    ELSIF ot = Test2.ILU_Get_Type_P(self.follower) THEN
      RETURN self.follower;
    ELSE
      RETURN NIL
    END;
  END T3OSvrLead_Qua_Type;

PROCEDURE T3OSvrFolw_Qua_Type (self: T3OSvrFolw; ot: Ilu.ObjectType):
  Ilu.Object =
  BEGIN
    RETURN self.leader.ILU_Qua_Type(ot)
  END T3OSvrFolw_Qua_Type;

PROCEDURE Test3_O_RS_R_to_R_IS (<*UNUSED*>     self: Test1.O3;
                                <*UNUSED*>     r   : Test1.RS;
                                           VAR r2  : Test1.R   ):
  Test1.IS RAISES {IluBasics.Failed, Thread.Alerted} =
  BEGIN
    Say("Test1.O3.RS-R-to-R-IS");
    r2.i := 3;
    r2.css := NEW(Test1.CSS, 0);
    r2.a := Test1.A1{"just", "a", "string"};
    RETURN NEW(Test1.IS, 0);
  END Test3_O_RS_R_to_R_IS;

PROCEDURE Test3_O_SR_to_I (<*UNUSED*> self: T3OSvrFolw; i: Ilu.ShortReal):
  INTEGER RAISES {IluBasics.Failed, Thread.Alerted} =
  BEGIN
    Say(Fmt.F("Test3.O.SR-to-I(%s)\n", Fmt.Real(i, 10, Fmt.Style.Flo)));
    RETURN ROUND(i);
  END Test3_O_SR_to_I;

PROCEDURE Test3_O_I_to_Test1U (<*UNUSED*> self: T3OSvrLead; i: INTEGER):
  Test1.U
  RAISES {IluBasics.Failed, Thread.Alerted, Test3.E1, Test1.E1} =
  BEGIN
    Say(Fmt.F("Test3.O.I-to-Test1U(%s)\n", Fmt.Int(i)));
    RETURN NEW(Test1.U_BOOLEAN, d:= Test1.U_BOOLEAN__Tag, v := TRUE);
  END Test3_O_I_to_Test1U;

VAR uc := NEW(T1O1Svr);

PROCEDURE Start () =
  VAR sbh, mstid: TEXT;
  VAR o2: Ilu.Object;
  BEGIN
    Ilu.Export_Server(theServer, NEW(Ilu.SunRpc2), NEW(Ilu.TCP));
    sbh := Ilu.SbhFromObject(uc);
    mstid := Ilu.IdOfObjectType(uc.ILU_Get_Type());
    Say("Created and exported '" & sbh & "' '" & mstid & "'");
    TRY
      EVAL IluSimpleBind.Publish(uc);
      Say("Published it too.");
      TRY
        o2 :=
          IluSimpleBind.Lookup(
            "Test1_Initial_Object@Test1-Server",
            Test1.ILU_Get_Type_O1(NIL));
        IF uc = o2 THEN
          Say("Lookup returned same object.")
        ELSE
          Say("Lookup returned different object!")
        END (*if*);
      EXCEPT
        IluBasics.Failed (f) => Say("Lookup failed (" & f.info & ").")
      END (*try-except*);
    EXCEPT
      IluBasics.Failed (f) => Say("Publish failed (" & f.info & ").")
    END (*try-except*);
    Wr.Flush(Stdio.stdout);
  END Start;

BEGIN
  Start();
  LOOP
    (* Linebreak, if you PLEASE! *)
    Time.LongPause(10);
  END (*loop*);
END TestServer.
