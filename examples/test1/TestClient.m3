(** $Id: TestClient.m3,v 1.6 1999/08/03 01:52:13 janssen Exp $
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
(* Last edited by Mike Spreitzer October 9, 1998 11:24 am PDT *)

MODULE TestClient EXPORTS Main;
IMPORT Fmt, Ilu, IluBasics, IluSimpleBind, Stdio, Test1, Test3, Thread,
       Wr;

EXCEPTION CantHappen;

<*FATAL Wr.Failure, IluBasics.Failed, Thread.Alerted, CantHappen*>
<*FATAL Test1.E1, Test1.E2, Test1.CantCreate, Test3.E1*>

PROCEDURE Say (t: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout, t & "\n");
    Wr.Flush(Stdio.stdout);
    RETURN
  END Say;

PROCEDURE FmtBool (b: BOOLEAN): TEXT =
  BEGIN
    IF b THEN RETURN "TRUE" ELSE RETURN "FALSE" END
  END FmtBool;

PROCEDURE FmtU (u: Test1.U): TEXT =
  BEGIN
    TYPECASE u OF
    | Test1.U_BOOLEAN (x) => RETURN "BOOLEAN[" & FmtBool(x.v) & "]"
    | Test1.U_O1 (x) => RETURN "O1[" & Ilu.SbhFromObject(x.v) & "]"
    ELSE
      RETURN "[unknown U variant]"
    END (*typecase*)
  END FmtU;

PROCEDURE Start () =
  VAR
    handle    : Test1.O1;
    o2        : Test1.O2;
    o3        : Test1.O3;
    f         : REAL;
    u, u2     : Test1.U;
    css, css2 : Test1.CSS;
    ro        : Test1.RO;
    r         : Test1.R;
    a         : Test1.A0;
    a1        : Test1.A1;
    i         : Test1.I;
    rs        : Test1.RS;
    i2        : Test1.IS;
    tT1O3, o3t: Ilu.ObjectType;
    r1, r2    : Ilu.Real;
  BEGIN
    TRY
      handle :=
        IluSimpleBind.Lookup(
          "Test1_Initial_Object@Test1-Server",
          Test1.ILU_Get_Type_O1(NIL))
    EXCEPT
      IluBasics.Failed (f) =>
        Say("Lookup failed (" & f.info & ")");
        RETURN;
    END (* try-except *);
    css := NEW(Test1.CSS, 2);
    css^ := ARRAY OF TEXT{"hello world", "hello mars"};
    u :=
      handle.U_CSS_to_U(
        NEW(Test1.U_BOOLEAN, d := Test1.U_BOOLEAN__Tag, v := TRUE), css);
    Say("u = " & FmtU(u));
    ro := handle.f_CSS_to_RO(css);
    Say("ro->i=" & Fmt.Int(ro.i));

    f := handle.R_ScS_to_F(ro^, css[0]);
    Say("f=" & Fmt.Real(f, 7, Fmt.Style.Flo));

    handle.a_RO(ro);

    o2 := handle.get_O2();

    Say("got O2, sbh = " & Ilu.SbhFromObject(o2));

    css2 := o2.OO_A0_to_CSS(handle, a);

    r.css := NEW(Test1.CSS, 0);
    r.i := 12;
    r.a := Test1.A1{"this is", "data", "initialization"};
    a1 := Test1.A1{"but this", "is", "fun"};
    a := o2.R_I_A1_to_I_A0(r, i, a1);

    rs := NEW(Test1.RS, 0);
    o3 := handle.get_O3(FALSE);
    Say("got O3, sbh = " & Ilu.SbhFromObject(o3));
    o3t := o3.ILU_Get_Type();
    tT1O3 := Test1.ILU_Get_Type_O3(o3);
    IF o3t # tT1O3 THEN
      Say("Instance of type " & Ilu.IdOfObjectType(o3t) & " received!")
    ELSE
      i2 := o3.RS_R_to_R_IS(rs, r);
      o3.O1_U_to_U(handle, u);
      Say("u = " & FmtU(u));
    END (*if*);

    o3 := handle.get_O3(TRUE);
    rs := NEW(Test1.RS, 0);
    Say("got O3, sbh = " & Ilu.SbhFromObject(o3));
    i2 := o3.RS_R_to_R_IS(rs, r);
    o3.O1_U_to_U(handle, u);
    Say("u = " & FmtU(u));
    TYPECASE o3 OF
    | Test3.O (x) => u2 := x.I_to_Test1U(397); Say("u2 = " & FmtU(u2));
    ELSE
      EVAL 0
    END (*typecase*);

    o3 := handle.get_O3(FALSE);
    Say("got O3, sbh = " & Ilu.SbhFromObject(o3));
    TYPECASE o3 OF
    | Test1.O4 (x) =>
        r1 := 12345.6789D0;
        r2 := x.R_to_R(r1);
        Say(
          "doubles:  r1 is " & Fmt.LongReal(r1, 10, Fmt.Style.Flo)
            & ", r2 is " & Fmt.LongReal(r2, 10, Fmt.Style.Flo));
    ELSE
      EVAL 0
    END (*typecase*);
    RETURN;
  END Start;

BEGIN
  Start();
END TestClient.
