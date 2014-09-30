/** $Id: cppsrvrw.cpp,v 1.8 1999/08/03 01:52:15 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:29 am PDT */

/* A crude brute force adaptation of the command line srvr.c program
   into a *very* elementry windows version (adapted from Petzold 
   "Programming Windows 3.1" */

#include <windows.h>

#include <stdio.h>
#if defined (_WINIO)
#include <winio.h>
#endif

//#include <math.h>
//#include <stdlib.h>
//#include <string.h>

#include "resource.h"

#ifdef AddPort
/* winspool.h (included by windows.h on WIN32) defines AddPort as AddPortA, 
   so we need to undef it here temporarily so it doesn't interfere with
   iluServer.AddPort()
*/
#undef AddPort
#define _undefined_addport
#endif /* AddPort */

#include "Test1.hh"
#include "Test3.hh" 

#ifdef _undefined_addport
/* If AddPort was undeffed, redefine it as is done in winspool.h */
#ifdef UNICODE
#define AddPort  AddPortW
#else
#define AddPort  AddPortA
#endif /* UNICODE */
#undef _undefined_addport
#endif  /* _undefined_addport */


#define _MAX_WINDOW_CHARS 8192

/* holds handle of our main window and application instance */
HWND g_h_hwnd;
HINSTANCE g_h_hInstance;

char g_windowchars[_MAX_WINDOW_CHARS] = "";
char g_c_line[512] = "";

#ifdef WIN16
/* NOTE!:  Under WIN32, you never need the call to ilu_StartupWinsock since ilu
 there is implemented as a dll, and dll's under NT have an entry that allows 
 things to happen when a process attaches to the dll.  It is at that point under win32
 that the winsock startup is taken care of for you.  Under WIN16, ilu is in static
 libraries, and there is no such mechanism.  Now it isn't a problem with the C 
 runtime, since the app can call ilu_StartupWinsock before it performs any ilu
 functions.  With the C++ runtime however, there are some static object initializers
 that make use of winsock operations (and these are run before WinMain is even entered).
 So we have to insure that ilu_StartupWinsock is somehow called before these other ilu
 initializers.  We do this by having a static initializer ourselves that causes
 ilu_StartupWinsock to be called, and we ensure that this happens before the ilu
 initializers by using the Microsoft pragma init_seg(lib) (Note that the ilu initializers
 occur in the 'user' part of the startup sequence.  See Microsoft Knowledge Base
 artical PSS ID Number: Q104248 for more infomation on this static initializer ordering.)
*/
#pragma init_seg(lib)     
     /* initialize winsock when under WIN16 */
int g_i_force_initializer_run = ilu_StartupWinsock ();
#endif


void paintit() {

    static int i_charcount = 10;   /* 10 to allow for safety margin */
    int i_c_line_length;
    RECT        rect ;

    i_c_line_length = strlen(g_c_line);

    if ((i_charcount + i_c_line_length) >= _MAX_WINDOW_CHARS) { 
         g_windowchars[0] = '\0';
         i_charcount = 10;
    }
    strcat(g_windowchars, g_c_line);
    i_charcount = i_charcount + i_c_line_length;

    /* force an entire window repaint - really crude here */
    GetClientRect (g_h_hwnd, &rect);
    InvalidateRect(g_h_hwnd, &rect, TRUE);
    SendMessage(g_h_hwnd, WM_PAINT, 0, 0);
    return;
}



class Test1_T_O1_impl : public virtual Test1_T_O1 {
public:
  Test1_T_O1_impl(char *instanceHandle, iluServer *server);

  virtual char * ILUGetInstanceHandle();
  virtual iluServer * ILUGetServer();

  virtual Test1_T_U * U_CSS_to_U (Test1Status *_status, Test1_T_U * u, Test1_T_CSS css);
  virtual Test1_T_RO f_CSS_to_RO (Test1Status *_status, Test1_T_CSS css);
  virtual ilu_ShortReal R_ScS_to_F (Test1Status *_status, Test1_T_R * r, Test1_T_ScS s);
  virtual void a_RO (Test1Status *_status, Test1_T_RO ro);
  virtual class Test1_T_O2 * get_O2 (Test1Status *_status);
  virtual class Test1_T_O3 * get_O3 (Test1Status *_status, ilu_Boolean subclass);

private:
  char *ourInstanceHandle;
  iluServer *ourServer;
};


class Test1_T_O2_impl : public virtual Test1_T_O2 {
public:
  virtual Test1_T_CSS OO_A0_to_CSS (Test1Status *_status, Test1_T_OO o, Test1_T_A0 a);
  virtual Test1_T_A0 * R_I_A1_to_I_A0 (Test1Status *_status, Test1_T_R * r, Test1_T_I * i, Test1_T_A1 a);
};


class Test1_T_O3_impl : public virtual Test1_T_O3 {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
};


class Test1_T_P_impl : public virtual Test1_T_P {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
  virtual Test1_T_IS m2 (Test1Status *_status, ilu_Integer j);
};


class Test1_T_O4_impl : public virtual Test1_T_O4 {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
  virtual ilu_Real R_to_R (Test1Status *_status, ilu_Real r);
};


class Test3_T_O_impl : public virtual Test3_T_O {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
  virtual ilu_Integer SR_to_I (Test2Status *_status, ilu_ShortReal i);
  virtual Test1_T_U * I_to_Test1U (Test3Status *_status, ilu_Integer i);
};


static char *strdup(char *s)
{
  char *copy = NULL;
  if (s != NULL)
    {
      copy = new char[strlen(s) + 1];
      strcpy (copy, s);
    }
  return (copy);
}

///////////////////// Test1_T_O1_impl methods /////////////////////

Test1_T_O1_impl::Test1_T_O1_impl(char *instanceHandle, iluServer *server)
{
  this->ourInstanceHandle = instanceHandle;
  this->ourServer = server;
}

char * Test1_T_O1_impl::ILUGetInstanceHandle()
{
  return this->ourInstanceHandle;
}

iluServer * Test1_T_O1_impl::ILUGetServer()
{
  return this->ourServer;
}

Test1_T_U * Test1_T_O1_impl::U_CSS_to_U (Test1Status *_status, Test1_T_U * u, Test1_T_CSS)
{
  Test1_T_U *result = new Test1_T_U;

  sprintf(g_c_line, "Test1.O1.U-CSS-to-U\n");
  paintit();
  *result = *u;
  _status->returnCode = Test1Reply_Success;
  return result;
}

Test1_T_RO Test1_T_O1_impl::f_CSS_to_RO (Test1Status *_status, Test1_T_CSS)
{
  Test1_T_RO x = new Test1_T_R;

  x->i = 9;
  x->css = new _Test1_T_CSS_sequence;
  x->a[0]= strdup("hi");
  x->a[1]= strdup("hi");
  x->a[2]= strdup("hi");
  sprintf(g_c_line, "Test1.O1.f-CSS-to-R0\n");
  paintit();
  _status->returnCode = Test1Reply_Success;
  return x;
}

ilu_ShortReal Test1_T_O1_impl::R_ScS_to_F (Test1Status *_status, Test1_T_R *, Test1_T_ScS)
{
  float f = (float) 39.7;

  sprintf(g_c_line, "Test1.O1.R-ScS-to-F\n");
  paintit();
  _status->returnCode = Test1Reply_Success;
  return f;
}

void Test1_T_O1_impl::a_RO (Test1Status *_status, Test1_T_RO)
{
  sprintf(g_c_line, "Test1.O1.a-RO\n");
  paintit();
  _status->returnCode = Test1Reply_Success;
}

class Test1_T_O2 * Test1_T_O1_impl::get_O2 (Test1Status *_status)
{
  static Test1_T_O2 *uc = NULL;

  sprintf(g_c_line, "Test1.O1.get-O2\n");
  paintit();
  if (uc == NULL)
    uc = new Test1_T_O2_impl;
  if (uc == NULL)
    {
      _status->returnCode = Test1_E_CantCreate;
      return NULL;
    }
  _status->returnCode = Test1Reply_Success;
  return uc;
}

class Test1_T_O3 * Test1_T_O1_impl::get_O3 (Test1Status *_status, ilu_Boolean subclass)
{
  Test1_T_O3 *uc;
  static int one = 0;

  sprintf(g_c_line, "Test1.O1.get-O3\n");
  paintit();
  if (subclass)
    uc = new Test3_T_O_impl();
  else
    {
      if (one == 0)
    {
      one = 1;
      sprintf(g_c_line, "making O3...\n");
      paintit();
      uc = new Test1_T_O3_impl();
    }
      else
    {
      one = 0;
      sprintf(g_c_line, "making O4...\n");
      paintit();
      uc = new Test1_T_O4_impl();
    }
    }
  if (uc == NULL)
    {
      _status->returnCode = Test1_E_CantCreate;
      return NULL;
    }
  _status->returnCode = Test1Reply_Success;
  return uc;
}


///////////////////// Test1_T_O2_impl methods /////////////////////

Test1_T_CSS Test1_T_O2_impl::OO_A0_to_CSS (Test1Status *_status, Test1_T_OO o, Test1_T_A0)
{
  sprintf(g_c_line, "Test1.o2.OO-A0-to-CSS\n");
  paintit();
  if (o == NULL)
    {
      _status->returnCode = Test1_E_E2;
      _status->values.Test1_E_E2_Value = 7;
      return NULL;
    }
  _status->returnCode = Test1Reply_Success;
  return new _Test1_T_CSS_sequence();
}

Test1_T_A0 * Test1_T_O2_impl::R_I_A1_to_I_A0 (Test1Status *_status, Test1_T_R *, Test1_T_I *, Test1_T_A1)
{
  Test1_T_A0 *a2;

  sprintf(g_c_line, "Test1.O2.R-I-A1-to-I-A0\n");
  paintit();
  a2 = (Test1_T_A0 *) malloc(sizeof(Test1_T_A0));
  _status->returnCode = Test1Reply_Success;
  return a2;
}


///////////////////// Test1_T_O3_impl methods /////////////////////

Test1_T_IS Test1_T_O3_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  sprintf(g_c_line, "Test1.O3.RS-R-to-R-IS\n");
  paintit();
  r2->i = 3;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("just");
  r2->a[1] = strdup("a");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test1_T_O3_impl::O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u)
{
  sprintf(g_c_line, "Test1.O3.O1-U-to-U\n");
  paintit();
  u->discriminator = 3;
  u->value.O1 = o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test1_T_O3_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
  _status->returnCode = Test1Reply_Success;
  return b->Length() * b->Length();
}


///////////////////// Test1_T_P_impl methods /////////////////////

Test1_T_IS Test1_T_P_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  sprintf(g_c_line, "Test1.P.RS-R-to-R-IS\n");
  paintit();
  r2->i = 25179;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("from");
  r2->a[1] = strdup("P");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test1_T_P_impl::O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u)
{
  sprintf(g_c_line, "Test1.P.O1-U-to-U\n");
  paintit();
  u->discriminator = 3;
  u->value.O1 = o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test1_T_P_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
  _status->returnCode = Test1Reply_Success;
  return b->Length();
}

Test1_T_IS Test1_T_P_impl::m2 (Test1Status *_status, ilu_Integer j)
{
  Test1_T_IS foo;

  foo = new _Test1_T_IS_sequence;
  foo->Append(j);
  foo->Append(j * j);
  _status->returnCode = Test1Reply_Success;
  return foo;
}


///////////////////// Test1_T_O4_impl methods /////////////////////

Test1_T_IS Test1_T_O4_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  sprintf(g_c_line, "Test1.O4.RS-R-to-R-IS\n");
  paintit();
  r2->i = 25179;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("from");
  r2->a[1] = strdup("P");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test1_T_O4_impl::O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u)
{
  sprintf(g_c_line, "Test1.O4.O1-U-to-U\n");
  paintit();
  u->discriminator = 3;
  u->value.O1 = o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test1_T_O4_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
#define GETB(x)     ((b->Length() <= (x)) ? 0 : b->Nth(x))

  sprintf(g_c_line, "Test1.O4.BS_to_I (%ul:  %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x ...) => %ul\n",
      b->Length(),
      GETB(0), GETB(1), GETB(2), GETB(3), GETB(4), GETB(5), GETB(6), GETB(7), GETB(8), GETB(9), GETB(10),
      b->Length());
  paintit();
  _status->returnCode = Test1Reply_Success;
  return b->Length();
}

ilu_Real Test1_T_O4_impl::R_to_R (Test1Status *_status, ilu_Real r)
{
  ilu_real r2 = 1020304.05060708;

  sprintf(g_c_line, "Test1.O4.R_to_R (%.10f) => %.10f\n", r, r2);
  paintit();
  _status->returnCode = Test1Reply_Success;
  return r2;
}


///////////////////// Test3_T_O_impl methods /////////////////////

Test1_T_IS Test3_T_O_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  sprintf(g_c_line, "Test3.O.RS-R-to-R-IS\n");
  paintit();
  r2->i = 3;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("just");
  r2->a[1] = strdup("a");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test3_T_O_impl::O1_U_to_U (Test1Status *_status, class Test1_T_O1 * o, Test1_T_U * u)
{
  sprintf(g_c_line, "Test3.O.O1-U-to-U(0x%lx, {%d})\n",
    (unsigned long) o, u->discriminator);
  paintit();
  u->discriminator = 3;
  u->value.O1 = o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test3_T_O_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
  _status->returnCode = Test1Reply_Success;
  return b->Length() * b->Length();
}

ilu_Integer Test3_T_O_impl::SR_to_I (Test2Status *_status, ilu_ShortReal i)
{
  _status->returnCode = Test1Reply_Success;
  sprintf(g_c_line, "Test3.O.SR-to-I(%f)\n", i);
  paintit();
  return (ilu_Integer) i;
}

Test1_T_U * Test3_T_O_impl::I_to_Test1U (Test3Status *_status, ilu_Integer i)
{
  Test1_T_U *u;

  sprintf(g_c_line, "Test3.O.I-to-Test1U(%ld)\n", i);
  paintit();
  u = new Test1_T_U;
  u->discriminator = 5;
  u->value.boolean = ilu_TRUE;
  _status->returnCode = Test3Reply_Success;
  return u;
}


extern "C" {
    void set_process_windows_messages_alarm (int* pi_stop);
    int after_windows_quit(int status);
};


int doserve(int* pi_stop) /* used to basically be main() in srvr.c */
{
  /* set up to check messages in the future */
  extern void Test1__InitializeServer(void);
  iluServer s ("Test1-Server", NULL);
  Test1_T_O1 *uc;
  Test1_T_O1 *uc2;

  /* blank our window chars */
  g_windowchars[0] = '\0';

#ifdef AddPort
/* winspool.h (included by windows.h on WIN32) defines AddPort as AddPortA, 
   so we need to undef it here temporarily so it doesn't interfere with
   iluServer.AddPort()
*/
#undef AddPort
  {
    static char *tcp[] = { "sunrpcrm", "tcp_0_0", (char *) 0 };
    s.AddPort("sunrpc_", (ilu_TransportInfo) tcp, ilu_TRUE);
  }
/* now redefine it as is done in winspool.h */
#ifdef UNICODE
#define AddPort  AddPortW
#else
#define AddPort  AddPortA
#endif /* UNICODE */
#else
  s.AddPort(NULL, NULL, ilu_TRUE);
#endif /* AddPort */

  ilu::SetDefaultServer(&s);

  uc = new Test1_T_O1_impl("Test1_Initial_Object", &s);
  if (!uc->ILUPublish())
    {
      sprintf(g_c_line, "*** Error, couldn't publish object\n");
      paintit();
      return (1);
    }

  /* test the publish and lookup a bit */
  uc2 = (Test1_T_O1 *) iluObject::Lookup("Test1-Server",
                     "Test1_Initial_Object",
                     Test1_T_O1::ILUClassRecord);
  if (uc2 != uc)
    sprintf(g_c_line, "*** Error, lookup returns wrong object\n");
    paintit();
  if (!uc2->ILUPublish())
    sprintf(g_c_line, "*** Error, second publish failed\n");
    paintit();

  if (uc != NULL)
    {
      sprintf(g_c_line, "exported %s\n", uc->ILUStringBindingHandle());
      paintit();
      /* set up to check messages in the future */
      set_process_windows_messages_alarm (pi_stop);
      s.Stoppable_Run(pi_stop);
      return 0;
    }
  else
    {
      MessageBox(g_h_hwnd, "couldn't create object\n", 
                    "Error", MB_OK | MB_ICONEXCLAMATION);
      return (-1);
    }
  return 1;
}


BOOL CALLBACK AboutDlgProc (HWND hDlg, UINT message, UINT wParam, LONG lParam) {
     switch (message) {
          case WM_INITDIALOG :
               return TRUE ;
          case WM_COMMAND :
               switch (wParam) {
                case IDOK :
                     EndDialog (hDlg, 0) ;
                     return TRUE ;
                }
               break ;
          }
     return FALSE ;
}
                     
                     
/* our post quit message processing */
int after_windows_quit(int status) {
    return status;
}


long CALLBACK WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam)
     {
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rect ;
     static int i_already_serving = 0;
     static int i_stop = 0;
     
     switch (message)
          {
          case WM_PAINT :
               hdc = BeginPaint (hwnd, &ps) ;
               GetClientRect (hwnd, &rect) ;
               DrawText (hdc, g_windowchars, -1, &rect, DT_LEFT) ;
               EndPaint (hwnd, &ps) ;
               return 0 ;

          case WM_DESTROY :
               PostQuitMessage (0) ;
               return 0 ;

          case WM_COMMAND :
            switch (LOWORD(wParam)) {
                case ID_ACTION_RUN  :
                    if (i_already_serving != 0) {
                        MessageBox(hwnd, "Already Serving", "Info", MB_OK | MB_ICONEXCLAMATION);
                        break;
                    }
                    i_already_serving = 1;
                    i_stop = 0;
                    if (doserve(&i_stop) != 0) { 
                        MessageBox(hwnd, "doserve Unsuccessful", "Error", MB_OK | MB_ICONEXCLAMATION);
                    }
                    MessageBox(hwnd, "doserve Successful", "Error", MB_OK | MB_ICONEXCLAMATION);
                    i_already_serving = 0;                  
                    break;
                case ID_ACTION_EXIT :
                    /* XXX Note:  Under WIN16, something in the tcpip system actually dispatches messages
                    for us while we're blocked in select.  Wherever this dispatch loop is, it
                    seems to not exit when a WM_QUIT is posted, so we'll force our ilu main loop to 
                    stop here by calling ilu_ExitMainLoop on our i_stop variable */  
                    ilu_ExitMainLoop(&i_stop);
                    // MessageBox(NULL, "WndProc posted a quit message", "Info", MB_OK);
                    PostQuitMessage (0) ;
                    /* XXX Note the preceeding works fine under the win16 susbsystem under NT, but doesn't work
                    properly on an actual win16 platform with Microsoft's tcpip ! The select() call in
                    this case doesn't seem to always pay attention to timeouts! So we're going to 
                    just flat out exit on WIN16 */
#ifdef WIN16
                    exit(after_windows_quit(0));
#endif
                    break;
                case ID_ABOUT_SRVRW :
                 DialogBox(g_h_hInstance, MAKEINTRESOURCE(IDD_DIALOG1),
                         g_h_hwnd, (DLGPROC)AboutDlgProc);
                    break;

            }
          }

     return DefWindowProc (hwnd, message, wParam, lParam) ;
     }


int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpszCmdParam, int nCmdShow)
     {
     static char szAppName[] = "iluTest1WindowsServer" ;
     HWND        hwnd ;
     MSG         msg ;
     WNDCLASS    wndclass ;

     if (!hPrevInstance)
          {
          wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
          wndclass.lpfnWndProc   = WndProc ;
          wndclass.cbClsExtra    = 0 ;
          wndclass.cbWndExtra    = 0 ;
          wndclass.hInstance     = hInstance ;
          wndclass.hIcon         = LoadIcon (hInstance, "srvrw_icon") ;
          wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
          wndclass.hbrBackground = (HBRUSH)GetStockObject (WHITE_BRUSH) ;
          wndclass.lpszMenuName  = MAKEINTRESOURCE(IDR_MENU1) ;
          wndclass.lpszClassName = szAppName ;

          RegisterClass (&wndclass) ;
          }

     hwnd = CreateWindow (szAppName,       // window class name
                    "iluTest1WindowsServer",   // window caption
                    WS_OVERLAPPEDWINDOW,   // window style
                    CW_USEDEFAULT,         // initial x position
                    CW_USEDEFAULT,         // initial y position
                    CW_USEDEFAULT,         // initial x size
                    CW_USEDEFAULT,         // initial y size
                    NULL,                  // parent window handle
                    NULL,                  // window menu handle
                    hInstance,             // program instance handle
                    NULL) ;                // creation parameters

     /* save our window handle and app instance in the globals */
     g_h_hwnd = hwnd;
     g_h_hInstance = hInstance;

#ifdef WIN16
/* XXX NOTE: If we were using the C runtime under WIN16, we would normally
do what follows.  Since we are using the C++ runtime in this example, 
we arrange for the ilu_StartupWinsock to occur during static initialization.
See the NOTE near the top of this file for more explanation. */      
     /* initialize winsock when under WIN16 */
     /* ilu_StartupWinsock (); */
#endif

#if defined (_WINIO)
     /* create our console for stdio output (e.g. from the ilu runtime) */
     winio_console(hInstance, hPrevInstance,
            nCmdShow, 0, "serverw Console");
#endif

     ShowWindow (hwnd, nCmdShow) ;
     UpdateWindow (hwnd) ;

     while (GetMessage (&msg, NULL, 0, 0))
          {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
          }
     // MessageBox(NULL, "WinMain left the dispatch loop", "Info", MB_OK);
     return after_windows_quit(msg.wParam) ;
     }

