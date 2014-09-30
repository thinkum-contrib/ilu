/** $Id: cppclntw.cpp,v 1.8 1999/08/03 01:52:15 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:28 am PDT */

/* A crude brute force adaptation of the command line clnt.c program
   into a *very* elementry windows version (adapted from Petzold 
   "Programming Windows 3.1" */

#include <windows.h>

#include <stdio.h>
#if defined (_WINIO)
#include <winio.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "resource.h"

#include "Test1.hh"
#include "Test2.hh"
#include "Test3.hh" 

/* holds handle of our main window */
HWND g_h_hwnd;

char* g_pc_windowchars = NULL;

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
 article PSS ID Number: Q104248 for more infomation on this static initializer ordering.)
*/
#pragma init_seg(lib)     
     /* initialize winsock when under WIN16 */
int g_i_force_initializer_run = ilu_StartupWinsock ();
#endif


static void printU(char* pc_line, char *prefix, char *varName, Test1_T_U u)
{
    sprintf(pc_line, "%s%s.discriminator=%d ", prefix, varName, u.discriminator);
    strcat(g_pc_windowchars, pc_line);
    switch (u.discriminator)
    {
    case 3:
    sprintf(pc_line, "%s.value.O1 = 0x%lx", varName, (unsigned long) u.value.O1);
    break;

    case 5:
       
    sprintf(pc_line, "%s.value.boolean = %u", varName, u.value.boolean);
    
    break;

    default:
    sprintf(pc_line, "(unexpected value)");
    break;
    }
    strcat(g_pc_windowchars, pc_line);
    strcat(g_pc_windowchars, "\n");
}

int dotest() /* used to basically be main() in cppclnt.c */
{
    Test1Status s1;
    Test1_T_O1  *handle;
    Test1_T_O2  *o2;
    Test1_T_O3  *o3;
    float   f;
    Test1_T_U   u;
    Test1_T_CSS css;
    Test1_T_ScS scs[ 3 ];
    Test1_T_RO  ro;
    Test1_T_R   r;
    char c_line[512];
    RECT        rect ;
   
    /* blank our window chars */
    g_pc_windowchars[0] = '\0';

        if ((handle = (Test1_T_O1 *) iluObject::Lookup("Test1-Server", "Test1_Initial_Object",
                               Test1_T_O1::ILUClassRecord)) == NULL) {
        MessageBox(g_h_hwnd, "iluObject::Lookup(\"Test1-Server/Test1_Initial_Object ...)\" failed", 
                    "Error", MB_OK | MB_ICONEXCLAMATION);
        return (-1);
    }    

    u.discriminator = 5;
    u.value.boolean = 1;
    scs[0] = "hello world";
    scs[1] = "hello mars";
    css = _Test1_T_CSS_sequence::Create( 2, scs );
    u = *handle->U_CSS_to_U( &s1, &u, css );
    printU(c_line, "", "u", u);
    ro = handle->f_CSS_to_RO( &s1, css );
    sprintf(c_line,  "ro->i=%ld\n", ro->i );
    strcat(g_pc_windowchars, c_line);

    f = handle->R_ScS_to_F( &s1, ro, scs[0] );
    sprintf(c_line,  "f=%f\n", f );
    strcat(g_pc_windowchars, c_line);


    handle->a_RO( &s1, ro );

    o2 = handle->get_O2 ( &s1 );
    if (s1.returnCode == Test1Reply_Success)
      {
    Test1_T_A0 a;
    ilu_Byte *ap;
    Test1_T_A1 a1;
    Test1_T_I i;
    Test1_T_CSS css2;

    sprintf(c_line, "got O2, sbh = %s\n", o2->ILUStringBindingHandle());
    strcat(g_pc_windowchars, c_line);

    css2 = o2->OO_A0_to_CSS ( &s1, handle, a);
    if (s1.returnCode == Test2Reply_Success)
      {
      }
    else
      {
        sprintf(c_line, "exception on Test1_T_O2::OO_A0_to_CSS, exception is \"%s\"\n", s1.returnCode);
        strcat(g_pc_windowchars, c_line);
      }

    r.css = new _Test1_T_CSS_sequence;
    r.i = 12;
    r.a[0] = "this is";
    r.a[1] = "data";
    r.a[2] = "initialization";
    a1[0] = "but this";
    a1[1] = "is";
    a1[2] = "fun";
    ap = (ilu_Byte *) o2->R_I_A1_to_I_A0 (&s1, &r, &i, a1);
      }
    else
      {
    sprintf(c_line, "couldn't get an instance of O2.  Exception is \"%s\".\n", s1.returnCode);
    strcat(g_pc_windowchars, c_line);
      }

    o3 = handle->get_O3 ( &s1, ilu_FALSE );
    if (s1.returnCode == Test1Reply_Success)
      {
    Test1_T_RS rs = _Test1_T_TheRS_sequence::Create (0, NULL);
    Test1_T_IS i2;

    sprintf(c_line, "got O3, sbh = %s, type = %s\n", o3->ILUStringBindingHandle(), o3->ILUClassName());
    strcat(g_pc_windowchars, c_line);

    if (o3->ILUInstanceClassRecord != ilu::FindClassFromTypeName("Test1.O3"))
      {
        sprintf(c_line, "instance of class %s received!\n", o3->ILUClassName());
        strcat(g_pc_windowchars, c_line);
      }
    else
      {
        i2 = o3->RS_R_to_R_IS ( &s1, rs, &r);
        o3->O1_U_to_U ( &s1, handle, &u);
        printU(c_line, "", "u", u);
      }
      }
    else
      {
    sprintf(c_line, "couldn't get an instance of O3.  Exception is \"%s\".\n", s1.returnCode);
    strcat(g_pc_windowchars, c_line);
      }

    /* this next call should return an instance of Test3.O */
    o3 = handle->get_O3 ( &s1, ilu_TRUE );
    if (s1.returnCode == Test1Reply_Success)
      {
    Test1_T_RS rs = _Test1_T_TheRS_sequence::Create (0, NULL);
    Test1_T_IS i2;

    sprintf(c_line, "got O3, sbh = %s, type = %s\n", o3->ILUStringBindingHandle(), o3->ILUClassName());
    strcat(g_pc_windowchars, c_line);

    i2 = o3->RS_R_to_R_IS (&s1, rs, &r);
    o3->O1_U_to_U (&s1, handle, &u);
    printU(c_line, "", "u", u);

    if (o3->ILUInstanceClassRecord == ilu::FindClassFromTypeName("Test3.O"))
      {
        Test3_T_O   *o;
        Test3Status s3;
        Test1_T_U u2;

        o = Test3_T_O::ILUQuaT(o3);
        u2 = *o->I_to_Test1U (&s3, 397);
        if (s3.returnCode != Test3Reply_Success)
          {
        sprintf(c_line, "exception on Test3_O::I_to_Test1U, exception is \"%s\"\n", s3.returnCode);
        strcat(g_pc_windowchars, c_line);
          }
        else
          printU(c_line, "Test3_O::I_to_Test1U:  ", "u2", u2);
      }
      }
    else
      {
    sprintf(c_line, "couldn't get an instance of O3.  Exception is \"%s\".\n", s1.returnCode);
    strcat(g_pc_windowchars, c_line);
      }

    /* this next call should return an instance of Test1.O4 */
    o3 = handle->get_O3 ( &s1, ilu_FALSE );
    if (s1.returnCode == Test1Reply_Success)
      {
    sprintf(c_line, "got O3, sbh = %s, type = %s\n", o3->ILUStringBindingHandle(), o3->ILUClassName());
    strcat(g_pc_windowchars, c_line);

    if (o3->ILUInstanceClassRecord == ilu::FindClassFromTypeName("Test1.O4"))
      {
        Test1_T_O4  *o4;
        ilu_real r1, r2;

        o4 = Test1_T_O4::ILUQuaT(o3);
        r2 = o4->R_to_R (&s1, r1 = 12345.6789);
        if (s1.returnCode != Test1Reply_Success)
          {
        sprintf(c_line, "exception on R_to_R, exception is \"%s\"\n", s1.returnCode);
        strcat(g_pc_windowchars, c_line);
          }
        else
          sprintf(c_line,  "doubles:  r1 is %.10f, r2 is %.10f\n", r1, r2);
          strcat(g_pc_windowchars, c_line);
      }
      }
    else
      {
    sprintf(c_line, "couldn't get an instance of O3.  Exception is \"%s\".\n", s1.returnCode);
    strcat(g_pc_windowchars, c_line);
      }

    /* force an entire window repaint */
    GetClientRect (g_h_hwnd, &rect);
    InvalidateRect(g_h_hwnd, &rect, TRUE);
    SendMessage(g_h_hwnd, WM_PAINT, 0, 0);
    return(0);
}


long CALLBACK WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam)
     {
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rect ;
     static int i_doingtest = 0;
     
     switch (message)
          {
          case WM_PAINT :
               hdc = BeginPaint (hwnd, &ps) ;
               GetClientRect (hwnd, &rect) ;
               DrawText (hdc, g_pc_windowchars, -1, &rect, DT_LEFT) ;
               EndPaint (hwnd, &ps) ;
               return 0 ;

          case WM_DESTROY :
               PostQuitMessage (0) ;
               return 0 ;

          case WM_COMMAND :
            switch (LOWORD(wParam)) {
                case ID_ACTION_RUN  :
                    /* on win16, something processes messages for us while we're blocked in 
                    a select(), which means we can enter here even though we're already in 
                    the middle of a test, so prevent re entering. */
                    if (i_doingtest == 0) {
                        i_doingtest = 1;
                        if (dotest() != 0) 
                            MessageBox(hwnd, "dotest unsuccessful", "Error", MB_OK | MB_ICONEXCLAMATION);
                        i_doingtest = 0;
                    }
                    break;
                case ID_ACTION_EXIT :
                    PostQuitMessage (0) ;
                    break;

            }
          }

     return DefWindowProc (hwnd, message, wParam, lParam) ;
     }


int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpszCmdParam, int nCmdShow)
     {                     
     static char szAppName[] = "iluTest1WindowsClient" ;
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
          wndclass.hIcon         = LoadIcon (hInstance, "clntw_icon") ;
          wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
          wndclass.hbrBackground = (HBRUSH)GetStockObject (WHITE_BRUSH) ;
          wndclass.lpszMenuName  = MAKEINTRESOURCE(IDR_MENU1) ;
          wndclass.lpszClassName = szAppName ;

          RegisterClass (&wndclass) ;
          }                                

     hwnd = CreateWindow (szAppName,       // window class name
                    "iluTest1WindowsClient",   // window caption
                    WS_OVERLAPPEDWINDOW,   // window style
                    CW_USEDEFAULT,         // initial x position
                    CW_USEDEFAULT,         // initial y position
                    CW_USEDEFAULT,         // initial x size
                    CW_USEDEFAULT,         // initial y size
                    NULL,                  // parent window handle
                    NULL,                  // window menu handle
                    hInstance,             // program instance handle
                    NULL) ;                // creation parameters

     /* save our window handle in the global */
     g_h_hwnd = hwnd;
     
     g_pc_windowchars = (char*) malloc(8192);
     if (g_pc_windowchars == NULL) {
        MessageBox(NULL, "Couldn't malloc(8192) for g_pc_windowchars", "Error", 
                    MB_ICONSTOP | MB_OK);
        return 0;
     }
     g_pc_windowchars[0] = '\0';

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
            nCmdShow, 0, "clientw Console");
#endif

     ShowWindow (hwnd, nCmdShow) ;
     UpdateWindow (hwnd) ;

     while (GetMessage (&msg, NULL, 0, 0))
          {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
          }
         
     return msg.wParam ;
     }

