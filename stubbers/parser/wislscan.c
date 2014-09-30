/** 
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
/*
Program using WINIO that prompts for working directory
command line args and then calls main.

Dan Larner, 1995
*/

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))

#include <windows.h>
#include <winio.h>

#ifdef WIN16
extern int scan_main (int ac, char **av, char **envp);
#define _MAX_PATH 256
#else
extern int main (int ac, char **av, char **envp);
#endif

int CALLBACK WinMain(HANDLE hInstance, HANDLE hPrevInstance, 
    LPSTR lpCmdLine, int nCmdShow) {
   	int i_argc;
	char* argv [3]; // only takes 1 arg
	char szArguments[_MAX_PATH * 3]; // large in case the user enters extra nonsense
	int i_result;
	if (winio_setmain(hInstance, hPrevInstance, lpCmdLine, nCmdShow,  
    	&i_argc, argv, 3, "ISL Scan", "wislscan.exe", szArguments) != 1) 
    	return -1;
#ifdef WIN16
   	i_result = scan_main(i_argc, argv, NULL); // call main
#else
   	i_result = main(i_argc, argv, NULL); // call main
#endif
	printf("\nwislscan complete\n");
	winio_end(); 	// let user look at output till user closes 
    return i_result;
}

#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */
