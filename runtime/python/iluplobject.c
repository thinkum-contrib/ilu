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
/*

$Id: iluplobject.c,v 1.3 1999/08/03 01:55:42 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluplobject.h"

PyObject *
ilupl_FromPipeline (ilu_Pipeline pl)
{
	IluplObject *	p	= PyObject_NEW(IluplObject, &Ilupl_Type);

	p->pipeline = pl;
	return (PyObject *) p;
}

ilu_Pipeline
  ilupl_AsPipeline(IluplObject *p)
{
  return (p->pipeline);
}

static void
ilupl_dealloc(PyObject *o)
{
  ilu_Error kerr;
  ilu_ReleasePipeline(ilupl_AsPipeline((IluplObject *) o), &kerr);
  if (ILU_ERRNOK(kerr)) { ILU_HANDLED(kerr); };

  PyMem_DEL(o);
}

PyTypeObject	Ilupl_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Pipeline",
	sizeof(IluplObject),
	0,
	ilupl_dealloc,		/*tp_dealloc*/
	0,			/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
