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

$Id: iluftobject.c,v 1.10 1999/08/03 01:55:31 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluftobject.h"

static void
iluft_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
iluft_FromFineTime(ilu_FineTime ft)
{
	IluftObject *	p	= PyObject_NEW(IluftObject, &Iluft_Type);

	if (p == 0)
		return 0;
	p->ft = ft;
	return (PyObject *) p;
}

ilu_FineTime
iluft_AsFineTime(PyObject *self)
{
	return ((IluftObject *) self)->ft;
}

static int
iluft_print(PyObject *o, FILE *fp, int flags)
{
	IluftObject *	p	= (IluftObject *) o;

	fprintf(fp, "<ilu_FineTime:  %ld + %lu/%lu>",
		(long) p->ft.ft_s, (unsigned long) p->ft.ft_t,
		(unsigned long) ilu_FineTimeRate);
	return 0;
}

static int
iluft_compare(PyObject *vo, PyObject *wo)
{
	IluftObject *	v	= (IluftObject *) vo;
	IluftObject *	w	= (IluftObject *) wo;

	return ilu_FineTime_Cmp(v->ft, w->ft);
}

static PyObject *
iluft_add(PyObject *vo, PyObject *wo)
{
	IluftObject *	v	= (IluftObject *) vo;
	IluftObject *	w	= (IluftObject *) wo;
	ilu_FineTime	ft;

	ft = ilu_FineTime_Add(v->ft, w->ft);
	return iluft_FromFineTime(ft);
}

static PyObject *
iluft_sub(PyObject *vo, PyObject *wo)
{
	IluftObject *	v	= (IluftObject *) vo;
	IluftObject *	w	= (IluftObject *) wo;
	ilu_FineTime	ft;

	ft = ilu_FineTime_Sub(v->ft, w->ft);
	return iluft_FromFineTime(ft);
}

static PyObject *
iluft_mul(PyObject *vo, PyObject *wo)
{
	IluftObject *	v	= (IluftObject *) vo;
	IluftObject *	w	= (IluftObject *) wo;
	double		rate	= ilu_FineTimeRate;
	double		wFloat	= w->ft.ft_s + w->ft.ft_t / rate;
	ilu_FineTime	ft;

	ft = ilu_FineTime_Mul(v->ft, wFloat);
	return iluft_FromFineTime(ft);
}

static PyObject *
iluft_div(PyObject *vo, PyObject *wo)
{
	IluftObject *	v	= (IluftObject *) vo;
	IluftObject *	w	= (IluftObject *) wo;
	double		rate	= ilu_FineTimeRate;
	double		wFloat	= w->ft.ft_s + w->ft.ft_t / rate;
	ilu_FineTime	ft;

	if (wFloat == 0.0)
	{
		PyErr_SetString(PyExc_ZeroDivisionError,
			"ilu_FineTime division");
		return 0;
	}
	ft = ilu_FineTime_Mul(v->ft, 1.0 / wFloat);
	return iluft_FromFineTime(ft);
}

static PyObject *
iluft_rem(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "ilu_FineTime % is not supported");
	return 0;
}

static PyObject *
iluft_divmod(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError,
		"ilu_FineTime divmod is not supported");
	return 0;
}

static PyObject *
iluft_power(PyObject *vo, PyObject *wo, PyObject *zo)
{
	PyErr_SetString(PyExc_TypeError, "ilu_FineTime pow is not supported");
	return 0;
}

static PyObject *
iluft_neg(PyObject *o)
{
	IluftObject *	p	= (IluftObject *) o;
	ilu_FineTime	zero;
	ilu_FineTime	ft;

	zero.ft_s = 0;
	zero.ft_t = 0;
	ft = ilu_FineTime_Sub(zero, p->ft);
	return iluft_FromFineTime(ft);
}

static PyObject *
iluft_pos(PyObject *o)
{
	Py_INCREF(o);
	return o;
}

static PyObject *
iluft_abs(PyObject *o)
{
	IluftObject *	p	= (IluftObject *) o;
	ilu_FineTime	zero;

	zero.ft_s = 0;
	zero.ft_t = 0;
	return ilu_FineTime_Cmp(p->ft, zero) < 0 ? iluft_neg(o) : iluft_pos(o);
}

static int
iluft_nonzero(PyObject *o)
{
	IluftObject *	p	= (IluftObject *) o;

	return p->ft.ft_s != 0 || p->ft.ft_t != 0;
}

static int
iluft_coerce(PyObject **pv, PyObject **pw)
{
	ilu_FineTime	ft;

	if (PyInt_Check(*pw))
	{
		ft.ft_s = PyInt_AsLong(*pw);
		ft.ft_t = 0;
	}
	else if (PyFloat_Check(*pw))
		ft = ilu_FineTime_FromDouble(PyFloat_AsDouble(*pw));
	else
		return 1; /* failed */
	*pw = iluft_FromFineTime(ft);
	Py_INCREF(*pv);
	return 0;
}

static PyObject *
iluft_int(PyObject *o)
{
	IluftObject *	p	= (IluftObject *) o;
	long		value	= p->ft.ft_s + p->ft.ft_t / ilu_FineTimeRate;

	return PyInt_FromLong(value);
}

static PyObject *
iluft_long(PyObject *o)
{
	IluftObject *	p	= (IluftObject *) o;
	double		rate	= ilu_FineTimeRate;
	double		value	= p->ft.ft_s + p->ft.ft_t / rate;

	return PyLong_FromDouble(value);
}

static PyObject *
iluft_float(PyObject *o)
{
	IluftObject *	p	= (IluftObject *) o;
	double		rate	= ilu_FineTimeRate;
	double		value	= p->ft.ft_s + p->ft.ft_t / rate;

	return PyFloat_FromDouble(value);
}

static PyNumberMethods iluft_as_number =
{
	iluft_add,	/*nb_add*/
	iluft_sub,	/*nb_subtract*/
	iluft_mul,	/*nb_multiply*/
	iluft_div,	/*nb_divide*/
	iluft_rem,	/*nb_remainder*/
	iluft_divmod,	/*nb_divmod*/
	iluft_power,	/*nb_power*/
	iluft_neg,	/*nb_negative*/
	iluft_pos,	/*nb_positive*/
	iluft_abs,	/*nb_absolute*/
	iluft_nonzero,	/*nb_nonzero*/
	0,		/*nb_invert*/
	0,		/*nb_lshift*/
	0,		/*nb_rshift*/
	0,		/*nb_and*/
	0,		/*nb_xor*/
	0,		/*nb_or*/
	iluft_coerce,	/*nb_coerce*/
	iluft_int,	/*nb_int*/
	iluft_long,	/*nb_long*/
	iluft_float,	/*nb_float*/
	0,		/*nb_oct*/
	0		/*nb_hex*/
};

PyTypeObject	Iluft_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_FineTime",
	sizeof(IluftObject),
	0,
	iluft_dealloc,		/*tp_dealloc*/
	iluft_print,		/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	iluft_compare,		/*tp_compare*/
	0,			/*tp_repr*/
	&iluft_as_number,	/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
