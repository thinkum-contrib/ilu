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

$Id: ilulrobject.c,v 1.10 1999/08/03 01:55:33 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilulrobject.h"

#ifdef __STDC__
#include <limits.h>
#endif

#ifndef LONG_MAX
#define LONG_MAX 0X7FFFFFFFL
#endif

#ifndef LONG_MIN
#define LONG_MIN (-LONG_MAX-1)
#endif

#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif

static void
ilulr_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
ilulr_FromLongReal(ilu_longreal lr)
{
	IlulrObject *	p	= PyObject_NEW(IlulrObject, &Ilulr_Type);

	if (p == 0)
		return 0;
	p->lr = lr;
	return (PyObject *) p;
}

#define Negate(p)		(((unsigned char *)p)[0] ^= 0x80)

static PyObject *
ilulr_Infinity(int isNegative)
{
	ilu_longreal	lr;

#ifdef LONG_REAL_TYPE
	lr = -lr;
#else
	int i;

	lr.b[0] = 0x7f;
	lr.b[1] = 0xff;
	for (i = 2; i < ilulr_NBytesInLongReal; i++)
		lr.b[i] = 0;
	if (isNegative)
		Negate(&lr);
#endif
	return ilulr_FromLongReal(lr);
}

static PyObject *
ilulr_NaN(void)
{
	ilu_longreal	lr;
	int		i;

	((unsigned char *)&lr)[0] = 0x7f;
	((unsigned char *)&lr)[1] = 0xff;
	((unsigned char *)&lr)[2] = 0x80;
	for (i = 3; i < ilulr_NBytesInLongReal; i++)
		((unsigned char *)&lr)[i] = 0;
	return ilulr_FromLongReal(lr);
}

#define LongRealBias	16383

/*
** Anti-optimizer obfuscation:  this may need to be more obscure if the
** compiler inlines.
*/
static double
identity(double dval)
{
	return dval;
}

static int
isDoubleNaN(double dval)
{
	int	equalsSelf	= identity(dval) == identity(dval);

	return !equalsSelf;
}

PyObject *
ilulr_FromDouble(double dval)
{
	ilu_longreal	lr;

	if (isDoubleNaN(dval))
		return ilulr_NaN();
	if (dval == 0)
	{
		int	i;

		for (i = 0; i < ilulr_NBytesInLongReal; i++)
			((unsigned char *)&lr)[i] = 0;
	}
	else
	{
		int	rawExp	= LongRealBias;
		int	trips	= 1 << 15; /* how big ilu_longreal's exp is */
		int	isNeg	= dval < 0.0;
		int	i;

		if (dval < 0.0)
		{
			((unsigned char *)&lr)[0] = 0x80;
			dval = - dval;
		}
		else
			((unsigned char *)&lr)[0] = 0;

		while (dval >= 2.0)
		{
			dval /= 2.0;
			rawExp += 1;
			if (--trips == 0)
				return ilulr_Infinity(isNeg);
		}
		while (dval < 1.0)
		{
			dval *= 2.0;
			rawExp -= 1;
		}

		((unsigned char *)&lr)[0] = (isNeg ? 0x80 : 0) | ((rawExp >> 8) & 0x7f);
		((unsigned char *)&lr)[1] = rawExp & 0xff;

		dval -= 1.0;
		for (i = 2; i < ilulr_NBytesInLongReal; i++)
		{
			dval *= 256;
			((unsigned char *)&lr)[i] = dval;
			dval -= ((unsigned char *)&lr)[i];
		}
	}
	return ilulr_FromLongReal(lr);
}

static int
bytesAreNonzero(unsigned char *bytes, int len)
{
	int	i;

	for (i = 0; i < len; i++)
		if (bytes[i] != 0)
			return 1;
	return 0;
}

static int
ilulr_print(PyObject *o, FILE *fp, int flags)
{
	IlulrObject *	p	= (IlulrObject *) o;
	int		isNeg	= ((unsigned char *)(&p->lr))[0] & 0x80;
	int		rawExp	= ((((unsigned char *)(&p->lr))[0] & 0x7f) << 8) + ((unsigned char *)(&p->lr))[1];
	int		i;

	fprintf(fp, "<ilu_longreal: ");

	/* Sign */
	if (isNeg)
		putc('-', fp);

	switch (rawExp)
	{
	case 0x7fff:
		/* Inf or NaN */
		if (bytesAreNonzero(((unsigned char *)(&p->lr)) + 2, ilulr_NBytesInLongReal - 2))
			fprintf(fp, "NaN");
		else
			fprintf(fp, "Infinity");
		break;

	case 0:
		/* Denormalized */
		fprintf(fp, "0x0.");
		for (i = 2; i < ilulr_NBytesInLongReal; i++)
			fprintf(fp, "%02x", ((unsigned char *)(&p->lr))[i]);
		fprintf(fp, " * 2**%d", 1 - LongRealBias);
		break;

	default:
		/* Normalized */
		fprintf(fp, "0x1.");
		for (i = 2; i < ilulr_NBytesInLongReal; i++)
			fprintf(fp, "%02x", ((unsigned char *)(&p->lr))[i]);
		fprintf(fp, " * 2**%d", rawExp - LongRealBias);
		break;
	}
	fprintf(fp, ">");
	return 0;
}

static int
longRealCompare(ilu_longreal *v, ilu_longreal *w)
{
	int	diff;
	int	i;

	if ((diff = (signed char) ((unsigned char *)v)[0] - (signed char) ((unsigned char *)w)[0]) != 0)
		return diff;
	for (i = 1; i < ilulr_NBytesInLongReal; i++)
		if ((diff = ((unsigned char *)v)[i] - ((unsigned char *)w)[i]) != 0)
			break;
	return diff;
}

static int
isNegativeZero(ilu_longreal *p)
{
	return ((unsigned char *)p)[0] == 0x80 &&
		!bytesAreNonzero(((unsigned char *)p) + 1, ilulr_NBytesInLongReal - 1);
}

static int
ilulr_compare(PyObject *vo, PyObject *wo)
{
	IlulrObject *	v	= (IlulrObject *) vo;
	IlulrObject *	w	= (IlulrObject *) wo;
	ilu_longreal *	vp	= &v->lr;
	ilu_longreal *	wp	= &w->lr;
	ilu_longreal	vlr;
	ilu_longreal	wlr;

	if (isNegativeZero(vp))
	{
		vlr = *vp;
		vp = &vlr;
		Negate(vp);
	}
	if (isNegativeZero(wp))
	{
		wlr = *wp;
		wp = &wlr;
		Negate(wp);
	}
	return longRealCompare(vp, wp);
}

static int
checkNotNaN(IlulrObject *p)
{
	if ((((unsigned char *)(&p->lr))[0] & 0x7f) == 0x7f && ((unsigned char *)(&p->lr))[1] == 0xff &&
	    bytesAreNonzero(((unsigned char *)(&p->lr)) + 2, ilulr_NBytesInLongReal - 2))
	{
		PyErr_SetString(PyExc_ValueError, "NaN");
		return 0;
	}
	return 1;
}

static PyObject *
ilulr_add(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "longreal + is not supported");
	return 0;
}

static PyObject *
ilulr_subtract(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "longreal - is not supported");
	return 0;
}

static PyObject *
ilulr_multiply(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "longreal * is not supported");
	return 0;
}

static PyObject *
ilulr_divide(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "longreal / is not supported");
	return 0;
}

static PyObject *
ilulr_remainder(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "longreal % is not supported");
	return 0;
}

static PyObject *
ilulr_divmod(PyObject *vo, PyObject *wo)
{
	PyErr_SetString(PyExc_TypeError, "longreal divmod is not supported");
	return 0;
}

static PyObject *
ilulr_power(PyObject *vo, PyObject *wo, PyObject *zo)
{
	PyErr_SetString(PyExc_TypeError, "longreal pow is not supported");
	return 0;
}

static PyObject *
ilulr_negative(PyObject *self)
{
	IlulrObject *	p	= (IlulrObject *) self;
	IlulrObject *	result;

	if (!checkNotNaN(p))
		return 0;
	if ((result = (IlulrObject *) ilulr_FromLongReal(p->lr)) == 0)
		return 0;
	Negate(&result->lr);
	return (PyObject *) result;
}

static PyObject *
ilulr_positive(PyObject *self)
{
	IlulrObject *	p	= (IlulrObject *) self;

	if (!checkNotNaN(p))
		return 0;
	Py_INCREF(self);
	return self;
}

static PyObject *
ilulr_absolute(PyObject *self)
{
	IlulrObject *	p	= (IlulrObject *) self;

	if (!checkNotNaN(p))
		return 0;
	return (((unsigned char *)(&p->lr))[0] & 0x080) ?
		ilulr_negative(self) : ilulr_positive(self);
}

static int
ilulr_nonzero(PyObject *self)
{
	IlulrObject *	p	= (IlulrObject *) self;

	if ((((unsigned char *)(&p->lr))[0] & 0x7f) != 0)
		return 1;
	return bytesAreNonzero(((unsigned char *)(&p->lr)) + 1, ilulr_NBytesInLongReal - 1);
}

static int
convertToDouble(PyObject *self, double *pResult)
{
	IlulrObject *	p	= (IlulrObject *) self;
	int		isNeg	= ((unsigned char *)(&p->lr))[0] & 0x80;
	int		rawExp	= ((((unsigned char *)(&p->lr))[0] & 0x7f) << 8) + ((unsigned char *)(&p->lr))[1];
	int		exp	= rawExp - LongRealBias;
	double		value;
	int		i;

	if (!checkNotNaN(p))
		return 0;
	value = 0.0;
	for (i = ilulr_NBytesInLongReal - 1; i >= 2; i--)
		value = (value + (double) ((unsigned char *)(&p->lr))[i]) / 256.0;
	if (rawExp == 0)
		exp += 1;
	else
		value = value + 1.0;
	if (isNeg)
		value = - value;
	while (exp > 0)
	{
		value *= 2.0;
		exp -= 1;
	}
	while (exp < 0)
	{
		value /= 2.0;
		exp += 1;
	}
	*pResult = value;
	return 1;
}

static PyObject *
ilulr_int(PyObject *self)
{
	double		value;

	if (!convertToDouble(self, &value))
		return 0;
	if (value < (long) LONG_MIN || (long) LONG_MAX < value)
	{
		PyErr_SetString(PyExc_OverflowError, "ilu_longreal to int");
		return 0;
	}
	return PyInt_FromLong((long) value);
}

static PyObject *
ilulr_float(PyObject *self)
{
	double		value;

	if (!convertToDouble(self, &value))
		return 0;
	return PyFloat_FromDouble(value);
}

static PyNumberMethods ilulr_as_number =
{
	ilulr_add,	/*nb_add*/
	ilulr_subtract,	/*nb_subtract*/
	ilulr_multiply,	/*nb_multiply*/
	ilulr_divide,	/*nb_divide*/
	ilulr_remainder,/*nb_remainder*/
	ilulr_divmod,	/*nb_divmod*/
	ilulr_power,	/*nb_power*/
	ilulr_negative,	/*nb_negative*/
	ilulr_positive,	/*nb_positive*/
	ilulr_absolute,	/*nb_absolute*/
	ilulr_nonzero,	/*nb_nonzero*/
	0,		/*nb_invert*/
	0,		/*nb_lshift*/
	0,		/*nb_rshift*/
	0,		/*nb_and*/
	0,		/*nb_xor*/
	0,		/*nb_or*/
	0,		/*nb_coerce*/
	ilulr_int,	/*nb_int*/
	0,		/*nb_long*/
	ilulr_float,	/*nb_float*/
	0,		/*nb_oct*/
	0		/*nb_hex*/
};

PyTypeObject	Ilulr_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_longreal",
	sizeof(IlulrObject),
	0,
	ilulr_dealloc,		/*tp_dealloc*/
	ilulr_print,		/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	ilulr_compare,		/*tp_compare*/
	0,			/*tp_repr*/
	&ilulr_as_number,	/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
