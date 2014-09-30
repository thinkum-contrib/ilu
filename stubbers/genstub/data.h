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


#ifndef DATA_H_
#define DATA_H_

#define MAXPROPERTIES 30
#define MAXFUNARGS    15

#define lclmemcpy(a,b,c) (memcpy( (void *)(a), (void *)(b), (c)))
#define lclmemset(a,b,c) (memset( (void *)(a), (b), (c)))

enum PropertyKind_ {PropString, PropList, PropBool, PropStruct,
          PropInt, PropOpaque, PropLongInt, PropLocals, PropArgs,
          PropNull, PropLast};

typedef enum PropertyKind_ PropertyKind;

typedef struct { 
        char              *propertyname;
        PropertyKind       propertykind;   
        char              *typename;     /*f property kind is PropStruct*/
        int                typeindex;    /*if property kind is PropStruct*/
     } PropDesc;

typedef struct { 
       char        *typename ;
       int          desc_index;  /* position in TYPES array */
       PropDesc     propertydesc[MAXPROPERTIES];      
   } TypeDesc;


/* Usage e.g., (and see ilup.data.c)
TypeDesc ST_TYPES[ ] = { 
    { "Interface", {
                     { "name",       PropString, NULL} , 
                     { "scoping"   , PropList,    "Interface"},
                     { "brand",      PropString, NULL },
                     ..
                     { "last",       PropLast, NULL}
                   }
    ... 
*/

typedef enum { storedata, storelocal, storeargs /*, storeglobal*/} StoreType;

/*The instance structures are of the form: */
/* ENSURE DON"T STORE PTRS TO  LOCALS IN OTHERS */
struct Instance_ {
      char             *typename;   /* development only */
      int               typeindex;
      StoreType         storetype;   
      struct Instance_ *prev_vs;  /* for currency handling */ 
      struct Instance_ *next_vs; 
      int               set_node;  /* node of search tree where set  */
      char              prop_set[MAXPROPERTIES]; 
      void *properties[MAXPROPERTIES];     
  };
typedef struct Instance_ Instance;

/* used internally in both preproc and inter */
typedef struct {
        PropDesc     *desc;
        Instance     *container; /*instance containing value */
        int           offset;   /* offset of value within container */
        void         *loc;      /* loc of instance value */
} Ref;

typedef void * (*UserFun)(void *, void* , void *, void *); 


/* declare functions as   */
/* F_(args...)            */ 
typedef struct {
       char      *fn_name;
       UserFun    funptr;
       PropDesc   args[4];
} FnLink;

/*
FnLink ST_FUNCTIONS[] = {

      { "Fn1" ,  Fn1, { ....  } 
       ...       
}
*/

#endif


