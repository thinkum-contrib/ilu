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

#include "convert.h"

jmp_buf ConvertExitEnv;

static Instance *cvt_type(CCtl *ctl, Type t);
static Instance * cvt_exception (CCtl *ctl, Exception e);
static Instance * cvt_constant (CCtl *ctl, Constant c);
static Instance * cvt_constant_value(CCtl *ctl,ConstantValue cv, Type t);
static Instance *cvt_module(CCtl *ctl, Module m); 
void cvt_interface2(CCtl *ctl, Interface i, Instance *new);

/* convert element to generic instance and return ptr */
typedef Instance *(*cvt_EnumProc) (CCtl * ctl, refany cvtin); 
typedef Instance *(*cvt_EnumProc1) (CCtl * ctl, refany cvtin, refany cvtarg); 

extern TypeDesc ST_TYPES[];

extern Module build_basic_module_structure(Interface interface);
 
#define put_field(a, b, c, d) put_by_name((a),(b),(c),(void*)(d)) 


/**********************************************************************/
void do_cvt_error(CvtErrorType errortype, const char *message) {

  char *etype;

  switch (errortype) {
     case cvt_error_system:
          etype = "System error in ilup convert";
          break;
     default:
         /* no other kinds */
         assert(0);
  }

   fprintf(stderr, "\n\n%s: ", etype);
   fprintf(stderr,"%s. \n", message);

   fprintf(stderr,"\nCan not continue ilup conversion.\n");
   longjmp(ConvertExitEnv, 1);

}

/**********************************************************************/
/*        UTILITIES                                                   */
/**********************************************************************/

/* convert to language accepted name by specified callback */
static char *cvt_to_lang_name(CCtl *ctl, char *isl_name) { 
  
   if(ctl->name_convert_fn == NULL) return isl_name;
   return (ctl->name_convert_fn(isl_name)); 
}


/* list dcls from ilu.bison */ 
list cvt_list(CCtl *ctl, list l, cvt_EnumProc cvtproc) {

     list      newlist; 
     listElement *ptr;
     Instance *new;
 
     if(l == NULL || list_size(l) == 0) return NULL;

     newlist = (list)iluparser_new_list(); 
 
     for(ptr = l->head; ptr !=  NULL; ptr = ptr->next) {  
         new = cvtproc(ctl, ptr->data); 
         list_insert(newlist, new); 
     } 
 
     return newlist; 
} 


/* for conversion of list elements where convert requires additional arg*/ 
list cvt_list1(CCtl *ctl, list l, cvt_EnumProc1 cvtproc, refany arg) {
 
     list      newlist;
     listElement *ptr;
     Instance    *new;
 
     if(l == NULL || list_size(l) == 0) return NULL;

     newlist = (list)iluparser_new_list(); 
 
     for(ptr = l->head; ptr !=  NULL; ptr = ptr->next) {  
         new  = cvtproc(ctl, ptr->data, arg); 
         list_insert(newlist, new); 
     } 
     return newlist;
}

/*********************************************************************/
/* Pointer BTree search routine, for mapping iluptype structures      */
/* to generic ones. Returns ptr to btree entry which may be a new    */
/* one if "makenew" specified.  And return value of 0 if none yet    */
/* present, and 1 if already present                                 */
/* Note: extend so same btree structure may hold many logical btrees */
/* at different positions                                            */
/* CHANGE TO HASH                                                  */
int mapbtree_search(CCtl* ctl, void *invalue,
              int makenew, MapBTree **btreeloc, int *exists) {

    void*   stored_value;
    unsigned short int index;
    MapBTree *btree;
    
    index = 1;
   *exists = 0;
   *btreeloc = NULL;
    btree = ctl->btree;

#ifdef DEBUG
    printf("\nbtree input %p", invalue);
#endif

    if(invalue == NULL) return 0;

    /* first access */
    if(ctl->next_btree == 1) { 
        *btreeloc = &(btree[ctl->next_btree]);
         btree[index].map_input = invalue;
         ctl->next_btree++;
         return 1;
    }

     while(1) {
        stored_value = btree[index].map_input; 
#ifdef DEBUG
        printf("\nbtree compare %p", stored_value);
#endif
        if( invalue == stored_value) { 
              *btreeloc = &(btree[index]);
              *exists = 1; 
              return 1;
        }
        else if(  invalue > stored_value) { 
           if(btree[index].larger) 
               { index=btree[index].larger; continue; }
           else if (!makenew) return 0;
           else { btree[index].larger = ctl->next_btree; break; }
         }
         /* invalue must be < stored value */
         else if(btree[index].smaller) 
                 { index=btree[index].smaller; continue; }

         else if (!makenew) return 0;
         else { btree[index].smaller = ctl->next_btree; break; }

     }  /* end while 1 */

    *exists = 0;
     index = ctl->next_btree;
    *btreeloc = &(btree[index]);
     btree[index].map_input = invalue;
     btree[index].smaller = 0;
     btree[index].larger = 0;
     btree[index].map_target = NULL;
      ctl->next_btree++; /* not checking for realloc; will cvt to hash*/
     return 1;
}

/******************************************************************/
/* Given a basic type  kind (get_ST_typename_and_index)           */
/* or a  nontype kind (get_ST_typename_and_index)                 */ 
/* find index and name of of entry in ST_TYPES which              */
/* gives property description                                     */
void get_ST_nontypename_and_index(CCtl *ctl,
           NonTypeKind ntk, char **typename, int *typeindex) { 

#ifdef DEBUG
   printf("\n%d, %s, %d", ntk, ctl->nontype_map[ntk].ST_name,       
          ctl->nontype_map[ntk].ST_index);        
   printf("\n%p", typename); 
#endif

  *typename  = ctl->nontype_map[ntk].ST_name;       
  *typeindex = ctl->nontype_map[ntk].ST_index;        
}


void get_ST_typename_and_index(CCtl *ctl,
           TypeKind tk, char **typename, int *typeindex) { 

#ifdef DEBUG
   printf("\n%d, %s, %d", tk, ctl->type_map[tk].ST_name,       
          ctl->type_map[tk].ST_index);        
   printf("\n%p", typename); 
#endif
  
  *typename = ctl->type_map[tk].ST_name;       
  *typeindex = ctl->type_map[tk].ST_index;        
}

/****************************************************************/
int get_property_kind_and_index(Instance *instance, char *property,
                         PropertyKind *propkind, int *propindex) { 

   int typeind, found, i;
   TypeDesc *typedesc;

   typeind = instance->typeindex;
   typedesc = &(ST_TYPES[typeind]);
    
#ifdef DEBUG
   printf("\n getproperty %d %s", typeind, property); 
#endif
   found = 0;
   for (i = 0;  typedesc->propertydesc[i].propertykind != PropLast; i++) {  
#ifdef DEBUG
         printf("\n %d, %s, %s", i,
             typedesc->propertydesc[i].propertyname, property);
#endif
         if(!strcmp(typedesc->propertydesc[i].propertyname, property)) {
                 found = 1;
                 break; 
        }
    }

   if(found) { 
       *propkind = typedesc->propertydesc[i].propertykind;
       *propindex = i;
        return 1;
   }
   else return 0;
}

/****************************************************************/

Instance *new_instance(CCtl *ctl) {

   Instance *new; 
    
   new = (void *)calloc(1, sizeof(Instance));
   return new;

}



/****************************************************************/
/* See if instance equivalent of input already defined. If so,   */
/* just return it.  If not, create new generic instance          */ 
/* and mapping from iluptype ptr to new instance.                */ 
/* If input pointer is NULL, not an error                        */
Instance *new_generic_instance ( CCtl *ctl, void *ilup_ptr,
            int* already_exists) { 

   Instance  *instance;
   MapBTree  *btreeloc;

  if(ilup_ptr == NULL) {
       *already_exists = 1;
        return NULL;
   }

   mapbtree_search(ctl, (void *)ilup_ptr,
         1, &btreeloc, already_exists);
   if(!(*already_exists)) { 
       instance = new_instance(ctl);   
       btreeloc->map_target = (void *)instance;
   }
    return btreeloc->map_target;
 
}

/****************************************************************/
void put_by_name(CCtl *ctl, Instance *instance, char *prop,
             void *value){

   PropertyKind propkind;
   int          propindex;
   char         msg[256];

   
   if(! get_property_kind_and_index( instance, 
                              prop, &propkind, &propindex)){ 
       sprintf(msg, "Property %s of type %s not found", prop,
              instance->typename); 
       do_cvt_error(cvt_error_system, msg);
    }

    /* long ints must already be refd by ptr */ 
    instance->properties[propindex] = value;
 }   

/* add indicator for a scope list member that represents a true class */
static void mark_scope_list(CCtl *ctl, list scope_list) { 
  listElement *ptr, *next_elem;   
  Instance    *class_name_mbr;

  /* indicate that next to last in scope list is true class */
  /* needed for stubber naming                              */
  for(ptr = scope_list->head; ptr; ptr= ptr->next) {  
     next_elem = ptr->next;
     assert(next_elem != NULL);
     if(next_elem->next == NULL) {
        class_name_mbr = (Instance *)ptr->data;
        put_by_name(ctl, class_name_mbr, "is_real_class", (void *)1);
        break; 
     }
  }

 }

/*********************************************************************/
/* add indicators for a scope list members that represent true classes */
static void mark_scope_lists_for_CORBA_nested_types(CCtl* ctl,
                               Module m) {

  int           propindex, exists;
  Instance     *this; 
  char          msg[100]; 
  listElement  *ptr; 
  PropertyKind  propkind;

  if(m->contained_types != NULL && m->contained_types->head != NULL) {
     for(ptr = m->contained_types->head; ptr; ptr = ptr->next) { 
       this = new_generic_instance( ctl, (Instance*)ptr->data, &exists); 
       if(!get_property_kind_and_index(this,"scoping",&propkind, &propindex)){ 
         sprintf(msg, "Property %s of type %s not found", "scoping",
                           this->typename);
         do_cvt_error(cvt_error_system, msg);
       }
       mark_scope_list(ctl, (list) this->properties[propindex]);
     }  
   }
  if(m->contained_exceptions != NULL
          && m->contained_exceptions->head != NULL) {
     for(ptr = m->contained_exceptions->head; ptr; ptr = ptr->next) { 
       this = new_generic_instance( ctl, (Instance*)ptr->data, &exists); 
       if(!get_property_kind_and_index( this,"scoping",&propkind, &propindex)){
         sprintf(msg, "Property %s of type %s not found", "scoping",
                           this->typename);
         do_cvt_error(cvt_error_system, msg);
       }
       mark_scope_list(ctl, (list) this->properties[propindex]);
     }
  }
  if(m->contained_constants != NULL
          && m->contained_constants->head != NULL) {
     for(ptr = m->contained_constants->head; ptr; ptr = ptr->next) { 
       this = new_generic_instance( ctl, (Instance*)ptr->data, &exists); 
       if(!get_property_kind_and_index(this,"scoping",&propkind, &propindex)){
        sprintf(msg, "Property %s of type %s not found", "scoping",
                           this->typename);
        do_cvt_error(cvt_error_system, msg);
        }
        mark_scope_list(ctl, (list) this->properties[propindex]);
     }
   }
 } 



/******************************************************************/
/* name conversions                                               */
/******************************************************************/

static char *last_in_scoping(list scoping) { 
   char       *return_value;   
   listElement *ptr;

   if(scoping == NULL || scoping->head == NULL) return NULL;
   for(ptr = scoping->head; ptr != NULL; ptr = ptr->next) {
        if(ptr->next == NULL) break; 
   }
   return_value = (char *)ptr->data;
   return return_value;
}


/* For conversion of non-scoped names, e.g., methods, args        */
/*  -store given name in orig_prop                                */
/* -convert name according to language and store in nameprop      */
void put_name_fields(CCtl *ctl, Instance *instance, char *name_prop,
                     char *orig_prop, char *value) {
   char *temp;

   put_field(ctl, instance, orig_prop, value);

   temp  = cvt_to_lang_name(ctl, value);

   put_field(ctl, instance, name_prop, temp);
}    

/* For conversion of scoped names                                        */
/* -store given name in orig_prop  (e.g. isl_name)                       */
/* -convert name according to lang, store in flat_prop (e.g., flat_name) */
/* -store last of scoping in nameprop   (e.g. name)                      */ 
void put_scoped_name_fields(CCtl *ctl, Instance *instance,
        char *name_prop, char *flat_prop, char *orig_prop, char *value,
       list scoping) {
   char *temp, *temp1, *temp2;

   put_field(ctl, instance, orig_prop, value);

   temp  = cvt_to_lang_name(ctl, value);
   put_field(ctl, instance, flat_prop, temp);

   temp1 = last_in_scoping(scoping); 
   if(temp1 == NULL || (!strcmp(temp1, value)))
          temp2 = temp;
   else temp2 = cvt_to_lang_name(ctl, temp1);
   put_field(ctl, instance, name_prop, temp2);
        
}    

/*********************************************************************/
void *get_by_name( Instance *instance, char *property) {

   PropertyKind propkind;
   int          propindex;
   char         msg[256];

   if(! get_property_kind_and_index( instance, 
                                   property, &propkind, &propindex)){ 
        sprintf(msg, "Missing property %s.", property); 
        do_cvt_error(cvt_error_system, msg);
    }

    return(instance->properties[propindex]); 
}


/*********************************************************************/
void *get_by_index (Instance *instance, int prop_index, void *value) {

     return(instance->properties[prop_index]); 
}

/***************************************************************/
/*       INITIALIZATION                                        */
/***************************************************************/

/***************************************************************/
/* FIX BUILTINS TO CREATE GENERIC INSTANCES...                 */
/**************************************************************/
/* Put mapping from iluptype primitive type enum values  to   */ 
/*   ST_TYPES indices in mapping btree                        */ 
/* Build Instance entries for builtin types                   */
 
void initialize_ilup_correspondences ( CCtl* ctl) { 

  int i,j, found;
  TypeKind typekind;
  NonTypeKind nontypekind; 
  char        msg[256];

  MapTypeKind  builtins[]= { 
      { byte_Type, "Byte"},
      { boolean_Type, "Boolean"},
      { character_Type,"Character"},
      { shortcharacter_Type, "ShortCharacter"},
      { shortinteger_Type, "ShortInteger"},
      { integer_Type, "Integer"},
      { longinteger_Type, "LongInteger"},
      { shortcardinal_Type, "ShortCardinal"},
      { cardinal_Type, "Cardinal"},
      { longcardinal_Type, "LongCardinal"},
      { real_Type, "Real"},
      { shortreal_Type, "ShortReal"},
      { longreal_Type, "LongReal"},
      { pickle_Type, "Pickle"},
      { void_Type, "Void"},
      { invalid_Type, NULL}
  };

  MapTypeKind nonbuiltins[] = {
     {record_Type, "Record"}, 
     {string_Type, "StringType"},
     {object_Type, "Object"},
     {array_Type, "Array"},
     {sequence_Type, "Sequence"},
     {enumeration_Type, "Enumeration"},
     {fixedpoint_Type, "FixedPoint"},
     {alias_Type, "Alias"},
     {pickle_Type, "Pickle"},
     {union_Type, "Union"},
     {optional_Type, "Optional"},
     {reference_Type, "Reference"},
     {pipe_Type, "Pipe"},
     {invalid_Type, NULL},
  };

  MapNonTypeKind nontypes[] = { 
    {interface_NonType, "Interface"},
    {module_NonType,    "Module"},
    {content_elem_NonType, "ContentElem"},
    {method_NonType, "Method"},
    {argument_NonType, "Argument"},
    {arm_NonType, "Arm"}, 
    {exception_NonType, "Exception"},
    {imported_NonType, "Imported"},
    {enumfield_NonType, "EnumField"},
    {constant_NonType, "Constant"},
    {integer_constantval_NonType, "IntConstantValue"},
    {boolean_constantval_NonType, "BoolConstantValue"},
    {real_constantval_NonType, "RealConstantValue"},
    {ilucstring_constantval_NonType, "ILUCStringConstantValue"},
    {enum_constantval_NonType, "EnumConstantValue"},
    {integerliteral_NonType, "IntegerLiteral"},
    {diminteger_NonType, "DimInteger"},   /* for dims */
    {name_mbr_NonType, "NameMember"},   /* for scope name lists */
    {statembr_NonType, "StateMbr"},
    {langname_NonType, "LangName"},
    {invalid_NonType, NULL},
  };
       
  /* develop map between types/nontypes and their ST_Type structures */ 
  /* and initialize entries for built-ins                            */
  for (i = 1; strcmp(ST_TYPES[i].typename,"LastTypeDesc"); i++) {

#ifdef DEBUG
   printf("\n%s",ST_TYPES[i].typename);
#endif

         found = 0;
         for(j = 0; builtins[j].typekind != invalid_Type; j++) {  
            if(!strcmp(ST_TYPES[i].typename, builtins[j].ST_name)) {
                typekind = builtins[j].typekind;
                ctl->type_map[typekind].thisindex = typekind; 
                ctl->type_map[typekind].ST_name = builtins[j].ST_name; 
                ctl->type_map[typekind].ST_index = i; 
                found = 1;
                break;
            }
         }
          
        if(found) continue; 
         for(j = 0; nonbuiltins[j].typekind != invalid_Type; j++) {  
             if(!strcmp(ST_TYPES[i].typename, nonbuiltins[j].ST_name)) { 
                typekind = nonbuiltins[j].typekind;
                ctl->type_map[typekind].thisindex = typekind; 
                ctl->type_map[typekind].ST_name = nonbuiltins[j].ST_name;
                ctl->type_map[typekind].ST_index = i; 
                found = 1;
                break;
             }
        }

       if(found) continue;
       for(j=0; nontypes[j].nontypekind != invalid_NonType; j++) { 
             if(!strcmp(ST_TYPES[i].typename, nontypes[j].ST_name)) {
                nontypekind = nontypes[j].nontypekind;
                ctl->nontype_map[nontypekind].thisindex = nontypekind; 
                ctl->nontype_map[nontypekind].ST_name = nontypes[j].ST_name;
                ctl->nontype_map[nontypekind].ST_index = i; 
                found = 1;
                break; 
           }
       }

       if(found) continue;

       sprintf(msg, "ST_TYPES typename %s unknown", ST_TYPES[i].typename);
       do_cvt_error(cvt_error_system, msg);

   }
 return; /* SHOULD NEVER GET HERE */  

}

            
/*****************************************************************/
/*     CONVERSION FUNCTIONS FOR NON_TYPES                        */
/****************************************************************/

static Instance *cvt_dim(CCtl* ctl, int i) {

   Instance *new;

   new = new_instance(ctl);
   get_ST_nontypename_and_index(ctl, diminteger_NonType, &new->typename,
                   &new->typeindex);
   put_field(ctl, new, "intvalue", i);
   return(new);
}


static Instance *cvt_name_mbr(CCtl* ctl, char *name) {

   Instance *new;
   char     *temp;

   new = new_instance(ctl);
   get_ST_nontypename_and_index(ctl, name_mbr_NonType, &new->typename,
                   &new->typeindex);

   put_field(ctl, new, "isl_name", name);

   temp =  cvt_to_lang_name(ctl, name); 

   put_field(ctl, new, "namevalue", temp);

   return(new);
}

/***********************************************************/
/* Not used.   convert produce name and isl_name fields   */
/* where name field is language specific                  */
static Instance *cvt_langname(CCtl *ctl, usagename u) {  

   Instance *new;

   new = new_instance(ctl);
   get_ST_nontypename_and_index(ctl, langname_NonType, &new->typename,
                   &new->typeindex);
   put_field(ctl, new, "language", u->lang);
   put_field(ctl, new, "name", u->name);
   return(new);
}



/***********************************************************/
static Instance *cvt_enum_field(CCtl *ctl, EnumField f) {

  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, f, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, enumfield_NonType,
                           &new->typename, &new->typeindex);

  put_name_fields(ctl, new, "name", "isl_name", f->name);
  put_field(ctl, new, "id", f->id); 

  return new;
}

/***********************************************************/
static Instance *cvt_integer_literal(CCtl *ctl, IntegerLiteral i) {

  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, i, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, integerliteral_NonType,
                           &new->typename, &new->typeindex);

  put_field(ctl, new, "small", i->small);
  put_field(ctl, new, "negative", i->negative); 
  /* next two are unioned in iluptype....*/
  put_field(ctl, new, "direct",  &(i->val.direct));
  put_field(ctl, new, "string",  i->val.string);

  return new;
}

/***********************************************************/
static Instance *cvt_imported(CCtl *ctl, Imported i) {

  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, i, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, imported_NonType,
                           &new->typename, &new->typeindex);

  put_name_fields(ctl, new, "name", "isl_name", i->name);
  put_field(ctl, new, "filename", i->filename); 

  return new;
}


static Instance * cvt_statembr(CCtl *ctl,  Argument a) {
  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, a, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, statembr_NonType,
                           &new->typename, &new->typeindex);


  put_name_fields(ctl, new, "name", "isl_name",name_base_name(a->name));
  put_field(ctl, new, "ilup_ref", a);
  put_field(ctl, new, "private", a->sibling); 
  put_field(ctl, new, "type", cvt_type(ctl, a->type));
  return new;
}


/***********************************************************/
static Instance * cvt_argument(CCtl *ctl,  Argument a) {

  Instance *new;
  char     *direction;   
  int       exists;

  new = new_generic_instance(ctl, a, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, argument_NonType,
                           &new->typename, &new->typeindex);

  /* maybe should add enum type to defs */
  switch(a->direction) {
       case In:    direction = "In";    break;
       case Out:   direction = "Out";   break;
       case InOut: direction = "InOut"; break;
   }

  put_field(ctl, new, "direction", direction);  
  put_name_fields(ctl, new, "name", "isl_name",name_base_name(a->name));
  put_field(ctl, new, "ilup_ref", a);
  put_field(ctl, new, "sibling", a->sibling); 
  put_field(ctl, new, "type", cvt_type(ctl, a->type));
  return new;
}


/************************************************************/
static Instance *cvt_content_elem(CCtl *ctl, ContentElem ce) {

  Instance *new; 
  int       exists;

  new = new_generic_instance(ctl, ce, &exists); 
  if(exists) return (new);

  get_ST_nontypename_and_index(ctl, content_elem_NonType, 
                           &new->typename, &new->typeindex);

  put_field(ctl, new, "content_type", ce->content_type);
  if(!strcmp(ce->content_type,"module")) 
       put_field(ctl, new, "content", cvt_module(ctl, (Module)ce->content)); 
  else put_field(ctl, new, "content", cvt_type(ctl, (Type)ce->content));
  
  return new;
 } 

/*************************************************/
static Instance *cvt_module(CCtl *ctl, Module m) { 

  Instance    *new, *equiv_type; 
  int          exists;

  new = new_generic_instance(ctl, m, &exists); 
  if(exists) return (new);

  get_ST_nontypename_and_index(ctl, module_NonType, 
                           &new->typename, &new->typeindex);

  put_name_fields(ctl, new, "simple_name", "isl_name", m->simple_name);
  put_field(ctl, new, "scoping", 
           cvt_list(ctl, m->scoping, (cvt_EnumProc)cvt_name_mbr)); 
  put_field(ctl, new, "contained_types",
       cvt_list(ctl, m->contained_types, (cvt_EnumProc)cvt_type)); 
  put_field(ctl, new, "contained_exceptions",
     cvt_list(ctl, m->contained_exceptions, (cvt_EnumProc)cvt_exception)); 
  put_field(ctl, new, "contained_constants",
     cvt_list(ctl, m->contained_constants, (cvt_EnumProc)cvt_constant)); 
  put_field(ctl, new, "contained_modules", 
     cvt_list(ctl, m->contained_modules, (cvt_EnumProc)cvt_module)); 
  put_field(ctl, new, "content_sequence",
     cvt_list(ctl, m->content_sequence,  (cvt_EnumProc)cvt_content_elem));
  put_field(ctl, new, "module_object", cvt_type(ctl, m->module_object));

  /* If module is dummy.. really corba object with nested types */
  /* Add indicator to scoping lists for contained types         */
  /* that last container is real class                         */ 
  if(m->module_object != NULL ) {
           /* equiv type must already exist*/
            equiv_type = new_generic_instance(
                         ctl, m->module_object, &exists); 
           put_field(ctl, equiv_type, "equiv_module", new); 
           mark_scope_lists_for_CORBA_nested_types(ctl, m);
   }

  return new;
}

  
/***********************************************************/
void cvt_interface2(CCtl *ctl, Interface i, Instance *new) {

  get_ST_nontypename_and_index(ctl, interface_NonType,
                           &new->typename, &new->typeindex);

  if( (i->module_structure == NULL) && 
       strcmp(name_base_name(i->name) , "ilu"))  
         build_basic_module_structure(i);
  if(i->module_structure != NULL) /* if not ref to ilu */
     put_field(ctl, new, "module_str",cvt_module(ctl,i->module_structure));

  put_name_fields(ctl, new, "name", "isl_name", name_base_name(i->name));
  put_field(ctl, new, "brand", i->brand); 
  put_field(ctl, new, "ilup_ref", i);
  put_field(ctl, new, "types", cvt_list(ctl, i->types,
                  (cvt_EnumProc)cvt_type)); 
  put_field(ctl, new, "classes", cvt_list(ctl, i->classes,
                 (cvt_EnumProc) cvt_type)); 
  put_field(ctl, new, "exceptions",
              cvt_list(ctl, i->exceptions, (cvt_EnumProc)cvt_exception)); 
  put_field(ctl, new, "imports", cvt_list(ctl, i->imports,
                 (cvt_EnumProc)cvt_imported)); 
  put_field(ctl, new, "constants",cvt_list(ctl, i->constants,
                  (cvt_EnumProc)cvt_constant)); 
  put_field(ctl, new, "filename", i->filename);
}


/************************************************************/
Instance * cvt_interface(CCtl *ctl, Interface i) {

  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, i, &exists); 

  if(exists) return new;
   
  cvt_interface2(ctl, i, new);
  return new;

}

/***********************************************************/
/* returns list of converted interface structures          */ 
 list cvt_interfaces(CCtl* ctl, list ilup_top) { 
     list         return_list;
     listElement *ptr;  
     Instance    *cvted_intf;
 
    if(setjmp(ConvertExitEnv) != 0)  {
        return NULL;
    }
 
     return_list = new_list();
 
     for(ptr = ilup_top->head; ptr; ptr= ptr->next) {
       cvted_intf = cvt_interface(ctl, (Interface)ptr->data);
       list_insert(return_list, cvted_intf); 
     }

     return return_list;  
}
 


/***********************************************************/
static Instance * cvt_arm(CCtl *ctl,  Argument a, Type disc_type) {

  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, a, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, arm_NonType,
                           &(new->typename), &(new->typeindex));

  put_name_fields(ctl, new, "name", "isl_name", name_base_name(a->name));
  put_field(ctl, new, "ilup_ref", a);
  /* assume for now that arm type cannot be object */
  put_field(ctl, new, "type", cvt_type(ctl, a->type));
  put_field(ctl, new, "values",
     cvt_list1(ctl, a->values,(cvt_EnumProc1)cvt_constant_value, disc_type));
  return new;
}

/***********************************************************/
static Instance * cvt_method (CCtl *ctl, Procedure p) 
{
  Instance *new;
  int       exists;

  new = new_generic_instance(ctl, p, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl,  method_NonType,
                           &new->typename, &new->typeindex);

  put_name_fields(ctl, new, "name", "isl_name",name_base_name(p->name));
  put_field(ctl, new, "ilup_ref", p);
  put_field(ctl, new, "returntype",cvt_type(ctl, p->returnType));
  put_field(ctl, new, "returnoptional", p->returnOptional);
  put_field(ctl, new,  "protocolid", p->id);
  put_field(ctl, new, "ofobject", cvt_type(ctl, p->object));
  put_field(ctl, new, "ofinterface", cvt_interface(ctl, p->interface));
  put_field(ctl, new, "functional", p->functional);
  put_field(ctl, new, "asynch", p->asynch);
  put_field(ctl, new, "authentication_type", p->authentication_type);
  put_field(ctl, new, "doc_string", p->doc_string);

  put_field(ctl, new, "arguments",cvt_list(ctl, p->arguments,
                 (cvt_EnumProc) cvt_argument)); 

  put_field(ctl, new, "exceptions",
              cvt_list(ctl, p->exceptions, (cvt_EnumProc)cvt_exception)); 

  return new;
}


/***********************************************************/
static Instance * cvt_exception (CCtl *ctl, Exception e) {

  Instance *new, *e1;
  int       exists;

  new = new_generic_instance(ctl, e, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, exception_NonType,
                           &new->typename, &new->typeindex);

  put_scoped_name_fields(ctl, new, "name", "flat_name", "isl_name",
        name_base_name(e->name), e->scoping);
  put_field(ctl, new, "ilup_ref", e);
  put_field(ctl, new, "scoping", 
           cvt_list(ctl, e->scoping, (cvt_EnumProc)cvt_name_mbr)); 
  put_field(ctl, new, "type", cvt_type(ctl, e->type));  
  put_field(ctl, new, "valueoptional", e->valueOptional);
  put_field(ctl, new, "builtin", e->builtIn); 
  put_field(ctl, new, "protocolid", e->id);
  put_field(ctl, new, "importedfrom", e->importInterfaceName); 
  if(e->import != NULL){
          e1 = cvt_exception(ctl, e->import); 
          put_field(ctl, new, "importedexception",e1); 
  }
  put_field(ctl, new, "interface", cvt_interface(ctl, e->interface));
  put_field(ctl, new, "corba_rep_id", e->corba_rep_id);
  put_field(ctl, new, "doc_string", e->doc_string);  

  return new; 

}

/***********************************************************/
static Instance * cvt_constant (CCtl *ctl, Constant c) {

  Instance *new, *c1;
  int       exists;

  new = new_generic_instance(ctl, c, &exists); 
  if(exists) return new;

  get_ST_nontypename_and_index(ctl, constant_NonType,
                           &new->typename, &new->typeindex);

  put_scoped_name_fields(ctl, new, "name", "flat_name", "isl_name",
        name_base_name(c->name), c->scoping);
  put_field(ctl, new, "scoping",
              cvt_list(ctl, c->scoping, (cvt_EnumProc)cvt_name_mbr)); 
  put_field(ctl, new, "ilup_ref", c);
  put_field(ctl, new, "type", cvt_type(ctl, c->type));  
  put_field(ctl, new, "importedfrom", c->importInterfaceName); 
  if(c->import != NULL) {
             c1 = cvt_constant(ctl, c->import); 
             put_field(ctl, new, "importedconstant",c1);
  }
  put_field(ctl, new, "interface", cvt_interface(ctl, c->interface));
  put_field(ctl, new,  "value", cvt_constant_value(ctl, c->value, c->type));
  return new; 

}


/***********************************************************/
/* TBD: check if asserts adequate                          */
static Instance * cvt_constant_value(CCtl *ctl,
                   ConstantValue cv, Type t) { 
  Instance *new;
  int       exists;

  TypeKind ctk, vtk;

   new = new_generic_instance(ctl, cv, &exists); 
   if(exists) return new;

   ctk  = type_ur_kind(t);  
   vtk =  cv->type;

   switch (ctk) {

      case byte_Type:
      case integer_Type:  case shortinteger_Type:  case longinteger_Type:     
      case cardinal_Type: case shortcardinal_Type: case longcardinal_Type:  
 
         assert(vtk == integer_Type);

         get_ST_nontypename_and_index(ctl,  integer_constantval_NonType,
                           &new->typename, &new->typeindex);
         put_field(ctl, new, "positive",  cv->val.i.sign);
         put_field(ctl, new, "value", &(cv->val.i.value));
         break;

      case sequence_Type:  

         assert(vtk == shortcharacter_Type); 
         get_ST_nontypename_and_index(ctl, ilucstring_constantval_NonType,
                           &new->typename, &new->typeindex);
         put_field(ctl, new, "value",  cv->val.s);
         break;

      case enumeration_Type:

         assert(vtk == shortcharacter_Type); 
         get_ST_nontypename_and_index(ctl, enum_constantval_NonType,
                           &new->typename, &new->typeindex);
         put_field(ctl, new, "value",  cv->val.s);
         break;

      case boolean_Type:

         assert(vtk == boolean_Type);
         get_ST_nontypename_and_index(ctl, boolean_constantval_NonType,
                           &new->typename, &new->typeindex);
         put_field(ctl, new, "value",  cv->val.b);
         break;

      case real_Type:  case shortreal_Type:  case longreal_Type:     

           if(vtk == integer_Type) {
               get_ST_nontypename_and_index(ctl,integer_constantval_NonType,
                           &new->typename, &new->typeindex);
                put_field(ctl, new, "positive",  cv->val.i.sign);
                put_field(ctl, new, "value", cv->val.i.value);
          }

          else {
               assert(vtk != integer_Type);
               get_ST_nontypename_and_index(ctl, real_constantval_NonType,
                           &new->typename, &new->typeindex);
               put_field(ctl, new, "positive",     cv->val.r.sign);
               put_field(ctl, new, "value",    cv->val.r.value);
               put_field(ctl, new, "fraction", cv->val.r.fraction);
               put_field(ctl, new, "exponent", &(cv->val.r.exponent));
          }
          break;
      default: assert(0);
 }    

 return new; 
}



/*****************************************************************/
/*     CONVERSION FUNCTIONS FOR TYPES                            */
/*  (conversions begun in "cvt_type", and then  completed        */
/*   in "complete_xxx_type"                                      */ 
/*****************************************************************/

static void complete_record_type(CCtl *ctl, Instance *new, 
               TypeDescription d) {

    put_field(ctl, new, "extensible", d->structuredDes.record.extensible);
    put_field(ctl, new, "fields",
         cvt_list(ctl, d->structuredDes.record.fields, 
               (cvt_EnumProc)cvt_argument)); 

}


/*****************************************************************/
static void complete_union_type(CCtl *ctl, Instance *new,
               TypeDescription d) {

   UnionDescription *u;

   u = &(d->structuredDes.uniond);

   put_field(ctl, new, "discrim_type",
               cvt_type(ctl, u->discriminator_type)); 

   put_field(ctl, new, "default_arm",
               cvt_arm(ctl, u->default_arm, u->discriminator_type));
   put_field(ctl, new, "others_allowed", u->others_allowed);
   put_field(ctl, new, "arms",
              cvt_list1(ctl,  u->types, (cvt_EnumProc1)cvt_arm,
                            u->discriminator_type)); 

}


/*****************************************************************/
static void complete_array_type(CCtl *ctl, Instance *new, 
               TypeDescription d) {

     put_field(ctl, new, "oftype", cvt_type(ctl, d->structuredDes.array.type));

     put_field(ctl, new, "optional", d->structuredDes.array.optional);

    /* LIST OF INT */
     put_field(ctl, new, "dimensions",   
        cvt_list(ctl, d->structuredDes.array.dimensions,
                 (cvt_EnumProc) cvt_dim));

}


/*****************************************************************/
static void complete_enumeration_type(CCtl *ctl, Instance *new,
             TypeDescription d) {

    put_field(ctl, new, "values",
           cvt_list(ctl, d->structuredDes.enumeration,
                   (cvt_EnumProc)cvt_enum_field));

}


/*****************************************************************/
static void complete_optional_type(CCtl *ctl, Instance *new,
             TypeDescription d) {

    put_field(ctl, new, "oftype",
               cvt_type(ctl, d->structuredDes.optional));
}


/*****************************************************************/
static void complete_reference_type(CCtl *ctl, Instance *new,
             TypeDescription d) {

      put_field(ctl, new, "optional", d->structuredDes.reference.optional);
      put_field(ctl, new, "aliased", d->structuredDes.reference.aliased);
      put_field(ctl, new, "base_type",
             cvt_type(ctl, d->structuredDes.reference.base_type));
}
   

/*****************************************************************/
static void complete_string_type(CCtl *ctl, Instance *new,
             TypeDescription d) {

   put_field(ctl, new, "max_length",&(d->structuredDes.string.max_length));
   put_field(ctl, new, "charset", d->structuredDes.string.charset);
   put_field(ctl, new, "language", d->structuredDes.string.language);

}


/*****************************************************************/
static void complete_alias_type(CCtl *ctl, Instance *new, Type t) {

   put_field(ctl, new, "oftype", cvt_type(ctl, t->supertype));

}


/*****************************************************************/
static void complete_sequence_type(CCtl *ctl, Instance *new, 
             TypeDescription d) {

   put_field(ctl, new, "optional", d->structuredDes.sequence.optional);
   put_field(ctl, new, "limit", &(d->structuredDes.sequence.limit));
   put_field(ctl, new, "oftype", 
         cvt_type(ctl, d->structuredDes.sequence.type));

}


/*****************************************************************/
static void complete_fixedpoint_type(CCtl *ctl, Instance * new,
             TypeDescription d) {

   put_field(ctl, new, "min_numerator",
          cvt_integer_literal(ctl, d->structuredDes.fixed.min_numerator));
   put_field(ctl, new, "max_numerator",
          cvt_integer_literal(ctl, d->structuredDes.fixed.max_numerator));
   put_field(ctl, new, "denominator",
          cvt_integer_literal(ctl, d->structuredDes.fixed.denominator));
   put_field(ctl, new, "fixed_digits",
               d->structuredDes.fixed.fixed_digits);
   put_field(ctl, new, "fixed_decimal_places",
               d->structuredDes.fixed.fixed_decimal_places);

   /* these are enums */
   put_field(ctl, new, "range_size", d->structuredDes.fixed.range_size);

}

 
/*****************************************************************/
static void complete_object_type(CCtl *ctl, Instance *new,
             TypeDescription d) {
  Class od;

  od = d->structuredDes.object;

  put_field(ctl, new, "brand", od->brand);
  put_field(ctl, new, "singleton", od->singleton);
  put_field(ctl, new, "collectible", od->collectible);
  put_field(ctl, new, "optional", od->optional);
  put_field(ctl, new, "authentication", od->authentication);
  put_field(ctl, new, "corba_rep_id", od->corba_rep_id);
/* not initialized by ilu.bison
  put_field(ctl, new, "sealed", od->sealed);
*/
  put_field(ctl, new, "local",  od->local);
 
  put_field(ctl, new, "superclasses",  
           cvt_list(ctl, od->superclasses, (cvt_EnumProc)cvt_type));

  put_field(ctl, new, "methods",
        cvt_list(ctl, od->methods, (cvt_EnumProc)cvt_method));

/* not initialized by ilu.bison
  put_field(ctl, new, "state",
        cvt_list(ctl, od->state, (cvt_EnumProc)cvt_statembr));
*/


  put_field(ctl, new, "doc_string", od->doc_string);

}


/*****************************************************************/
static Instance *cvt_type(CCtl *ctl, Type t) {

 TypeDescription d;
 TypeKind fundamental_kind;
 Instance *new;
 int       exists;

#ifdef DEBUG
  printf("\ncvt type %p", t);
#endif

  if(t == NULL) return NULL;
  new = new_generic_instance(ctl, t, &exists); 
#ifdef DEBUG
  printf("\nexists %d", exists);
#endif

  if(exists) return new;

  fundamental_kind = type_ur_kind(t);

  /* TBD: unscramble what do here for imports */
  get_ST_typename_and_index(ctl, fundamental_kind,
                           &new->typename, &new->typeindex);

#ifdef DEBUG
  printf("\nType %s, typeindex %d", new->typename, new->typeindex);
#endif
  
  new->storetype = storedata;

 /*TBD.. something about cached descriptions */  
  put_scoped_name_fields(ctl, new, "name", "flat_name", "isl_name",
        name_base_name(t->name), t->scoping);
  put_field(ctl, new, "supertype", cvt_type(ctl, t->supertype));

  put_field(ctl, new, "scoping", 
           cvt_list(ctl, t->scoping, (cvt_EnumProc)cvt_name_mbr)); 
  put_field(ctl, new, "builtin", t->builtIn); 
  put_field(ctl, new, "importedfromname", t->importInterfaceName);
  put_field(ctl, new, "typeininterface",
          cvt_interface(ctl, t->interface));
  put_field(ctl, new, "uid", t->uid); 
  put_field(ctl, new, "explicit_uid", t->explicit_uid); 
  put_field(ctl, new, "brand", t->brand); 
  put_field(ctl, new, "ilup_ref", t);

  switch(fundamental_kind) {  
     case void_Type:          case byte_Type:          case boolean_Type:
     case character_Type:     case shortcharacter_Type:
     case shortinteger_Type:  case integer_Type:       case longinteger_Type:
     case shortcardinal_Type: case cardinal_Type:      case longcardinal_Type:
     case real_Type:          case shortreal_Type:     case longreal_Type:
     case pickle_Type:
         return new; 
     default: break;
 }
      
  if(t->supertype != NULL) return new;

  d = type_description(t);

  switch (fundamental_kind) {

    case record_Type:
         complete_record_type(ctl, new, d);
         break;
    case union_Type:
         complete_union_type(ctl, new, d);
         break;
    case array_Type:
         complete_array_type(ctl, new, d);
         break;
    case enumeration_Type:
         complete_enumeration_type(ctl, new, d);
         break; 
    case optional_Type:
         complete_optional_type(ctl, new, d);
         break;
    case reference_Type:
         complete_reference_type(ctl, new, d);
         break; 
    case string_Type:
         complete_string_type(ctl, new, d);
         break; 
    case alias_Type:
         complete_alias_type(ctl, new, t);
         break; 
    case sequence_Type:
         complete_sequence_type(ctl, new, d);
         break; 
    case fixedpoint_Type:
         complete_fixedpoint_type(ctl, new, d);
         break; 
    case object_Type:
         complete_object_type(ctl, new, d);
         break; 
    default:  
         do_cvt_error(cvt_error_system,"Unknown ilup type.");
  }
  return new;

}

/********************************************************************/
/* argument specifies callback to convert to language-specific names */
CCtl * initialize_convert(NameCvt *cvtf ) {

    CCtl *ctl;
    ctl = (void *)calloc(1, sizeof(CCtl));  
    ctl->btree_alloc = INITIAL_BTREE_ALLOC; 
    ctl->next_btree = 1;
    ctl->btree = (void *)calloc(INITIAL_BTREE_ALLOC, sizeof(MapBTree));

    if(cvtf != NULL) ctl->name_convert_fn = cvtf; 

    initialize_ilup_correspondences (ctl); 

    return ctl;
}



/************************************************************************/
/* Print routines for generic structures                                */ 
/************************************************************************/

static void get_single_prop_value(Instance *instance, char buf[]) { 

   int          typeind;
   TypeDesc    *td;
   char        *outstring;
   long int    thelong;

   typeind = instance->typeindex;
   td = &(ST_TYPES[typeind]);


   /* must be integer or string */
    switch(td->propertydesc[0].propertykind) {

           case PropString:

              outstring = (char *)(instance->properties[0]); 
              if(outstring == NULL) outstring = "NULL";
              sprintf(buf,"%s", (char *)instance->properties[0]);  
              break; 

           case PropBool:
           case PropInt:

              sprintf(buf,"%d", (int)instance->properties[0]);
              break;

           case PropStruct:
           case PropList:
           case PropLocals:
           case PropArgs:
           case PropLast:
               do_cvt_error(cvt_error_system,
                     "\nBad type for single prop value print"); 

           case PropLongInt:
              thelong = *(long int*)instance->properties[0]; 
              sprintf(buf,"%ld", thelong);
              break;

           case PropOpaque:
              sprintf(buf,"%p", (void *)instance->properties[0]);
              break;

           case PropNull:  /* should not occur */
              sprintf(buf,"%s", "NULL");  
              break; 
   }
         
}

/***********************************************************************/
/* FIX THESE FOR EFFICIENCY */
#define MAX_SUBORDS_TYPES 30
static int in_subords_print(char *ref_type) {
  
  int i;
  char *subords[MAX_SUBORDS_TYPES] =
    {"Method", "Argument", "Arm", "Imported", /* ? */
     "EnumField","IntConstantValue", "BoolConstantValue",
     "RealConstantValue", "ILUCStringConstantValue",
     "EnumConstantValue", "IntegerLiteral", "StateMbr",
     "LangName", "LastType"};

  for(i = 0; strcmp(subords[i], "LastType"); i++) {
    if(!strcmp(subords[i], ref_type)) return 1; 
  } 
  return 0;

 }


/***********************************************************************/
#define MAX_IMMEDIATE_TYPES 30
static int in_immediate_print(char *ref_type) {

  int i;
  char *immediates[MAX_IMMEDIATE_TYPES] =
      {"DimInteger", "NameMember", "LastType"};

  for(i = 0; strcmp(immediates[i], "LastType"); i++) {
    if(!strcmp(immediates[i], ref_type)) return 1; 
  } 
  return 0;

 }


/***********************************************************************/
static int get_print_number(CCtl * ctl, Instance *instance){

   int i;
   for(i = 0; i < ctl->toprint_ct; i ++) { 

     if(instance == ctl->toprint[i]) return i;

   }   

   ctl->toprint[ctl->toprint_ct] = instance; 
   (ctl->toprint_ct)++; 

   return( ctl->toprint_ct -1);
}

#define MAX_PRINT_SUBORDS 500
/***********************************************************************/
static void get_print_ref(CCtl *ctl, Instance *localq[], int *localct,
                  char *ref_type, Instance *instance, char buf[]) { 

     int number;

     if(instance == NULL)
         sprintf(buf, "NULL");

     /* if referenced structure is one that should be    */ 
     /* printed as subordinate of current, add to queue */
     else if(ref_type != NULL && in_subords_print(ref_type)) {    
                 assert(*localct < MAX_PRINT_SUBORDS);
                 localq[*localct] = instance; 
                 (*localct)++;
                 sprintf(buf, "<%p>", instance);
           }
 
    else if(ref_type != NULL && in_immediate_print( ref_type))
              get_single_prop_value((Instance*)instance,  buf);

   /* otherwise non-local (back to margin) print */
   else {  
         number = get_print_number(ctl, instance);
         sprintf(buf, "(%d)", number);
    } 

}


/***********************************************************************/
/* if prtnumber non zero is nonembedded instance print                 */ 
/* otherwise indent should be > 1                                      */

static void print_instance(CCtl *ctl, FILE *outfile,
          Instance *inst, int prtnumber,int firstindent){


   int         typeind, i, j, indent, localct, ct;
   TypeDesc   *td;
   char       *ref_type, *outstring;
   Instance   *localq[MAX_PRINT_SUBORDS]; 
   char        buf[256];
   listElement *le_ptr;
   list        l;
   long        thelong;         

   typeind = inst->typeindex;
   td = &(ST_TYPES[typeind]);
   indent = firstindent;

   localct = 0;
   fprintf(outfile, "\n\n"); 

   if(indent == 0) {
      fprintf(outfile, "%4d %s %d", prtnumber, td->typename, typeind);
      indent = 0;
   }

   else {  
      assert(indent > 0);
      fprintf(outfile, "%*s <%p> %s",
             indent, " ", (void *)inst, td->typename);
   }

   indent += 4;
   for (i = 0;  td->propertydesc[i].propertykind != PropLast; i++) {

       switch(td->propertydesc[i].propertykind) {

           case PropString:

              outstring = (char *)(inst->properties[i]); 
              if(!outstring) outstring = "NULL";
              fprintf(outfile, "\n%*s  %s = %s",
                  indent, " ", td->propertydesc[i].propertyname, outstring);
              break; 

           case PropBool:
           case PropInt:

              fprintf(outfile, "\n%*s  %s = %d",
                    indent, " ", td->propertydesc[i].propertyname,
                    (int)inst->properties[i]);  
              break;

           case PropLongInt:

               if( ((char *)inst->properties[i]) == NULL) thelong = 0;
               else thelong = *(long int *)(inst->properties[i]); 
               fprintf(outfile, "\n%*s  %s = %ld",
                    indent, " ", td->propertydesc[i].propertyname,
                    thelong);
               break;

          case PropStruct:

              if((Instance *)inst->properties[i] == NULL)  
                       strcpy(&buf[0], "NULL");
              else {
                ref_type = td->propertydesc[i].typename;
                get_print_ref(ctl, localq, &localct,
                      ref_type, (Instance *)inst->properties[i], buf);
              } 
              fprintf(outfile, "\n%*s  %s = %s",
                  indent, " ", td->propertydesc[i].propertyname, buf);
              break;

          case PropOpaque:
               fprintf(outfile, "\n%*s  %s = %p",
                    indent, " ", td->propertydesc[i].propertyname,
                    (void *)inst->properties[i]);  
               break;


        /* fix this for serious nesting */
         case PropList:

             ref_type = td->propertydesc[i].typename;

             l = (list)inst->properties[i];
             if(l == NULL || list_size(l) == 0)  
                  fprintf(outfile, "\n%*s  %s = %s", indent, " ",
                          td->propertydesc[i].propertyname, "NULL");
          
             else {
                 /* print list content references 3 per line */
                 fprintf(outfile, "\n%*s  %s = ",
                      indent, " ", td->propertydesc[i].propertyname);
                 ct = 3;
                 for(le_ptr = l->head;
                    le_ptr != NULL; le_ptr = le_ptr->next) { 
                    /* get print form for ref */
                    get_print_ref(ctl,localq, &localct, 
                          ref_type, le_ptr->data, buf);
                    if(ct < 3) {
                       fprintf(outfile, ", %s" , buf);
                       ct++;
                    }
                    else  {
                       fprintf(outfile, "\n %*s  %s", indent + 5, " ",buf);
                      ct = 1; 
                    }
                 }
              }
              break;

         default: assert(0);  /* rest of possible values shouldn't occur*/

      } /* end switch */
                      
  } /* end for i */

 /* now print queued subordinate references */

  for(j = 0; j< localct; j++) { 
      print_instance(ctl, outfile, localq[j], 0 , indent +3);
   }
 
}


/***********************************************************************/
/* these are top level lists */
void print_instances(CCtl *ctl, FILE *outfile, list top) {  

    listElement *le_ptr;
    char         buf[256];
    int          ct= 0;
    char        *ref_type;
    Instance    *instance;
   
    ctl->toprint_ct = 0;
    ctl->current_toprint = 0;

    fprintf(outfile, "\n\n  top = ");

    for(le_ptr = top->head; le_ptr != NULL; le_ptr = le_ptr->next) { 

         /* get print form for ref */

          instance = (Instance *)le_ptr->data;
          ref_type = instance->typename; 
          get_print_ref(ctl, NULL, NULL, ref_type, le_ptr->data, buf);
          if(ct < 3) {
              fprintf(outfile, ", %s" , buf);
              ct++;
           }
           else  {
              fprintf(outfile, "%*s  %s", 5, " ", buf);
              ct = 1; 
           }
      }

     /* Now process pending instnaces */
     
     while( ctl->current_toprint < ctl->toprint_ct) {

        print_instance(ctl, outfile, ctl->toprint[ctl->current_toprint],
                       ctl->current_toprint, 0); 
        ctl->current_toprint++;
          
     }

}

#ifdef undef
char *cvt_name_hyphens(char *value) {

     char *temp;  
     int   i, len;

     len = strlen(value);
     temp =(void *)calloc(1, len +1);
     for(i = 0;  i < len; i++) {  
           if(value[i] == '-') temp[i] = '_';  
           else temp[i]=value[i];
      }
      temp[i] = '\0';

      return temp;
}

/*********************************************************************/
void put_by_index( Instance *instance,int prop_index, void *value) { 

    /* long ints must already be refd by ptr */ 
    instance->properties[prop_index] = value;
 }

#endif


