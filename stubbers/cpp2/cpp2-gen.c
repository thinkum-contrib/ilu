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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h> 
#include <ctype.h> 
#include <iluptype.h>
#include "../parser/version.h"
#include <data.h>
#include <rules.h>

extern RuleSet RuleSets[];
extern Function Functions[];
extern PropDesc PropDescs0[];
extern int struct_type_count;

extern char *get_string_property_value(void *, Instance *instance,
                                      char *property);
void *get_any_property_value(void *, Instance *instance,
                             char *property);
int get_int_property_value(void *ctl, Instance *instance, char *property); 
void  set_string_property_value(void *ctl, Instance *topscope,
                         char *property, char * newvalue);

Instance *ur_instance(void *ctl, Instance *t);

extern int produce_file(void *, Function * functions,
                        PropDesc *funpropdescs,
                        RuleSet *ruleset, Instance *input);

extern void * build_basic_module_structure(Interface interface);
extern void  top_fwd_def_sort(void *top_module); 
extern void *initialize_gen(int struct_type_count, void *stub_ctl);
typedef char* NameCvt(char*);
extern void *initialize_convert( NameCvt);

extern void  print_instances(void *, FILE *, list top);
extern list cvt_interfaces(void *, list);

typedef struct {
                int  use_cpp_namespace;
                int  use_cpp_classes;
               } SCtl;


/*************************************************************************/
/* Functions referenced in rule set (F_), and associated utilities       */
/*************************************************************************/

char *get_cpp_corba_name(void *ctl, SCtl *sctl, Instance *t, int uscore) {
  Type     thetype, urtype;
  TypeKind tk;

  /* get referenced iluptype struct for type */
  thetype = (Type)get_any_property_value(ctl, t, "ilup_ref");
  urtype = ur_type(thetype);
  tk = urtype->description->type; 

  if( uscore == 0 && (sctl->use_cpp_namespace || sctl->use_cpp_classes) ) {
    switch(tk) {
     case byte_Type:  
          return("CORBA::Octet");
     case boolean_Type:
          return("CORBA::Boolean");
    /* TEMP HACK */
     case character_Type:
          return("iluCharacter"); 
     case shortcharacter_Type:
          return("CORBA::Char"); 
     case shortinteger_Type:
          return("CORBA::Short");
     case integer_Type:
          return("CORBA::Long"); 
     case longinteger_Type:
          return("iluLongInteger"); 
     case shortcardinal_Type:
          return("CORBA::UShort"); 
     case cardinal_Type:
          return("CORBA::ULong"); 
     case longcardinal_Type:
          return("iluLongCardinal"); 
     case real_Type:
          return("CORBA::Double");
     case shortreal_Type: 
          return("CORBA::Float");
     case longreal_Type:
          return("iluLongReal");
     case pickle_Type:
          return("CORBA::Any");
     default:
          return NULL;
   }
 }
 else if(uscore != 2) {
    switch(tk) {
     case byte_Type:  
          return("CORBA_Octet");
     case boolean_Type:
          return("CORBA_Boolean");
    /* TEMP HACK */
     case character_Type:
          return("iluCharacter"); 
     case shortcharacter_Type:
          return("CORBA_Char"); 
     case shortinteger_Type:
          return("CORBA_Short");
     case integer_Type:
          return("CORBA_Long"); 
     case longinteger_Type:
          return("iluLongInteger"); 
     case shortcardinal_Type:
          return("CORBA_UShort"); 
     case cardinal_Type:
          return("CORBA_ULong"); 
     case longcardinal_Type:
          return("iluLongCardinal"); 
     case real_Type:
          return("CORBA_Double");
     case shortreal_Type: 
          return("CORBA_Float");
     case longreal_Type:
          return("iluLongReal");
     case pickle_Type:
          return("CORBA_Any");
     default:
          return NULL;
   }
 }
else {
    switch(tk) {
     case byte_Type:  
          return("CORBA.Octet");
     case boolean_Type:
          return("CORBA.Boolean");
    /* TEMP HACK */
     case character_Type:
          return("iluCharacter"); 
     case shortcharacter_Type:
          return("CORBA.Char"); 
     case shortinteger_Type:
          return("CORBA.Short");
     case integer_Type:
          return("CORBA.Long"); 
     case longinteger_Type:
          return("iluLongInteger"); 
     case shortcardinal_Type:
          return("CORBA.UShort"); 
     case cardinal_Type:
          return("CORBA.ULong"); 
     case longcardinal_Type:
          return("iluLongCardinal"); 
     case real_Type:
          return("CORBA.Double");
     case shortreal_Type: 
          return("CORBA.Float");
     case longreal_Type:
          return("iluLongReal");
     case pickle_Type:
          return("CORBA.Any");
     default:
          return NULL;
   }
 }

}

/* returns ptr to listElement in tscope list */
listElement *get_scope_diverge_pt(void *ctl,list t_scope, list m_scope) { 
   listElement *msp, *tsp;
   char        *mname, *tname;

   msp = m_scope->head;
   for(tsp = t_scope->head; tsp; tsp=tsp->next) {  
       tname = get_string_property_value(ctl,
                      (Instance *)(tsp->data), "namevalue");
       mname = get_string_property_value(ctl,
                      (Instance *)(msp->data), "namevalue");
       if(strcmp(tname, mname)) break;
       msp = msp->next;
       if(msp == NULL) {
             if(tsp->next != NULL) tsp = tsp->next;
             break;
       }  
    }
    return tsp;
}

/* NOT USED */
char *concat_instance_namelist_isl(void *ctl, SCtl *sctl,
            listElement *first_elem){

    listElement *ptr;
    char        *name, *result;
    char        *sep;  
    char         temp[256];
    int          len;

    temp[0]='\0';
    sep = ".";

    for(ptr = first_elem; ptr; ptr = ptr->next) {   
       name = get_string_property_value(ctl,
                      (Instance *)(ptr->data), "isl_name");
       strcat(temp, name);  
       if( ptr->next != NULL) strcat(temp, sep);
    }
    if( (len = strlen(temp)) == 0) return ""; 
    result = (void *)malloc(len +1); 
    strcpy(result, temp);
    return result;
}


char *concat_instance_namelist(void *ctl, SCtl *sctl,
   listElement *first_elem, char *sfx,
    int isprefix, int uscore, int notlast){

    listElement *ptr;
    char        *name, *result;
    char        *sep, *usesep;  
    char         temp[256];
    int          len;

    temp[0]='\0';
    if(sctl->use_cpp_namespace || sctl->use_cpp_classes) sep = "::";
    else sep = "_";

    if(uscore == 1) sep = "_";
    else if(uscore == 2) sep = ".";

    for(ptr = first_elem; ptr; ptr = ptr->next) {   
       if(notlast && ptr->next == NULL) break;
       name = get_string_property_value(ctl,
                      (Instance *)(ptr->data), "namevalue");
       strcat(temp, name);  
       /* if is member of CORBA IDL interface */
       if(get_int_property_value(ctl,(Instance*)(ptr->data), "is_real_class")
            && !uscore == 1)
            usesep = "::"; 
        else usesep = sep; 
                   
       if(isprefix || ptr->next != NULL) strcat(temp, usesep);
    }
    strcat(temp, sfx);
    if( (len = strlen(temp)) == 0) return ""; 
    result = (void *)malloc(len +1); 
    strcpy(result, temp);
    return result;
}

/* TBD: FIX THIS */
int is_varlen_type(Type t, int formember) {

    UnionDescription *theunion;
    Argument          arg;
    listElement      *ptr;
    TypeKind          typekind; 
    Type              type; 

    type = ur_type(t); 
   
    typekind = type->description->type;
    switch(typekind) {
         case string_Type:
         case object_Type:  
         case pickle_Type: 
         case sequence_Type: 
            return 1;
         case union_Type:
            if(formember) return 1; 
            theunion =&(type->description->structuredDes.uniond); 
            for(ptr = theunion->types->head; ptr; ptr= ptr->next) {
                 arg = (Argument)ptr->data;
                 if(is_varlen_type(arg->type, formember)) return 1;
            }
            arg = theunion->default_arm; 
            if(arg != NULL && is_varlen_type(arg->type,formember)) return 1;
            return 0;
         case record_Type:   
            if(formember) return 1; 
            for(ptr = type->description->structuredDes.record.fields->head;
                ptr; ptr= ptr->next) { 
                arg = (Argument)ptr->data; 
                if(is_varlen_type(arg->type, formember)) return 1;
            }
            return 0;
          case array_Type:
             return( is_varlen_type(
                  type->description->structuredDes.array.type, formember)); 
         /* ? */
          case optional_Type:
              return 0; 
          case reference_Type: return 0;
          default: return 0; 
      }
}

int contains_object(Type type, list l) {

    UnionDescription *theunion;
    Argument          arg;
    listElement      *ptr;
    TypeKind          typekind; 

    /* ?? */
    if(type->supertype!=NULL) return(contains_object(type->supertype, l)); 
   
   /* prevent cycles */ 

    if(l == NULL || l-> head == NULL);  
    else { 
          for (ptr = l->head;  ptr != NULL;  ptr = ptr->next)
               if( ((void *)ptr->data) == ((void *) type) ) return 0;
        }
    list_insert(l, type);

    typekind = type->description->type;
    switch(typekind) {

         case object_Type:  
              return 1;
         case sequence_Type: 
             return( contains_object(
                  type->description->structuredDes.sequence.type, l)); 
         case union_Type:
            theunion =&(type->description->structuredDes.uniond); 
            for(ptr = theunion->types->head; ptr; ptr= ptr->next) {
                 arg = (Argument)((Type)ptr->data);
                if(contains_object(arg->type, l) ) return 1;
            }
            arg = theunion->default_arm; 
            if(arg != NULL && contains_object(arg->type, l)) return 1;
            return 0;
         case record_Type:   
            for(ptr = type->description->structuredDes.record.fields->head;
                ptr; ptr= ptr->next) { 
                arg = (Argument)ptr->data; 
                if(contains_object(arg->type, l)) return 1;
            }
            return 0;
          case array_Type:
             return( contains_object(
                  type->description->structuredDes.array.type, l)); 
         /* ? */
          case optional_Type:
             return(contains_object (
                  type->description->structuredDes.optional, l)); 
          case reference_Type: return 0;
          default: return 0; 
      }
}


char *string_chartype(Type type) {
    Type  urtype, oftype, uroftype;
    char *chtype;

    urtype = ur_type(type);
    if(urtype->description->type != sequence_Type) return NULL; 

    oftype = urtype->description->structuredDes.sequence.type;
    uroftype = ur_type(oftype); 
    chtype = name_base_name(uroftype->name);
    if( (!strcmp(chtype, "shortcharacter")) ||
        (!strcmp(chtype,"character") ) ||
        (!strcmp(chtype, "byte" ))) 
       return chtype;
    else return NULL;
}

/* call only with sequence type  */
/* return shortcharacter or character if really string  */
char * F_string_chartype(void *ctl, SCtl *sctl, Instance *s)  {

    Type type;
    type = (Type)get_any_property_value(ctl, s, "ilup_ref");
    return (string_chartype(type)); 
}


/* must be called with a type */
int F_varlen_type(void *ctl, SCtl *sctl, Instance *instance) {

    Type  type;
    /* get iluptype type */
    type = (Type)get_any_property_value(ctl, instance, "ilup_ref");
    return is_varlen_type(type, 0);
}
    

/* gets CORBA prefix */
char *F_corba_pfx(void *ctl, SCtl *sctl) {  
  if(sctl->use_cpp_namespace || sctl->use_cpp_classes) return "CORBA::";
  else return "CORBA_";
}

/* get prefixed name, e.g, "const_abc",  "size_Array" */ 
char *F_full_pfxd_name(void *ctl, SCtl *sctl, char *pfx, Instance *t){

  list l;
  char *fullname, *name, *begin; 

  l= (list)get_any_property_value(ctl, t, "scoping"); 
  begin = concat_instance_namelist(ctl, sctl, l->head,"", 1, 0, 1);

  name = get_string_property_value(ctl, t, "name"); 

  fullname=(void *)calloc(1,
                    strlen(begin) + strlen(pfx) + strlen(name)+2);
  strcpy(fullname, begin);
  strcat(fullname, pfx); 
  strcat(fullname, "_"); 
  strcat(fullname, name);
  if(strlen(begin) != 0) {
    free(begin);
   }
  return fullname;
}


/* get prefixed name, e.g, "const_abc",  "size_Array" */ 
/* relative to current module                       */
char *F_rel_pfxd_name(void *ctl, SCtl *sctl, char *pfx,
                  Instance *m, Instance *t){

  list        t_scoping, m_scoping;
  char        *fullname, *name, *begin; 
  listElement *t_pt;

  t_scoping= (list)get_any_property_value(ctl, t, "scoping"); 
  if(m == NULL ||
    !( sctl->use_cpp_namespace || sctl->use_cpp_classes) ) 
        begin = concat_instance_namelist(ctl, sctl, t_scoping->head,
                    "", 1, 0, 1);
    else {
      m_scoping = (list)get_any_property_value(ctl, m, "scoping");

     /* get pt at which t_scoping diverges from m_scoping */
      t_pt = get_scope_diverge_pt(ctl, t_scoping, m_scoping);   

      begin = concat_instance_namelist(ctl, sctl, t_pt,"", 1, 0, 1);
   }

  name = get_string_property_value(ctl, t, "name"); 

  fullname=(void *)calloc(1,
                    strlen(begin) + strlen(pfx) + strlen(name)+2);
  strcpy(fullname, begin);
  strcat(fullname, pfx); 
  strcat(fullname, "_"); 
  strcat(fullname, name);
  if(strlen(begin) != 0) {
    free(begin);
   }
  return fullname;
}

/* get ur prefixed name, e.g, "const_abc",  "size_Array" */ 
char *F_ur_pfxd_name(void *ctl, SCtl *sctl, char *pfx, Instance *t){

  Instance *t1;
  t1 = ur_instance(ctl, t);
  return (F_full_pfxd_name(ctl, sctl,pfx, t1));
}


/*  Used only within sides (surrogate/true), or indirectly */
char *F_rel_side_name(void *ctl, SCtl *sctl, Instance *m, char *side,
     Instance *t) {

  int      dofull; 
  char     first[100];
  char   *intf, *name, *rest; 
  Type    type, urtype; 
  list    m_scoping, t_scoping;
  listElement *t_pt; 

  type = (Type)get_any_property_value(ctl, t, "ilup_ref");
  urtype = ur_type(type);

  if( !strcmp(name_base_name(urtype->name), "CORBA-Object"))  {
      if( sctl->use_cpp_namespace || sctl->use_cpp_classes ) 
                   return("CORBA::Object");
      else return ("CORBA_Object");
  }

  t_scoping= (list)get_any_property_value(ctl, t, "scoping"); 

  dofull = 0;
  if(m == NULL ||
    !( sctl->use_cpp_namespace || sctl->use_cpp_classes) )  dofull = 1;

  else {
     m_scoping = (list)get_any_property_value(ctl, m, "scoping");

     /* get pt at which t_scoping diverges from m_scoping */
     t_pt = get_scope_diverge_pt(ctl, t_scoping, m_scoping);   

     if(t_pt == t_scoping->head) dofull = 1; 
  }

  if(dofull) {

     intf = get_string_property_value(ctl,
                  (Instance *)(t_scoping->head->data), "namevalue");
     strcpy(first, intf);
     strcat(first, "_");  
     strcat(first, side);
     if(sctl->use_cpp_namespace || sctl->use_cpp_classes) strcat(first, "::");
     else strcat(first, "_");
   
     t_pt = t_scoping->head->next;
  }
  else first[0]= '\0'; 

  rest = concat_instance_namelist(ctl, sctl, t_pt, "", 0, 0, 0);  

  name = (void *)calloc(1, strlen(first) + strlen(rest)+1);

  strcpy(name, first); 
  if(strlen(rest) != 0) { strcat(name, rest); free(rest); }

  return name;
}

char *F_full_side_name(void *ctl, SCtl *sctl, char *side, Instance *t) {
     return(F_rel_side_name(ctl, sctl, NULL, side, t));
}

char *F_ur_side_name(void *ctl, SCtl *sctl, char *side, Instance *t) {

    Instance *t1;
    t1 = ur_instance(ctl, t);
    return (F_full_side_name(ctl, sctl,side, t1));
}

Instance *F_ur_instance(void *ctl, SCtl* sctl, Instance *t) {
    return (ur_instance(ctl, t));
}


/* for references to prefix within immediately containing module          */
/* returns "" if module mapping is to namespaces or nested classes       */ 
/* otherwise returns topmodule_true/surrogate.._rest of prefix           */
char *F_side_prefix(void *ctl, SCtl *sctl, char *side,
              Instance *t, int ismodule) {
  list l;
  char first[100];
  char *intf, *name, *rest; 

  if(sctl->use_cpp_namespace || sctl->use_cpp_classes)  return ""; 

  l= (list)get_any_property_value(ctl, t, "scoping"); 

  intf = get_string_property_value(ctl,
                  (Instance *)(l->head->data), "namevalue");
  strcpy(first, intf);
  strcat(first, "_");  
  strcat(first, side);
  strcat(first, "_");
   
  rest = concat_instance_namelist(ctl, sctl, l->head->next, "", 0, 0,
                        !ismodule);  
  name = (void *)calloc(1, strlen(first) + strlen(rest)+1);
  strcpy(name, first); 
  if(strlen(rest) != 0) {
    strcat(name, rest);
    free(rest);
   }
  return name;
}

/* for references to prefix within immediately containing module      */    
/* returns "" if module mapping  is to namespaces or nested classes  */
char *F_module_prefix(void *ctl, SCtl *sctl, Instance *m, int ismodule) {

  list l;
  if(sctl->use_cpp_namespace || sctl->use_cpp_classes) return ""; 

  l= (list)get_any_property_value(ctl, m, "scoping"); 
  return(concat_instance_namelist(ctl, sctl, l->head, "",1, 0,!ismodule));  
}

/* Like F_module_prefix except that if using underscores and */
/* prefix is that of real corba interface, also returns ""  */  
char *F_noninterface_prefix(void *ctl, SCtl *sctl, Instance *m, int ismodule) {

  list l;
  char *return_prefix;
  if(sctl->use_cpp_namespace || sctl->use_cpp_classes) return ""; 

  l= (list)get_any_property_value(ctl, m, "scoping"); 
  return_prefix = concat_instance_namelist(ctl, sctl, l->head, "",1, 0,!ismodule);  
  if( strstr(return_prefix, "::")!= NULL) return "";
  else return return_prefix;
}



/* full prefix */
char *F_full_prefix(void *ctl, SCtl *sctl, Instance *m, int ismodule) {

  list l;
  l= (list)get_any_property_value(ctl, m, "scoping"); 
  return(concat_instance_namelist(ctl, sctl, l->head, "", 1, 0, !ismodule));  
}

char *F_underscore_prefix(void *ctl, SCtl *sctl, Instance *m) {
  list l;
  l= (list)get_any_property_value(ctl, m, "scoping"); 
  return(concat_instance_namelist(ctl, sctl, l->head, "",  0, 1, 0));  
}


char *F_underscore_name(void *ctl, SCtl *sctl, Instance *t) {
  list l;
  Type type; 
  type = (Type)get_any_property_value(ctl, t, "ilup_ref");
  if(type->builtIn) {
       return(get_cpp_corba_name(ctl, sctl, t, 1)); 
  }
  l= (list)get_any_property_value(ctl, t, "scoping"); 
  return(concat_instance_namelist(ctl, sctl, l->head, "", 0, 1,0));  
}


/* Assumes that isl_name, after convert from CORBA IDL */  
/* contains sub-module names, separated by hyphens     */ 
/* retained as function in case changes                */
char *F_reg_name(void *ctl, SCtl *sctl, Instance *t) {
  char *reg_name;
  reg_name = (char *)get_any_property_value(ctl, t, "isl_name"); 
  return reg_name;
}

char *F_dotted_reg_name(void *ctl, SCtl *sctl, Instance *t) {  

  char *reg_name, *intf_name, *result;
  list l;
  Type type; 

  type = (Type)get_any_property_value(ctl, t, "ilup_ref");
  if(type->builtIn) {
       return(get_cpp_corba_name(ctl, sctl, t, 2)); 
  }
  l= (list)get_any_property_value(ctl, t, "scoping"); 
  intf_name = get_string_property_value(ctl,
                      (Instance *)(l->head->data), "isl_name");
  reg_name = (char *)get_any_property_value(ctl, t, "isl_name"); 
  result = (void*)malloc( strlen(intf_name) + strlen(reg_name) +3);
  strcpy(result, intf_name);
  strcat(result, ".");
  strcat(result, reg_name);
  return(result);
}


char *F_concat(void *ctl, SCtl *sctl, char *pfx, char *name) {
      char *result;

      if(pfx == NULL || name == NULL) {
         fprintf(stderr, "\nSystem or grammar error: " 
             "Null arguments to F_concat \n");
          exit(1); 
       }
      result = malloc(strlen(pfx) + strlen(name) +1);
      strcpy(result, pfx); 
      strcat(result, name);
      return result;
}

int F_use_namespace(void *ctl, SCtl *sctl) {
   if(sctl->use_cpp_namespace) return 1;
   return 0; 
}

int F_use_classes(void *ctl, SCtl *sctl) {
   if(sctl->use_cpp_classes) return 1;
   return 0; 
}


/******************************************************************/
Instance *ur_instance(void *ctl, Instance *t) {

   Instance *super, *lastsuper;
   lastsuper = super = t;
   while (1) {
      super = (Instance *)get_any_property_value(ctl, super, "supertype");
      if(super == NULL) break; 
      lastsuper = super;
    }
    return lastsuper;
}
     
char * F_ur_typename(void *ctl, SCtl *sctl, Instance *t) {

   Instance *uri;

   uri = ur_instance(ctl, t); 
   return uri->typename;
}


int F_ur_builtin(void *ctl, SCtl *sctl, Instance *t) { 
    Type  type, urtype;

    type = (Type)get_any_property_value(ctl, t, "ilup_ref");
    urtype = ur_type(type);
    if(urtype->builtIn) return 1;
    return 0;
}

char *F_instance_kind(void *ctl, void *SCtl, Instance *t) {
   return t->typename;
}

/***********************************************************/
/* gets reference to a type given current namespace */
/* change check if builtin to check if ur-type builtin? */
/* If m NULL, gets entire typeref                       */
char *F_rel_typeref(void *ctl, SCtl *sctl,
        Instance *m, Instance *t, char *sfx) {
    char  *cname;
    list   m_scoping, t_scoping;
    listElement *t_pt; 
    Type  type, urtype;

    type = (Type)get_any_property_value(ctl, t, "ilup_ref");
    urtype = ur_type(type);
    if(urtype->builtIn) {
       cname = get_cpp_corba_name(ctl, sctl, t, 0); 
       if(!strcmp(sfx, "")) return cname; 
       return(F_concat(ctl, sctl, cname, sfx));
    }

    /* turn ilu.CORBA_OBJECT into appropriate corba object reference */
    /* fix to check that interface name ilu */
    if( !strcmp(name_base_name(urtype->name), "CORBA-Object"))  {
           if( sctl->use_cpp_namespace || sctl->use_cpp_classes ) 
                   return("CORBA::Object");
           else return ("CORBA_Object");
     }

   {
    char  *chtype;
    chtype = string_chartype(urtype); 
    if(chtype != NULL) { 
        if( !strcmp(chtype, "shortcharacter")) { 
                return("iluShortCharacter*");
        }
        if(!strcmp(chtype, "character"))
           return "iluCharacter*";
    } 
  }
 
    t_scoping = (list)get_any_property_value(ctl, t, "scoping"); 

    if(m == NULL || !( sctl->use_cpp_namespace ||
                      sctl->use_cpp_classes) ) 
       return (concat_instance_namelist(ctl, sctl, t_scoping->head, sfx, 
                                 0, 0, 0));

    m_scoping = (list)get_any_property_value(ctl, m, "scoping");

    /* get pt at which t_scoping diverges from m_scoping */
    t_pt = get_scope_diverge_pt(ctl, t_scoping, m_scoping);   
    return(concat_instance_namelist(ctl, sctl, t_pt, sfx, 0, 0,0));  
}

char *F_full_rel_typeref(void *ctl, SCtl *sctl, Instance *t, char *sfx) {

   return(F_rel_typeref(ctl, sctl, NULL, t, sfx)); 
}

/* For non types */
char *F_full_ref(void *ctl, SCtl *sctl, Instance *t) {
  list l;
  Type type;
  
  type = (Type)get_any_property_value(ctl, t, "ilup_ref");

  l= (list)get_any_property_value(ctl, t , "scoping"); 
  return(concat_instance_namelist(ctl, sctl, l->head, "", 0, 0,0));  
}


char *F_full_typeref(void *ctl, SCtl *sctl, Instance *t) {
  return (F_rel_typeref(ctl, sctl,NULL, t, "")); 
}

char *F_full_ur_typeref(void *ctl, SCtl *sctl, Instance *t) {
  return(F_full_typeref(ctl, sctl, ur_instance(ctl, t))); 
}

/******************************************************************/
/* for optionals ... optionals of optionals?*/ 
/* USED ANYWHERE ? */
char *F_ref_typeref(void *ctl, SCtl *sctl, Instance *m, Instance *t) {
    Type  urtype, oftype, uroftype;
    Instance *urinstance, *ofinstance; 

    urinstance = ur_instance(ctl, t);
    urtype = (Type)get_any_property_value(ctl, urinstance, "ilup_ref");
    assert(urtype->description->type == optional_Type);

    ofinstance = (Instance *)
         get_any_property_value(ctl, urinstance, "oftype"); 
    oftype = (Type)get_any_property_value(ctl, ofinstance, "ilup_ref");
    uroftype =ur_type(oftype); 
    
    if(uroftype->description->type != object_Type)
         return(F_rel_typeref(ctl, sctl, m, ofinstance, "*"));
    return(F_rel_typeref(ctl, sctl, m, ofinstance, "_ptr"));
 }

char *F_full_ref_typeref(void *ctl, SCtl *sctl, Instance *t) {
     return(F_ref_typeref(ctl, sctl, NULL, t));
}

/******************************************************************/
/* of_instance set if instance is an optional                     */
char *F_internal_mbr_typeref(void *ctl, SCtl *sctl,
               Instance *m, Instance *instance, Instance *of_instance) {

    Type  type, urtype;
    char  *sfx, *chtype, *of_typekind;
    int   req_var; 
    Instance *net_instance;

    req_var = 0;
    net_instance = instance;
    if(of_instance != NULL) {
       of_typekind = F_ur_typename(ctl, sctl, of_instance);

       /* if is optional of nonprimitive,                */
       /*  base of mbrtype is the of_type, and var needed */
       if(  ( F_ur_builtin(ctl, sctl, of_instance) != 1 ) ||  
             (!strcmp(of_typekind,  "Pickle")) ) {
             net_instance = of_instance;
             req_var = 1; 
      }
   }
   
   type = (Type)get_any_property_value(ctl, net_instance, "ilup_ref");
   urtype = ur_type(type);
   chtype = string_chartype(urtype); 
   if(chtype != NULL) { 
        if( !strcmp(chtype, "shortcharacter")) {
            if( sctl->use_cpp_namespace || sctl->use_cpp_classes ) 
                return("CORBA::String_var");
            else return ("CORBA_String_var");
        }
        if(!strcmp(chtype, "character"))
           return "iluWString_var";
        else req_var = 1;   /* byte string, not clear what do */
    }  

  if(req_var == 1  ||
     !strcmp(F_ur_typename(ctl, sctl, net_instance), "Object") ) 
     sfx = "_var";
  else sfx = "";
 
  return F_rel_typeref(ctl, sctl, m, net_instance, sfx); 
} 
   

char *F_mbr_typeref(void *ctl, SCtl *sctl, Instance *m, Instance *t) {

    Instance *ur_instance, *instance, *of_instance;
    char     *typekind;

    instance  = t;
    ur_instance = F_ur_instance(ctl, sctl, instance);
    typekind = F_ur_typename(ctl, sctl, ur_instance);

    of_instance = NULL; 
    if(!strcmp(typekind, "Optional")) 
        of_instance =
            (Instance *)get_any_property_value(ctl, ur_instance, "oftype");

    return F_internal_mbr_typeref(ctl, sctl, m, instance, of_instance);

}    

char *F_full_mbr_typeref(void *ctl, SCtl *sctl, Instance *t) {

    return F_mbr_typeref(ctl, sctl, NULL, t); 
}

/***********************************************************************/
int  F_mbr_is_var(void *ctl, SCtl *sctl, Instance *t) {
  char   *mbr_typeref;

  mbr_typeref = F_mbr_typeref(ctl, sctl, NULL, t);
  if(strstr(mbr_typeref, "_var") == NULL) return 0;
  else if(!strcmp(mbr_typeref, F_rel_typeref(ctl, sctl, NULL, t, ""))) 
       return 0;
  else return 1;
 }


/***********************************************************************/
/* gets m_buf types for sequences;  instance is member type             */
/* of_instance is set if member type is an optional, and is            */
/* the reference of the optional                                       */
char *F_internal_mbr_seqbuf(void *ctl, SCtl *sctl,
               Instance *m, Instance *instance, Instance *of_instance) {

    char  *sfx, *of_typekind;
    Instance *net_instance;

    net_instance = instance;

    /* if sequence member an optional */
    /* which is by definition a pointer */ 
    if(of_instance != NULL) {
       of_typekind = F_ur_typename(ctl, sctl, of_instance);

       if( !strcmp(of_typekind, "Object")) { 
            net_instance = of_instance;
            sfx = "_ptr";
        }
        /*  seems to be needed */
        else if (!strcmp(of_typekind, "Array")) {
            net_instance = of_instance;
            sfx = "_slice*";   
        }
        else { 
            net_instance = instance;
            sfx = "";
        }
    }
    else {
         of_typekind = F_ur_typename(ctl, sctl, instance);
         if( !strcmp(of_typekind, "Object")) { 
            sfx = "_ptr";
        }
       /* removed
        else if (!strcmp(of_typekind, "Array")) {
            sfx = "_slice*";   
        }
       */
        else sfx = "";
    }
   
    return (F_rel_typeref(ctl, sctl, m, net_instance, sfx)); 
 }
         

/* obtains the type ref for the m_buf (as contrasted with m_varbuf)    */
/* members of a sequence                                               */
char *F_mbr_seqbuf(void *ctl, SCtl* sctl, Instance * m, Instance *t) {
    Instance *ur_instance, *instance, *of_instance;
    char     *typekind;

    instance  = t;
    ur_instance = F_ur_instance(ctl, sctl, instance);
    typekind = F_ur_typename(ctl, sctl, ur_instance);

    of_instance = NULL; 
    if(!strcmp(typekind, "Optional")) 
        of_instance =
            (Instance *)get_any_property_value(ctl, instance, "oftype");
    return (F_internal_mbr_seqbuf(ctl, sctl, m, instance, of_instance));
  }
     
char *F_full_mbr_seqbuf(void *ctl, SCtl *sctl, Instance *t) {
   return (F_mbr_seqbuf(ctl, sctl, NULL, t));
 }

/***********************************************************************/

int F_contains_object(void *ctl, SCtl *sctl, Instance *t) {

    Type  type;
    list  scanned_list;
    int   return_value;

    scanned_list = new_list();
    /* get iluptype type */
    type = (Type)get_any_property_value(ctl, t, "ilup_ref");
    
    return_value = contains_object(type, scanned_list);

    list_clear(scanned_list, 0);
    free(scanned_list); 
    return return_value;    
}

char *F_ur_property(void *ctl, SCtl* sctl, Instance *t, char *p1) {

  Instance *t1, *tlast;

  tlast = t1 = t;

  while ( t1 != NULL) {
     tlast = t1;
     t1 = get_any_property_value(ctl, t1, "supertype");
  }

  /* p1 must have string as value . add check*/
  return(get_string_property_value(ctl, tlast, p1));
}

/* gets various forms of array dims */
char *F_dims(void *ctl, SCtl *sctl, Instance *a, char *dimtype, int slice ){

    Type  type;
    list  dims;
    char  temp[100], temp1[10];
    char *rtnval, *comma, *mult; 
    listElement *ptr;
    int   first;

    /* get iluptype type */
    type = (Type)get_any_property_value(ctl, a, "ilup_ref");
    type = ur_type(type);
    dims = type->description->structuredDes.array.dimensions;
    if(dims == NULL) return "";

    /* gets [d1]..[dn], and if slice omits first */ 
    if(!strcmp(dimtype, "square")) {
       first = 1;
       temp[0]='\0'; 
       for(ptr = dims->head; ptr; ptr = ptr->next) {
          if(first) {
             first = 0;
             if(slice) continue;
          }
       sprintf(temp1, "[%d]", (int)ptr->data);
       strcat(temp, temp1);
       }
    }

    /* gets {d1,.. dn}*/ 
    else if(!strcmp(dimtype, "braces")) {
       strcpy(temp, "{ ");
       for(ptr = dims->head; ptr; ptr = ptr->next) {
          if(ptr->next != NULL) comma = ",";
          else comma = ""; 
          sprintf(temp1, "%d%s ", (int)ptr->data,comma);
          strcat(temp, temp1);
       }
       strcat(temp, "}");
    }

    /* gets d1 * d2 *.. dn*/ 
    else if(!strcmp(dimtype, "mult")) { 
       temp[0]='\0'; 
       for(ptr = dims->head; ptr; ptr = ptr->next) {
          if(ptr->next != NULL) mult= " * ";
          else mult = ""; 
          sprintf(temp1, "%d%s", (int)ptr->data, mult);
          strcat(temp, temp1);
       }
    }

    /* gets []..[dn] */
    else if(!strcmp(dimtype, "last")) {
       temp[0]='\0'; 
       for(ptr = dims->head; ptr; ptr = ptr->next) {
/*           if( (ptr == dims->head) || ptr->next != NULL) */
           if( ptr == dims->head) 
               sprintf(temp1, "[]");
           else 
              sprintf(temp1, "[%d]", (int)ptr->data);
           strcat(temp, temp1);
       }
    }

    else {
       fprintf(stderr, "\nSystem error:"
             "Unknown alternative invocation of F_dims\n"); 
         exit(1);
     }

    rtnval = (void *)calloc(1, strlen(temp)+1);
    strcpy(rtnval, temp);
    return rtnval;
}


/* for use in referencing dim indexes within for loop */
char *F_named_dims(void *ctl, SCtl *sctl, Instance *a, char *dimprefix) {

    Type  type;
    list  dims;
    char  temp[200], temp1[25];
    char *rtnval; 
    listElement *ptr;
    int    i;

    /* get iluptype type */
    type = (Type)get_any_property_value(ctl, a, "ilup_ref");
    type = ur_type(type);
    dims = type->description->structuredDes.array.dimensions;
    if(dims == NULL) return "";

     temp[0]='\0'; 
     for(ptr = dims->head, i = 0; ptr; ptr = ptr->next, i++) {
       sprintf(temp1, "[%s%d]", dimprefix, i);
       strcat(temp, temp1);
       assert (strlen(temp) < 200);
    }

    rtnval = (void *)calloc(1, strlen(temp)+1);
    strcpy(rtnval, temp);
    return rtnval;
}


/* OPTIONALS need more work? */
char *F_arg_sfx(void *ctl, SCtl* sctl, Instance *t, char *direction) {  
   int      i;
   Type     type, urtype;
   TypeKind typekind;
   char    *chtype;
 
   typedef struct {
           char     *in_suffix;
           char     *inout_suffix;
           char     *out_suffix;
           char     *return_suffix;
       } SfxSet;

   typedef struct {
          TypeKind  typekind;
          SfxSet    fixed_suffixes;  
          SfxSet    var_suffixes;  
   } ArgSfx;

  ArgSfx Suffixes[] = { 
      {enumeration_Type, {"", "&", "&", ""},
                     {"", "&", "&", ""}},    
      {record_Type,  {"&", "&", "&", ""},
                     {"&", "&", "*&", "*"}},
      {union_Type,   {"&", "&", "&",  ""},
                     {"&", "&", "*&", "*"}},
     /* ?? */
      {optional_Type, {"", "&", "&", ""},  
                      {"", "&", "&", ""}},  
      {sequence_Type, {"&", "&", "*&", "*"},
                      {"&", "&", "*&", "*"}},
      {pickle_Type,   {"&", "&", "*&", "*"},
                      {"&", "&", "*&", "*"}},
      {array_Type,    {"", "", "",        "_slice*"},
                      {"", "", "_slice*&", "_slice*"}},
      {object_Type,   {"_ptr", "_ptr&", "_ptr&", "_ptr"},
                      {"_ptr", "_ptr&", "_ptr&", "_ptr"}},
      {invalid_Type},  
   };  

   SfxSet *s1;   

    type = (Type)get_any_property_value(ctl, t, "ilup_ref");
    urtype = ur_type(type);

    typekind = urtype->description->type;

    if(urtype->builtIn && typekind != pickle_Type) {
         if( (strcmp(direction, "In") != 0)
              && strcmp(direction, "Return") != 0)
              return "&";
         else return "";
    }

    /* if really string */
  
    if(typekind == sequence_Type) 
      chtype = string_chartype(urtype); 
    else chtype = NULL; 

    if(chtype  != NULL && (strcmp(chtype,"byte") != 0)) { 
            if( (strcmp(direction, "In") != 0)
              && (strcmp(direction, "Return") != 0)) return "&";
            else return "";
     }
       

    for(i = 0; Suffixes[i].typekind != invalid_Type; i++) { 
        if(Suffixes[i].typekind == typekind) break;
    }
    if(Suffixes[i].typekind == invalid_Type){
       fprintf(stderr, "\nSystem error:"
             "Unknown type in invocation of F_arg_sfx\n"); 
       exit(1);
    }
         
    if(is_varlen_type(urtype, 0)) 
              s1 = &(Suffixes[i].var_suffixes);
    else s1 = &(Suffixes[i].fixed_suffixes);

    if(!strcmp(direction, "Return"))     return(s1->return_suffix); 
    else if(!strcmp(direction, "In"))    return(s1->in_suffix); 
    else if(!strcmp(direction, "Out"))   return(s1->out_suffix); 
    else if(!strcmp(direction, "InOut")) return(s1->inout_suffix); 

    fprintf(stderr, "\nSystem Error:"
                    "Unknown argument direction in invocatio of F_arg_sfx");  
    exit(1);
}


char *F_arg_pfx(void *ctl, SCtl* sctl, Instance *t, char *direction) {  

   Type     type, urtype, ur_oftype;

    if(strcmp(direction, "In")) return "";
    type = (Type)get_any_property_value(ctl, t, "ilup_ref");
    urtype = ur_type(type);

    if(urtype->builtIn && urtype->description->type != pickle_Type) 
       return "";
      
    if(urtype->description->type == enumeration_Type ||
       urtype->description->type == object_Type ) return "";

    if(urtype->description->type == optional_Type) {
       ur_oftype =
            ur_type(urtype->description->structuredDes.optional); 
       if(ur_oftype->description->type == object_Type) return "";
    } 

  
    return "const ";
 }

int F_last_index(void *ctl, SCtl* sctl, list l) {
    if(l == NULL) return -1;
    return (list_size(l) - 1);
}

int F_list_ct(void *ctl, SCtl *sctl, list l) { 
    return (list_size(l));
}

/* compare elements of l to inst (ptr only) and return index+1 if found */
int F_list_match_ix( void *ctl, SCtl *sctl, list l, Instance *inst){ 

  listElement *ptr;
  int          j;

  for( ptr = l->head, j= 0 ; ptr; ptr = ptr->next, j++) {  
     if((void *) ptr->data == (void *) inst) return j+1; 
    }
  return 0;
}
   

/* gets positive vals for integers  */
/* don't use for reals  */
char *F_constant_value(void *ctl, SCtl *sctl, Instance * i) {
    char     *ctype, *returnstring;
    char      rtnstg[100];
    long int *longintptr;
    int       intvalue;

    ctype= i->typename;

    if(!strcmp(ctype, "BoolConstantValue")) {  
          intvalue = (int)get_any_property_value(ctl, i, "value");
          if(intvalue) return ("ILUCPP_TRUE"); 
          else return ("ILUCPP_FALSE");
    }
    else if (!strcmp(ctype, "IntConstantValue")) {  
         longintptr = (long int*)get_any_property_value(ctl, i, "value");
         sprintf(&rtnstg[0], "%ld", *longintptr);
     }
    else {
         strcpy(&rtnstg[0],(char *)get_any_property_value(ctl, i, "value"));
     }
    
     returnstring = (void *)malloc(strlen(&rtnstg[0])+1);
     strcpy(returnstring, rtnstg);
      
     return returnstring;
}


/* restore escapes to strings */
char *F_escape_string(void *ctl, SCtl* sctl, char *value) {

   char *newvalue, *newptr, *oldptr;

   newvalue = (void *)malloc(2*(1+ strlen(value))); 
   newptr = newvalue;
   oldptr = value; 
   while(1) { 
       if(*oldptr == '\n') 
         { *newptr = '\\';  *(newptr+1) = 'n';  newptr += 2; }
       else if(*oldptr == '\r')
         { *newptr = '\\';  *(newptr+1) = 'r';  newptr += 2; }
      else if(*oldptr == '\"') 
         { *newptr = '\\';  *(newptr+1) = '\"'; newptr += 2; }
      else {
          *newptr = *oldptr; 
          if(*newptr == '\0') break;
           newptr++;
      }
      oldptr++;      
   }

   if(strlen(value) == strlen(newvalue)) {
     free(newvalue);
     return(value); 
   }
   else return(newvalue);
}
   

char *F_vs_string(void *ctl, SCtl *sctl) {
    return (ILU_VERSION_STRING);
}

char *F_mult_blanks(void *ctl, SCtl *sctl, int i, int j) {

   char temp[50], *temp1;

   sprintf(temp, "%*s", (i*j)-1, " "); 
   temp1 = (void *)malloc(strlen(temp)+1); 
   strcpy(temp1, temp);
   return (temp1); 
}

int F_add(void *ctl, SCtl *sctl, int i, int j) {
    return (i+j);
 }

char *F_longint_to_str(void *ctl, SCtl *sctl, long int *i) {
   char temp[20], *temp1;

   sprintf(temp, "%ld", *i); 
   temp1 = (void *)malloc(strlen(temp)+1); 
   strcpy(temp1, temp);
   return (temp1); 
}

char *F_lowercase_name(void *ctl, SCtl* sctl, char *name) {
   char *temp;

   temp = (void *)malloc(strlen(name)+1); 
   strcpy(temp, name);
   temp[0] = tolower(temp[0]);
   return (temp); 
}

/* gets a default value with which to init union discriminators */
char *F_get_union_default(void *ctl, SCtl *sctl, Instance *ut) {

    Type      type, disc_type;
    list      arms, armvalues, enumvals;
    int       found_true, found_false, found;
    TypeKind  d_typekind;
    Argument  arm;
    char      *int_temp;
    EnumField  enumfield; 
    listElement  *ptr, *ptr1, *ptr2;
    ConstantValue armvalstruct;
    unsigned long max_intval;

    max_intval = 0;
    found_true = 0;
    found_false = 0;
    
    /* get iluptype type */
    type = (Type)get_any_property_value(ctl, ut, "ilup_ref");
    type = ur_type(type);

    disc_type = type->description->structuredDes.uniond.discriminator_type;
    disc_type = ur_type(disc_type);
    d_typekind = disc_type->description->type;

    arms = type->description->structuredDes.uniond.types;

    /* if discriminator type is enumeration, find a value not */
    /* a value of some arm                                    */
    if(d_typekind == enumeration_Type) {

        enumvals = disc_type->description->structuredDes.enumeration;

        for(ptr2 = enumvals->head; ptr2; ptr2 = ptr2->next) {  
            enumfield = (EnumField)(ptr2->data);  
            found = 0; 
            
           for(ptr = arms->head; ptr; ptr = ptr->next) {
              arm = (Argument)ptr->data; 
              armvalues = arm->values;
              for( ptr1 = armvalues->head; ptr1; ptr1 = ptr1->next) {
                 armvalstruct = (ConstantValue)ptr1->data;  
                 if(!strcmp(enumfield->name, armvalstruct->val.s)){
                     found = 1;
                     break;
                  }
               }
               if(found) break;
          }
          if(!found) break;
       }    

       /* if found an enum value not covered by arms, return it  */  
       if(!found) return(enumfield->name);

       fprintf(stderr,"Error, union with enumeration discriminator"
              "has default arm, but no available default value.");
       exit(1);
    }


   /* discriminator type is some form of integer, or boolean */
    for(ptr = arms->head; ptr; ptr = ptr->next) {

        arm = (Argument)ptr->data; 
        armvalues = arm->values;
        for( ptr1 = armvalues->head; ptr1; ptr1 = ptr1->next) {

           armvalstruct = (ConstantValue)ptr1->data;  
           if(d_typekind == boolean_Type) { 
               if(armvalstruct->val.b == TRUE)
                   found_true = 1;
               else found_false = 1;
           }
          else { /* must be some sort of integer */
               if(armvalstruct->val.i.value > max_intval)
                       max_intval = armvalstruct->val.i.value;
          }

       }
    }

   if(d_typekind == boolean_Type) {  
      if(!found_true) return "ILUCPP_TRUE";
      else if(!found_false) return "ILUCPP_FALSE";
      else {
         fprintf(stderr,"Error, union with boolean discriminator"
              "has default arm, but no available default value.");
         exit(1);
      }
   }

   else  {
     int_temp = (void *)malloc(20);    
     sprintf(int_temp, "%ld" , max_intval + 1);
     return(int_temp); 
   }

}


/* Used if no arm name.                    */ 
/* If not builtin_type, return _typename,  */
/* else return corba name with underscores            */
char * F_build_arm_name(void *ctl, SCtl* sctl, Instance *armtype) {

  char *base, *name;

  /* get referenced iluptype struct for type */
  if(F_ur_builtin(ctl, sctl, armtype))
     base = get_cpp_corba_name(ctl, sctl, armtype, 1); 
  else 
     base = (char *)get_any_property_value(ctl, armtype, "name");

   name = (void *)malloc(strlen(base) + 10); 
   strcpy(name, "_");
   strcat(name, base);
   strcat(name, "_arm");
   return name;
}

/* objects are only types that can have optional switch set */ 
int F_optional_switch(void *ctl, SCtl* sctl, Instance *type) {
   char     *typename;
   int       optional;
   Instance *urtype;

   typename = F_ur_typename(ctl, sctl, type);
   urtype = F_ur_instance(ctl, sctl, type);  
   if( strcmp(typename, "Object"))  return 0; 

   optional = (int)get_any_property_value(ctl, urtype, "optional");
   return optional;
}

Instance *F_optional_ofurtype(void *ctl, SCtl *sctl, Instance *type) {

  char     *typekind; 
  Instance *urtype, *oftype, *ofurtype; 

  urtype = F_ur_instance(ctl, sctl, type);
  typekind = F_ur_typename(ctl, sctl,urtype);
  
  if( !strcmp(typekind, "Optional")) {
      oftype = (Instance *)get_any_property_value(ctl, urtype, "oftype");
      ofurtype = F_ur_instance(ctl, sctl, oftype);
  }
  else if( (!strcmp(typekind, "Object")) &&  
           (((int)get_any_property_value(ctl, urtype, "optional"))==1) )
            ofurtype = urtype;
  else ofurtype = NULL; 

  return ofurtype;
}
  

/* returns an alternate type if given type is an optional, */  
/* and is represented when optional by _var in  structures */ 
Instance *F_optional_vartype(void *ctl, SCtl* sctl, Instance * type) {

  Instance *ofurtype; 

  ofurtype = F_optional_ofurtype(ctl, sctl, type);

  if(ofurtype == NULL) return NULL;

  if( (F_ur_builtin(ctl, sctl, ofurtype) == 1) 
       && (strcmp(F_ur_typename(ctl, sctl, ofurtype), "Pickle")!= 0))
       return NULL;
     
   else return ofurtype;
}

/* This is for sunpro workaround. Ambiguity of size/insert/delete  */
/* calls for these types                                           */
char *F_ilu_long_name(void *ctl, SCtl *sctl, Instance *type, int paren) {

  Type     thetype, urtype;
  TypeKind tk;


  /* get referenced iluptype struct for type */
  thetype = (Type)get_any_property_value(ctl, type, "ilup_ref");
  urtype = ur_type(thetype);
  if(!urtype->builtIn) return "";

  tk = urtype->description->type; 

  if(paren) {
     switch(tk) {
       case longinteger_Type:
            return("(iluLongInteger)"); 
       case longcardinal_Type:
          return("(iluLongCardinal)"); 
       case longreal_Type:
          return("(iluLongReal)");
       default:
          return "";
      }
   } else {
     switch(tk) {
       case longinteger_Type:
            return("iluLongInteger"); 
       case longcardinal_Type:
          return("iluLongCardinal"); 
       case longreal_Type:
          return("iluLongReal");
       default:
          return "";
     }
  }
}

int F_idl_exception_prefix(void *ctl, SCtl* sctl, char * name) {

 char *prefix = strstr(name, "ilu--prefix-idlExceptionType");
 if(prefix != NULL) return 1;
 return 0;
}

/* If method name is not a get/set for a CORBA attribute,  */ 
/* just return the input method name.                       */
/* If it is,  drop the get-set for external reference purposes   */     
/* (attribute access by overloaded reference)                 */
char *F_external_method_name(void *ctl, SCtl* sctl,
                         char *method_name, char *isl_name) { 
   char *overload_name;
   int   len; 

   if(strstr(method_name, "get_") == method_name  ||
       strstr(method_name, "set_") == method_name) {
        
      if( (strstr(isl_name, "ilu--prefix-idlAttribute--get-") != NULL)  ||
         (strstr(isl_name, "ilu--prefix-idlAttribute--set-") != NULL) ) {

           len = strlen("get_");
           overload_name = method_name + len;
           return overload_name;
       }
      
   }
   return method_name;
}



char * F_convert_underscores(void *ctl, SCtl* sctl, char *name) { 
     char *temp;
     int   i, len;
     if(strchr(name, '_') == NULL) return name; 
     len = strlen(name);
     temp =(void *)calloc(1, len +1);
     for(i = 0;  i < len; i++) {
           if(name[i] == '_') temp[i] = '-';
           else temp[i]=name[i];
     }
    temp[i] = '\0';
    return temp;
}

int F_print_labelled_name(void *ctl, SCtl* sctl, 
            char * s1, char *s2)  {

    printf("\n %s %s", s1, s2); 
    return 1;
}
int F_print_module_call(void *ctl, SCtl* sctl, Instance *i) {
    printf("\n module call for %s\n",
           get_string_property_value(ctl, i, "simple_name"));
   return 1;

}
      

#ifdef undef
/* outdated functions.   kept around just in case */

char *F_discrim_tk_type(void *ctl, SCtl* sctl, Instance *u) {
    char *d_itype;
 
       d_itype = F_ur_property(ctl, sctl, u, "discrim_type")
       if ( d_itype == "ShortCardinal")  d_type = "ilu_shortcardinal_tk";
       else if(d_itype == "Enumeration")  d_type = "ilu_enumeration_tk";
       else if (d_itype == "Boolean")      d_type = "ilu_boolean_tk";
       else if( d_itype == "Cardinal")   d_type = "ilu_cardinal_tk"; 
       else if(d_itype == "ShortInteger)" d_type = "ilu_shortinteger_tk"; 
       else if( d_itype == "Integer")      d_type = "ilu_integer_tk"; 
       else if (d_itype == "Byte")          d_type = "ilu_byte_tk";
       else assert(0);

    return (d_itype);
}


/* gets actual exceptions */
list F_object_exceptions(void *ctl, SCtl *sctl, Instance *t) { 

    list        m_exceptions, exceptions, methods;
    Instance   *method;
    listElement *mptr, *meptr, *ceptr; 
    Type        type;

    methods = (list)get_any_property_value(ctl, t, "methods");
    
    if(methods == NULL | list_size(methods) == 0) return NULL;

    exceptions = (list)iluparser_new_list(); 

    for(mptr = methods->head; mptr != NULL; mptr= mptr->next) { 

         method = (Instance*)mptr->data;
         m_exceptions = (list)get_any_property_value(ctl,method,
               "exceptions");

         if(m_exceptions == NULL | list_size(m_exceptions) == 0)
               continue;
         for (meptr = m_exceptions->head; meptr; meptr=meptr->next) {

              for(ceptr = exceptions->head; ceptr; ceptr = ceptr->next) {  
                 if((Instance *)meptr->data == (Instance*)ceptr->data) 
                       break;
              }

              if(ceptr != NULL) continue; 
              list_insert(exceptions, meptr->data);
        }
     }

     if(list_size(exceptions) == 0) { 
             free(exceptions);
             return NULL;  
     }
     return exceptions;
}

char *F_ur_property_property(void *ctl, SCtl *sctl, Instance *t,
      char *p1, char *p2){

  Instance *t1, *tlast, *t2;
  t1 = tlast = t;

  while ( t1 != NULL) {
     tlast = t1;
     t1 = get_any_property_value(ctl, t1, "supertype");
  }

  /* p1 must have a type as value */
  t2 = get_any_property_value(ctl, tlast, p1);
  tlast = t2;
  while (t2 != NULL) {
      tlast = t2; 
      t2 = get_any_property_value(ctl, t2, "supertype");  
  }

  return(get_string_property_value(ctl, tlast, p2));
}

int F_int_ur_property_property(void *ctl, SCtl *sctl, Instance *t, 
    char *p1, char *p2) {
  Instance *t1, *tlast, *t2;
  t1 = tlast = t;

  while ( t1 != NULL) {
     tlast = t1;
     t1 = get_any_property_value(ctl, t1, "supertype");
  }

  /* p1 must have a type as value */
  t2 = get_any_property_value(ctl, tlast, p1);
  tlast = t2;
  while (t2 != NULL) {
      tlast = t2; 
      t2 = get_any_property_value(ctl, t2, "supertype");  
  }
  return(get_int_property_value(ctl, tlast, p2));
}


/* handling of \r newlines vague */

/* gets the index of list entry e in list l'*/ 
int F_Listindex_e(void *ctl, SCtl *sctl, list l, Instance *e) {

   listElement *ptr; 
   Instance    *list_e, *input_e; 
   int          i;
   
   if(l == NULL || list_size(l)==0) {
     printf("\nFunction F_Listindex invoked on empty list.");
     exit(1);
    }

   input_e = e; 
   get_current_vs(ctl,&input_e); 
   for(ptr = l->head, i = 0; ptr!= NULL; ptr = ptr->next, i++) {
       /* get latest version of both list entry and compare entry */  
       list_e = (Instance *)(ptr->data);
       get_current_vs(ctl,&list_e); 
       if(list_e == input_e) break;
   }
   if(ptr != NULL) return i;
     else return -1;
}


/* gets the index of the list entry where 'property' has value "value'*/ 
int F_Listindex_v(void *ctl, SCtl *sctl, list l,
                    char *property, char *value) {

   listElement *ptr; 
   Instance    *instance; 
   char        *thisvalue;
   int          i;
   
   if(l == NULL || list_size(l)==0) {
     printf("\nFunction F_Listindex invoked on empty list.");
     exit(1);
    }

   for(ptr = l->head, i = 0; ptr!= NULL; ptr = ptr->next, i++) {

       instance = (Instance *)(ptr->data);
       thisvalue = get_string_property_value(ctl, instance, property);

       if(thisvalue != NULL && !strcmp(thisvalue, value))
           break;
   }

   if(ptr != NULL) return i;
     else return -1;
}
           
#endif


/***************************************************************/
/* functions to create c++ name from isl name                  */

static int call_strcmp(const void *name, const void *keyword) {
  char **y = (char**) keyword;
  return strcmp((char *)name, *y); 
}

static char *cvt_name_hyphens(char *name) {
     char *temp;
     int   i, len;
     if(strchr(name, '-') == NULL) return name; 
     len = strlen(name);
     temp =(void *)calloc(1, len +1);
     for(i = 0;  i < len; i++) {
           if(name[i] == '-') temp[i] = '_';
           else temp[i]=name[i];
     }
    temp[i] = '\0';
    return temp;
}

/* strip ilu_prefixes indicating CORBA class attributes */
/* (DOn't strip from exceptions..  will get duplicate names */
static char *strip_ilu_prefixes(char *name) { 
   int   len;
   char *name1, *name2;

   if(strstr(name, "ilu--prefix-idl") == NULL) return name; 

   /* PROBLEM:  this should also pare off the get/set, but */
   /* result is multiple true side defs because of overloading */
   if( (strstr(name, "ilu--prefix-idlAttribute--get-") != NULL)  ||
      (strstr(name, "ilu--prefix-idlAttribute--set-") != NULL) )
         len = strlen("ilu--prefix-idlAttribute--");

   else return name;

   name1 = name + len;
   name2 = (void *)malloc(strlen(name1) +2); 
   strcpy(name2, name1);
   return(name2); 
}


static char *prefix_kwd_by_uscore (char *name) {  
     char *temp, *search_result;
     char *keywords[] = { 
        "and",
        "and_eq",
        "asm", 
        "auto",
        "bitand",
        "bitor",
        "bool",
        "break",
        "case",
        "catch", 
        "char", 
        "class",
        "compl",
        "const", 
        "const_cast",
        "continue", 
        "default",
        "delete",
        "do", 
        "double",
        "dynamic_cast",
        "else",
        "enum",
        "explicit",
        "export",
        "extern",
        "false",
        "float",
        "for",
        "friend",
        "goto",
        "if",
        "inline", 
        "int",
        "long", 
        "mutable",
        "namespace",
        "new",
        "not",
        "not_eq", 
        "operator",
        "or",
        "or_eq", 
        "private",
        "protected", 
        "public",
        "register",
        "reinterpret_cast",
        "return",
        "short",
        "signed",
        "sizeof",
        "static",
        "static_cast",
        "struct",
        "switch",
        "template",
        "this", 
        "throw",
        "true",
        "try",
        "typedef",
        "typeid",
        "typename",
        "union", 
        "unsigned",
        "using",
        "virtual", 
        "void",
        "volatile",
        "wchar_t",
        "while",
        "xor",
        "xor_eq"
         };

  search_result = bsearch((void *)name, 
            (void *)&keywords[0], 74, sizeof(char *), call_strcmp);
  if(search_result == NULL) return name;
  temp = (void *)malloc(strlen(name)+2);
  strcpy(temp, "_");  
  strcat(temp, name);
  return temp;
}

char * make_cpp_name(char *name) { 
  char *name0,  *name1, *name2;
  if (name == NULL) return name;
  name0 = strip_ilu_prefixes(name);
  name1 = cvt_name_hyphens(name0);
  name2 = prefix_kwd_by_uscore (name1);
  return name2;
}


/*************************************************************************/
/* Utilities for modifying top level module name if necessary            */
/* occurs when top level module name is same as an immediately contained */
/* classs and mapping is to namespaces or nested classes                 */
/*************************************************************************/

/* See if name of top level module = interface name same as that of an */
/* immediately ctd class.                                               */
int  interface_name_is_classname(void *ctl, Instance *interface) {

  list         classes;
  listElement *ptr;
  Instance    *t;
  char        *iname, *tname;

  iname = get_any_property_value(ctl, interface, "name"); 
#ifdef DEBUG
  printf("\nInterface name %s\n", iname);
#endif
  if( (!strcmp(iname, "ilu")) || !strcmp(iname, "CORBA")) return 0; 
  classes = get_any_property_value(ctl, interface, "classes");
  if(classes == NULL || classes->head == NULL) return 0;
  for (ptr = classes->head; ptr; ptr = ptr->next) {
     t = (Instance*)(ptr->data); 
     if(get_any_property_value(ctl, t, "importedfromname") != NULL) return 0;
     tname = get_string_property_value(ctl, t, "name");
     if(!strcmp(iname, tname)) break; 
  } 
  /* if not completed list, there is a class name = to interface name */
  if(ptr == NULL) return 0;
  return 1;
}

   
/* For list of instances (representing types, exceptions, constants) */
/* converts initial name in the scoping lists of those instances     */
/* to the given one                                                  */
void convert_top_names(void *ctl, list ilist, char *iname, char *newname){

 Instance    *instance, *topscope; 
 listElement *ptr; 
 list         slist; 
  
 for(ptr = ilist->head; ptr; ptr= ptr->next) {
    instance = (Instance *)(ptr->data); 
    slist = get_any_property_value(ctl, instance, "scoping");
    if(slist == NULL || slist->head == NULL) continue; /* should not occur*/
    topscope = (Instance *)(slist->head->data);
    /* if it is really contained in this interface */
    if(!strcmp(iname,
       get_string_property_value(ctl, topscope, "namevalue")) ) 
       set_string_property_value(ctl, topscope, "namevalue", newname);
   }
}

void convert_module_scopings(void *ctl, Instance *module,
                            char *iname, char *newname) { 

 list         ilist;
 listElement *ptr;
 Instance    *instance;

 ilist = get_any_property_value(ctl, module, "contained_modules");
 if(ilist != NULL) {
    /* convert the scoping lists for immed contained modules */ 
    convert_top_names(ctl, ilist, iname, newname);
    /* recurse for their contained modules if any */   
    for(ptr = ilist->head; ptr; ptr= ptr->next) {
       instance = (Instance *)(ptr->data); 
       convert_module_scopings(ctl, instance, iname, newname);
    }
  }
 }

/* changes first element of scoping lists, as well as names of */
/* interface and top-level module, to given newname            */
/* (isl-names are unchanged)                                   */
void convert_instance_scopings(void *ctl, Instance *interface, char *newname ) { 

 list         ilist, slist;
 Instance    *topscope, *topmodule; 
 char        *iname; 

  iname = get_string_property_value(ctl, interface, "name"); 
 /* convert language oriented name of interface and topmost module */ 
  topmodule = (Instance *)get_any_property_value(ctl, interface, "module_str");
  set_string_property_value(ctl, topmodule, "simple_name", newname); 


 /* convert first in scoping list for all types, constants, exceptions */
 ilist = (list) get_any_property_value(ctl, interface, "types"); 
 if(ilist != NULL) convert_top_names(ctl, ilist, iname, newname);

 ilist = (list) get_any_property_value(ctl, interface, "exceptions"); 
 if(ilist != NULL) convert_top_names(ctl, ilist, iname, newname);
 
 ilist = (list) get_any_property_value(ctl, interface, "constants"); 
 if(ilist != NULL) convert_top_names(ctl, ilist, iname, newname);

 /* Do the same for all modules */  
 /* (unfortunately, these are in trees not lists */
 /* convert topmost module scoping */
  slist = (list) get_any_property_value(ctl, topmodule, "scoping");
  assert(slist != NULL);
  topscope = (Instance *)(slist->head->data);
  set_string_property_value(ctl, topscope, "namevalue", newname);

  convert_module_scopings(ctl, topmodule, iname, newname);  
}


void module_name_check_adjust(void *ctl, Instance *interface, char *lang) {
  char        *newname, *iname;

  if(!interface_name_is_classname(ctl, interface)) return; 
  iname = get_string_property_value(ctl, interface, "name"); 

  /* Warn */ 
  fprintf(stdout, "\nWarning: renaming top level module/ilu-interface"); 
  fprintf(stdout, "\n   from %s to _%s to avoid %s compilation problems.",
                      iname, iname, lang);
  fprintf(stdout, "\nIf this unacceptable, ensure top level name different");
  fprintf(stdout, "\n   from that of any immediately contained object.");    
  fprintf(stdout, "\nIf top level name was derived from filename, filename");
  fprintf(stdout,  " should be changed.\n");

  newname = (void *)malloc(strlen(iname) +2);
  strcpy(newname, "_");
  strcat(newname, iname);

  convert_instance_scopings(ctl, interface, newname);
  set_string_property_value(ctl, interface, "name", newname); 
}
    
/***********************************************************************/
int main(int argc, char *argv[]) {

 list         ilup_top, cvt_top, ilup_tops; 
 void         *cvt_ctl;  /*used wholly within convert, for now */   
 void         *gen_ctl;  /*used wholly within generator, for now */ 
 void         *top_module;  /*used wholly within convert, for now */ 
 SCtl         *stub_ctl;
 Instance     *interface;
 listElement  *ptr, *ptr1; 
 int           i,j; 

 stub_ctl = (void *)calloc(1, sizeof(SCtl));

#ifdef CPLUSPLUSMAPPING_NAMESPACES
    stub_ctl->use_cpp_namespace = 1;
#endif

#ifdef CPLUSPLUSMAPPING_NESTEDCLASSES
    stub_ctl->use_cpp_classes = 1;
#endif

 if(argc < 2) { 
     fprintf(stderr, "\nStubber input must include at least one interface file to be stubbed.\n"); 
     exit(1); 
 }

 /* arg indicates function for name conversion */
 cvt_ctl = (void *)initialize_convert(make_cpp_name); 

 if(cvt_ctl == NULL) {
      fprintf(stderr,"System Error: iluptype convert");
      exit(1);
 }

 ilup_tops = new_list();

 for (i = 1; i < argc; i ++) {
    if( (ilup_top = ParseFile(argv[i])) == NULL) {   
      fprintf(stderr,"Couldn't find or parse %s", argv[i]);  
      exit(1);
     }
    /* add new interfaces to collection */
    for(ptr = ilup_top->head; ptr; ptr = ptr->next) {
      for(ptr1 = ilup_tops->head; ptr1; ptr1 = ptr1->next) {
         if(ptr->data == ptr1->data) break;
      }
      if(ptr1 == NULL) list_insert(ilup_tops, (void *)ptr->data);
    }
  }

  /* build module structures */
  for(ptr = ilup_tops->head; ptr != NULL; ptr = ptr->next) {
       top_module =  build_basic_module_structure((Interface)ptr->data);
       /* sort for forward defs only */
       top_fwd_def_sort(top_module); 
  }

  cvt_top =  cvt_interfaces(cvt_ctl, ilup_tops); 

  if(cvt_top == 0) exit(1); 


#ifdef DEBUG 
  print_instances(cvt_ctl, stdout, cvt_top);
#endif

 gen_ctl = (void *)initialize_gen(struct_type_count, stub_ctl);


 for(ptr = cvt_top->head; ptr != NULL; ptr = ptr->next) {

    interface = (Instance *)ptr->data;
    /* if there is a problem with interface name = name of ctned class*/ 
    /* revise interface name (as used as class or namespace name)    */ 
    if( stub_ctl->use_cpp_namespace || stub_ctl->use_cpp_classes)
       module_name_check_adjust(gen_ctl, interface, "C++");  

   /* 10 just to avoid runaway on error */
    for(j = 0; j<10 ;j++) {   
      if(RuleSets[j].in_rule_file_name == NULL) break;
      /* functions will reference first set of property descriptors */ 
      /* should change these to make global                         */
      if(!produce_file(gen_ctl, &Functions[0], &PropDescs0[0],
           &RuleSets[j], (Instance *)ptr->data))
      exit(1); 
    }
 }
 exit(0); 

 return 0; /* just to forestall possible warning */

}
