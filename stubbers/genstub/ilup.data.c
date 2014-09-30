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


#include <stddef.h>
#include "data.h"

TypeDesc ST_TYPES[ ] = { 

   { "Any", 0, { 
                   { "dummy",      PropString, NULL},
                   { "last",       PropLast, NULL}
               }
    },


    { "Interface", 0, {
                     { "name",       PropString, NULL} , 
                     { "isl_name",  PropString, NULL} , 
                     { "flat_name",  PropString, NULL} , 
                     { "module_str" ,PropStruct, "Module"},
                     { "brand"      , PropString, NULL},
                     { "ilup_ref"   , PropOpaque, NULL},
                     { "types"      , PropList,   NULL},
                     { "classes"    , PropList,   "Object" },
                     { "exceptions" , PropList,  "Exception" },
                     { "imports"    , PropList,  "Interface" }, 
                     { "constants"  , PropList,  "Constant"}, 
                     { "filename"   , PropString, NULL},
                     { "u_attach"   , PropOpaque, NULL},
                     { "last",       PropLast, NULL}
                   }
    }, 

   /* added, not in iluptype */ 
   {"ContentElem", 0, {
                  { "content_type",       PropString, NULL},
                  { "content",           PropStruct, "Any" },
                  { "last",               PropLast, NULL}
                }
    },
 
   /* added, not in iluptype */ 
    { "Module", 0, {
                  { "simple_name"         ,PropString, NULL},
                  { "isl_name"            ,PropString, NULL},
                  { "scoping"             ,PropList,  "NameMember"},
                  { "contained_modules"   ,PropList,  "Module"},
                  { "contained_types"     ,PropList,  "Any"},
                  { "contained_constants" ,PropList,  "Constant"},
                  { "contained_exceptions",PropList,  "Exception"},
                  { "content_sequence"    ,PropList,  "ContentElem"},
                  { "module_object"       ,PropStruct, "Any"},   
                  { "last",                PropLast,   NULL}
                }
    },
              
    { "Exception", 1, {
                 { "name"          , PropString,  NULL} , 
                 { "isl_name"      , PropString,  NULL} , 
                 { "flat_name"     , PropString,  NULL} , 
                 { "scoping"       , PropList,   "NameMember"}, 
                 { "ilup_ref"      , PropOpaque,  NULL},
                 { "type"          , PropStruct,  "Any"},  
                 { "valueoptional" , PropBool,    NULL}, 
                 { "builtin"       , PropBool,    NULL},
                 { "protocolid"    , PropInt,     NULL},
                 { "importedfrom"   , PropString, NULL},
                 { "importedexception", PropStruct, "Exception"},
                 { "interface"     , PropStruct,  "Interface"},
                 { "corba_rep_id"  , PropString,  NULL},
                 { "doc_string"    , PropString,  NULL},
                 { "u_attach"      , PropOpaque,  NULL},
                 { "last"          , PropLast, NULL}
               }
     }, 

    { "Constant", 2,  {
                 { "name",      PropString,    NULL},
                 { "isl_name",  PropString,    NULL},
                 { "flat_name",  PropString,    NULL},
                 { "scoping"   , PropList,    "NameMember"}, 
                 { "ilup_ref"  , PropOpaque,   NULL},
                 { "type"      , PropStruct,   "Any"}, 
                 { "importedfrom", PropString, NULL},
                 { "importedconstant", PropStruct, "Constant"},
                 { "interface"     , PropStruct,  "Interface"},
                 { "value"     , PropStruct,  "Any"},  
                 { "u_attach"  , PropOpaque,   NULL},
                 { "last"      ,PropLast, NULL},
                }
    },


    /* In type lists */
    { "FixedPoint", 3, {
                 { "name"         , PropString,   NULL}, 
                 { "isl_name"    , PropString,  "NULL"},  
                 { "flat_name"    , PropString,  "NULL"},  
                 { "scoping"      , PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,      NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,   NULL},
                 { "explicit_uid" , PropBool,     NULL},
                 { "brand"        , PropString,   NULL},
                 { "ilup_ref"     , PropOpaque,   NULL},
                 { "min_numerator", PropStruct,  "IntegerLiteral"},
                 { "max_numerator", PropStruct,  "IntegerLiteral"},
                 { "denominator"  , PropStruct,  "IntegerLiteral"},
                 { "fixed_digits"  ,PropInt,      NULL},
                 { "fixed_decimal_places", PropInt,  NULL},
                 { "fixed_range_size", PropInt,    NULL},  /*ENUM? */
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         ,PropLast, NULL}
                }
    },
    { "StringType", 4, {
                 { "name",          PropString,  NULL},   
                 { "isl_name"     , PropString,  "NULL"},  
                 { "flat_name"    , PropString,  "NULL"},  
                 { "scoping"      , PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,      NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "max_length"   , PropLongInt,   NULL},
                 { "charset"      , PropInt,       NULL},
                 { "language"     , PropString,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
                }
    },
    { "Object", 5, {
                 { "name",          PropString,   NULL},   /* simple name */
                 { "isl_name"      , PropString,  NULL},  
                 { "flat_name"     , PropString,  "NULL"},  
                 { "scoping"       ,PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "singleton"    , PropString,    NULL},
                 { "authentication",PropString,    NULL},
                 { "corba_rep_id"  ,PropString,    NULL},
                 { "doc_string"    ,PropString,    NULL},
                 { "collectible"  , PropBool,      NULL}, 
                 { "optional"     , PropBool,      NULL}, 
                 { "sealed"       , PropBool,      NULL}, 
                 { "local"        , PropBool,      NULL}, 
                 { "superclasses" , PropList,      NULL}, /* can be rec? */ 
                 { "methods"      , PropList,     "Method"}, 
                 { "state"        , PropList,     "StateMbr"}, 
                 { "equiv_module" , PropStruct,   "Module"},
                 { "doc_string"   , PropString,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
             }
    },
    { "Alias",  6, { 
                 { "name",          PropString,    NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
                }
    },
    { "Pickle", 7, {
                 { "name",          PropString,    NULL},  /* simple name */
                 { "isl_name"    , PropString,    NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct ,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
               }
    },
    { "Union",  8, { 
                 { "name",           PropString,   NULL},  /* simple name */
                 { "isl_name"     , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,     "NameMember"}, 
                 { "builtin"       , PropInt,       NULL},  
                 { "supertype"     , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"           , PropString,    NULL},
                 { "explicit_uid"  , PropBool,      NULL},
                 { "brand"         , PropString,    NULL},
                 { "ilup_ref"      , PropOpaque,    NULL},
                 { "discrim_type"  , PropStruct,   "Any"},  
                 { "arms"          , PropList,     "Arm"},    
                 { "default_arm"   , PropStruct,   "Arm"},   
                 { "others_allowed", PropBool,      NULL},
                 { "u_attach"      , PropOpaque,    NULL},
                 { "last"          , PropLast, NULL}
                }
    }, 
    { "Sequence", 9, {
                 { "name",          PropString,   NULL},   /* simple name */
                 { "isl_name"    , PropString,    NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,      NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "oftype"       , PropStruct,   "Any"},     
                 { "optional"     , PropBool,      NULL},
                 { "limit"        , PropLongInt,   NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
               }
    }, 
    { "Pipe", 10,   { 
                 { "name",          PropString,    NULL},  /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "sink_p"       , PropBool,      NULL},
                 { "optional"     , PropBool,      NULL}, 
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast,      NULL}
                 }
    }, 

    { "Record", 11, { 
                 { "name",          PropString,    NULL},  /* simple name */
                 { "isl_name"    , PropString,      NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,      "NameMember"}, 
                 { "builtin"      , PropInt,        NULL},  
                 { "supertype"    , PropStruct,     "Any"}, 
                 { "importedfromname", PropString,  NULL},
                 { "typeininterface", PropStruct,   "Interface"}, 
                 { "uid"          , PropString,     NULL},
                 { "explicit_uid" , PropBool,       NULL},
                 { "brand"        , PropString,     NULL},
                 { "ilup_ref"     , PropOpaque,     NULL},
                 { "fields"       , PropList,      "Argument"}, 
                 { "extensible"   , PropBool,       NULL},
                 { "supertype"    , PropStruct,     "Any"}, 
                 { "u_attach"     , PropOpaque,     NULL},
                 { "last"         , PropLast,    NULL}
                }
    },
    { "Array", 12, {  
                 { "name",          PropString,    NULL},  /* simple name */
                 { "isl_name"    , PropString,      NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"       ,PropList,      "NameMember"}, 
                 { "builtin"      , PropInt,        NULL},  
                 { "supertype"    , PropStruct,     "Any"}, 
                 { "importedfromname", PropString,  NULL},
                 { "typeininterface", PropStruct,   "Interface"}, 
                 { "uid"          , PropString,     NULL},
                 { "explicit_uid" , PropBool,       NULL},
                 { "brand"        , PropString,     NULL},
                 { "ilup_ref"     , PropOpaque,     NULL},
                 { "oftype"       , PropStruct,    "Any"},     
                 { "dimensions"   , PropList,      "DimInteger"}, 
                 { "optional"     , PropBool,       NULL}, 
                 { "u_attach"     , PropOpaque,     NULL},
                 { "last"         , PropLast,    NULL}
               }
    }, 
    { "Enumeration", 13, {
                 { "name"         , PropString,   NULL},  /* simple name */
                 { "isl_name"    , PropString,    NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,      NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "values"       , PropList,     "EnumField"},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast,  NULL}
               }
    }, 
    { "Optional",  14,  {
                 { "name"         , PropString,   NULL},   /* simple name */
                 { "isl_name"    , PropString,    NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,      NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "oftype"       , PropStruct,    "Any"}, /*where def*/
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast,   NULL}
               }
    }, 
    { "Reference", 15, 
               {
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "optional"     , PropBool,      NULL}, 
                 { "aliased"      , PropBool,      NULL}, 
                 { "base_type"    , PropStruct,    "Any"}, 
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
                }
    },

    {"Void", 16, { 
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"    , PropInt,         NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },


    {"Byte", 17, { 
                 { "name",        PropString,     NULL},   /* simple name */
                 { "isl_name"    , PropString,    NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,      NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,   NULL},
                 { "explicit_uid" , PropBool,     NULL},
                 { "ilup_ref"     , PropOpaque,   NULL},
                 { "brand"        , PropString,   NULL},
                 { "u_attach"     , PropOpaque,   NULL},
                 { "last"         , PropLast, NULL}
              }
    },
             
    {"Boolean", 18, {  
                 { "name",        PropString,     NULL},  /* simple name */
                 { "isl_name"    , PropString,    NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,    "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,   "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"Character", 19, {  
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"ShortCharacter", 20, {  
                 { "name",        PropString,      NULL},  /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"ShortInteger", 21,  {
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"Integer", 22, {
                 { "name",        PropString,      NULL},  /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"LongInteger", 23, {
                 { "name",        PropString,      NULL},  /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"ShortCardinal", 24, { 
                 { "name",        PropString,      NULL},  /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"Cardinal", 25, { 
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },
    {"LongCardinal", 26,  {
                 { "name",        PropString,      NULL},  /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
     },

    {"ShortReal", 27, { 
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"Real", 28, {
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },

    {"LongReal", 29, {
                 { "name",        PropString,      NULL}, /* simple name */
                 { "isl_name"    , PropString,     NULL},  
                 { "flat_name"    , PropString,  NULL},  
                 { "scoping"      , PropList,     "NameMember"}, 
                 { "builtin"      , PropInt,       NULL},  
                 { "supertype"    , PropStruct,    "Any"}, 
                 { "importedfromname", PropString, NULL},
                 { "typeininterface", PropStruct, "Interface"}, 
                 { "uid"          , PropString,    NULL},
                 { "explicit_uid" , PropBool,      NULL},
                 { "brand"        , PropString,    NULL},
                 { "ilup_ref"     , PropOpaque,    NULL},
                 { "u_attach"     , PropOpaque,    NULL},
                 { "last"         , PropLast, NULL}
              }
    },


    /* from here lower level structs */
    {"Method" , 30, { 
                 { "name",           PropString,  NULL},   
                 { "isl_name",      PropString,  NULL},   
                 { "ilup_ref"      , PropOpaque,  NULL},
                 { "arguments"     , PropList,   "Argument"}, 
                 { "returntype"    , PropStruct,  "Any"},
                 { "returnoptional", PropBool,    NULL},
                 { "exceptions"    , PropList,   "Exception"}, 
                 { "protocolid"    , PropInt,     NULL},
                 { "ofobject"      , PropStruct,  "Any"},
                 { "ofinterface"   , PropStruct, "Interface"},
                 { "functional"    , PropBool,    NULL}, 
                 { "asynch"        , PropBool,     NULL}, 
                 { "authentication_type", PropString, NULL},
                 { "doc_string"    , PropString,    NULL},
                 { "u_attach"      , PropOpaque,    NULL},
                 { "last"          , PropLast, NULL}
                }
    },

    {"Argument" , 31 ,{
                 { "name",           PropString,    NULL},   
                 { "isl_name",      PropString,    NULL},   
                 { "ilup_ref"      , PropOpaque,    NULL},
                 { "interface"     , PropStruct,   "Interface"}, /*wh def*/
                 { "type"          , PropStruct,    "Any"}, 
                 { "direction"     , PropString,    NULL},
                 { "sibling"       , PropBool,      NULL},
                 { "u_attach"      , PropOpaque,    NULL},
                 { "last"          , PropLast, NULL}
               }
    },

    { "Arm" , 32 , {
                 { "name",           PropString,    NULL},   
                 { "isl_name",      PropString,    NULL},   
                 { "ilup_ref"      , PropOpaque,    NULL},
                 { "interface"     , PropStruct,   "Interface"}, /*whe def*/
                 { "type"          , PropStruct,    "Any"},
                 { "values"        , PropList,     "Any"},  
                 { "u_attach"      , PropOpaque,    NULL},
                 { "last"          , PropLast, NULL}
               }
    },

    { "StateMbr", 33, {  
                 { "name",           PropString,    NULL},   
                 { "isl_name",      PropString,    NULL},   
                 { "ilup_ref"      , PropOpaque,    NULL},
                 { "interface"     , PropStruct,  "Interface"}, /*where def*/
                 { "type"          , PropStruct,   "Any"},
                 { "private"       , PropBool,     NULL},
                 { "u_attach"      , PropOpaque,   NULL},
                 { "last"          , PropLast, NULL}
               }
    },
#ifdef undef
    {"Field" , 34, {
                 { "name",           PropString,    NULL},   
                 { "interface"     , PropStruct,  "Interface"}, /*where def*/
                 { "type"          , PropStruct,  "Any"}, 
                 { "last"          , PropLast,   NULL}
               }
    },
#endif

    {"IntegerLiteral", 35, {
                { "small"        , PropBool,     NULL},  
                { "negative"     , PropBool,     NULL}, 
                { "direct"       , PropLongInt,  NULL},
                { "string"       , PropString,   NULL},
                { "last"         , PropLast,   NULL}
               }
    },

    {"EnumField", 36, {
               { "name"         , PropString, NULL}, 
               { "isl_name"     , PropString, NULL}, 
               { "id"           , PropInt,    NULL}, 
               { "last"         , PropLast, NULL}
              }
    },

    {"BoolConstantValue", 37,  {
              { "value"     ,PropBool,     NULL},  /* must allow long */ 
              { "last"      ,PropLast, NULL}
            }   
     },
            
    {"IntConstantValue", 38, {
              { "positive"  ,PropBool,     NULL},  
              { "value"     ,PropLongInt,  NULL},  
              { "last"     , PropLast, NULL}
            }   
     },

    {"RealConstantValue", 39, {
              { "positive"  ,PropBool,     NULL},  
              { "value"     ,PropString,   NULL},   
              { "fraction"  ,PropString,   NULL},  
              { "exponent"  ,PropLongInt,   NULL},  
              { "last"     , PropLast, NULL}
            }   
     },

    {"ILUCStringConstantValue", 40, {
              { "value"      ,PropString,    NULL},  
              { "last"      , PropLast, NULL}
            }   
     },

    {"EnumConstantValue", 41, {
              { "value"     ,PropString,    NULL},  
              { "last"      , PropLast, NULL}
            }   
    },

    {"DimInteger", 42, {
              { "intvalue" ,PropInt, NULL},   
              { "last"     , PropLast, NULL},
         }
     },

    {"NameMember", 43, {
              { "namevalue"      ,PropString, NULL},   
              { "isl_name"      ,PropString, NULL},   
              { "is_real_class" , PropInt, NULL}, 
              { "last"          ,PropLast, NULL},
         }
     },

    {"Imported", 44, {
              { "name"     ,PropString, NULL},   
              { "isl_name" ,PropString, NULL},   
              { "filename" ,PropString, NULL},   
              { "last"     , PropLast, NULL},
         }
     },

    {"LangName", 45, {
              { "language" ,PropString, NULL},   
              { "name"     ,PropString, NULL},   
              { "last"     , PropLast, NULL},
         }
     },

   {"LastTypeDesc", 46,  {
              { "last"      , PropLast, NULL},
        }
    },
};
                 

int dummy1() {
    return 1;
}
