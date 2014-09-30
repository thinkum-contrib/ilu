#include <stdio.h>
#include "SunPortMapper.h"

ilu_boolean
  ilu_portmapper_register (ILU_C_Object *obj, ILU_C_ENVIRONMENT *env)
{
  SunPortMapper_T	portmapper = ILU_NIL;
  SunPortMapper_Mapping	mapping;
  char *		portmapper_sbh = ILU_NIL;
  char *		object_sbh;
  char *		tinfo[] = { "sunrpcrm", NULL, NULL };
  char			object_hostname[128];
  char			tinfo_with_host[200];
  char			sid[1000], ih[1000], mstid[100];
  unsigned int		port;
  ILU_C_ENVIRONMENT	lenv;

  object_sbh = ILU_C_SBHOfObject(obj);
  if (sscanf(object_sbh, "ilu:%[^/]/%[^;];%[^;];sunrpc_2_%lu_%lu@sunrpcrm=tcp_%[^_]_%u",
	     sid, ih, mstid,
	     &mapping.program_number, &mapping.version_number,
	     object_hostname, &mapping.unix_port) != 7) {
    ILU_C_RAISE_SYSTEM(env,BAD_PARAM,((int)ilu_bpm_SBH),NO);
    return ilu_FALSE;
  }
  SunPortMapper__Initialize();
  sprintf (tinfo_with_host, "tcp_%s_111", object_hostname);
  tinfo[1] = tinfo_with_host;
  portmapper_sbh = ILU_C_FormSBH ("SunPortMapper", "T", SunPortMapper_T__MSType,
				  SunPortMapper_T__MSType->cl_singleton, tinfo, env);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ilu_FALSE;
  }
  portmapper = ILU_C_SBHToObject(portmapper_sbh, SunPortMapper_T__MSType, env);
  ilu_free(portmapper_sbh);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ilu_FALSE;
  }
  mapping.protocol_type = SunPortMapper_TCP_Protocol;
  SunPortMapper_T_Set (portmapper, &mapping, env);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ilu_FALSE;
  };
  CORBA_Object_release(portmapper, env);
  return ILU_C_SUCCESSFUL(env);
}

ILU_C_Object *
  ilu_portmapper_bind (char *hostname, ilu_Class type, ILU_C_ENVIRONMENT *env)
{
  char *		object_sbh;
  char *		portmapper_sbh;
  char *		tinfo[3] = { "sunrpcrm", ILU_NIL, ILU_NIL };
  char			tinfo_with_host[200];
  SunPortMapper_T	portmapper = ILU_NIL;
  SunPortMapper_Mapping	mapping;
  ilu_cardinal		port;
  ilu_Error		kerr;
  ILU_C_Object *	lobj;

  if ((type == ILU_NIL) ||
      (type->cl_singleton == ILU_NIL) ||
      (sscanf (type->cl_singleton, "sunrpc_2_%lu_%lu",
	       &mapping.program_number, &mapping.version_number) != 2)) {
    ILU_C_RAISE_SYSTEM(env,BAD_PARAM,((int)ilu_bpm_typeID),NO);
    return ILU_NIL;
  };

  SunPortMapper__Initialize();
  sprintf (tinfo_with_host, "tcp_%s_111", hostname);
  tinfo[1] = tinfo_with_host;
  portmapper_sbh = ILU_C_FormSBH ("SunPortMapper", "T", SunPortMapper_T__MSType,
				  SunPortMapper_T__MSType->cl_singleton, tinfo, env);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ILU_NIL;
  }
  portmapper = ILU_C_SBHToObject(portmapper_sbh, SunPortMapper_T__MSType, env);
  ilu_free(portmapper_sbh);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ILU_NIL;
  }
  mapping.protocol_type = SunPortMapper_TCP_Protocol;
  port = SunPortMapper_T_GetPort (portmapper, &mapping, env);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ILU_NIL;
  };
  CORBA_Object_release(portmapper, env);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ILU_NIL;
  };
  sprintf (tinfo_with_host, "tcp_%s_%u", hostname, port);
  object_sbh = ILU_C_FormSBH (type->cl_name, "T", type, type->cl_singleton, tinfo, env);
  if (!ILU_C_SUCCESSFUL(env)) {
    return ILU_NIL;
  };
  lobj = ILU_C_SBHToObject (object_sbh, type, env);
  ilu_free(object_sbh);
  return lobj;
}

