#   File:       'C++ runtime.make'#   Target:     'C++ runtime'#   Sources:    ilu.cp iluLoop.cp iluObject.cp iluServer.cp#   Created:    Friday, July 23, 1993 2:41:16 PMOBJECTS = ilu.cp.o iluObject.cp.o iluServer.cp.oCPlusOptions = -r -model far -d MACOS -d SUNRPC_PROTOCOL -d REGISTRY_LAST_RESORT='""' -i :headers: -i ::'ILU kernel':'C++ runtime' �� 'C++ runtime.make' {OBJECTS}	Lib {OBJECTS}	�		-o 'C++ runtime'