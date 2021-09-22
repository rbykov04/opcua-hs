#include "opcua.h"
#include <stdio.h>

#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/plugin/log_stdout.h>

#include <stdlib.h>

const UA_DataType *get_UA_TYPES(int i);
const UA_DataType *get_UA_TYPES(int i) {
   return &UA_TYPES[i];
}
