#include "opcua.h"
#include <stdio.h>

#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/plugin/log_stdout.h>

#include <stdlib.h>

UA_StatusCode UA_Client_readValueAttribute2(
  UA_Client *client, const UA_NodeId *nodeId, void *out);

UA_StatusCode UA_Client_readValueAttribute2(
  UA_Client *client, const UA_NodeId *nodeId, void *out) {

    UA_StatusCode c = UA_Client_readValueAttribute(client, *nodeId, out);
    return c;
}
