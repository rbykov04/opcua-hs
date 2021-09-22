#include "opcua.h"
#include <stdio.h>

#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/plugin/log_stdout.h>

#include <stdlib.h>
void UA_DateTime_toStruct2(UA_DateTimeStruct *dst, UA_DateTime *t);

void UA_DateTime_toStruct2(UA_DateTimeStruct *dst, UA_DateTime *t) {
   UA_DateTimeStruct date = UA_DateTime_toStruct (*t);
   memcpy (dst, &date, sizeof (UA_DateTimeStruct));
}

UA_StatusCode UA_Client_readValueAttribute2(
  UA_Client *client, const UA_NodeId *nodeId, void *out);

UA_StatusCode UA_Client_readValueAttribute2(
  UA_Client *client, const UA_NodeId *nodeId, void *out) {

    UA_StatusCode c = UA_Client_readValueAttribute(client, *nodeId, out);
    return c;
}

void opcua_helper(UA_Client *client, UA_Variant *variant);

void opcua_test(int a) {
   printf("HELLO!! test\n");
    (void) a;
    UA_Client *client = UA_Client_new();
    UA_ClientConfig_setDefault(UA_Client_getConfig(client));

    UA_StatusCode retval = UA_Client_connect(client, "opc.tcp://localhost:4840");
    if(retval != UA_STATUSCODE_GOOD) {
       UA_Client_delete(client); /* Disconnects the client internally */
        return;
    }

    UA_Variant value; /* Variants can hold scalar values and arrays of any type */
    UA_Variant_init(&value);

    opcua_helper(client, &value);
    UA_Variant_clear(&value);
    UA_Client_delete(client); /* Disconnects the client internally */

}
void opcua_helper(UA_Client *client, UA_Variant *value) {
    UA_StatusCode retval;

    /* NodeId of the variable holding the current time */
    const UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME);
    retval = UA_Client_readValueAttribute(client, nodeId, value);
/*
    if(retval == UA_STATUSCODE_GOOD &&
       UA_Variant_hasScalarType(value, &UA_TYPES[UA_TYPES_DATETIME])) {
        UA_DateTime raw_date = *(UA_DateTime *) value->data;
        UA_DateTimeStruct dts = UA_DateTime_toStruct(raw_date);
        UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND, "date is: %u-%u-%u %u:%u:%u.%03u\n",
                    dts.day, dts.month, dts.year, dts.hour, dts.min, dts.sec, dts.milliSec);
    }
   * */

    return;
}
