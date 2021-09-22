--{-# INCLUDE "opcua.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module OpcUa.Bindings where

import Foreign
import Foreign.C

import OpcUa.Types


foreign import ccall "UA_Client_new"              ua_client_new :: IO (Ptr UaClient)
foreign import ccall "UA_Client_delete"           ua_client_delete :: Ptr UaClient -> IO ()
foreign import ccall "UA_Client_getConfig"        ua_client_get_config ::Ptr UaClient -> Ptr UaClientConfig
foreign import ccall "UA_ClientConfig_setDefault" ua_client_config_set_default :: Ptr UaClientConfig -> IO (Int32)
foreign import ccall "UA_Client_connect"          ua_client_connect :: Ptr UaClient -> CString -> IO (Int32)

foreign import ccall "UA_Client_readValueAttribute2" ua_read_value
        :: Ptr UaClient -> Ptr UaNodeId -> Ptr UaVariant ->  IO (Int32)

foreign import ccall "UA_Client_readValueAttribute2" ua_read_value2
        :: Ptr UaClient -> Ptr UaNodeId -> Ptr UaVariantStruct ->  IO (Int32)
