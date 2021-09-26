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

foreign import ccall "__UA_Client_Service"  ua_client_service
        :: Ptr UaClient
        -> Ptr ()
        -> Ptr UaDataType
        -> Ptr ()
        -> Ptr UaDataType
        -> IO ()

foreign import ccall "UA_StatusCode_name"  ua_status_code_name ::  Int32 -> IO (CString)



foreign import ccall "get_UA_TYPES" ua_data_type :: Int32 -> IO (Ptr UaDataType)
