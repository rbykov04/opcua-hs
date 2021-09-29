module Main where
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Data.Int
import Foreign.Marshal.Alloc
import OpcUa.Types
import OpcUa.Bindings
import OpcUa.Storable
import OpcUa.Services

uri :: String
uri = "opc.tcp://localhost:4840"
create_client = do
  client <- ua_client_new
  code <- ua_client_config_set_default (ua_client_get_config client)

  endpoint <- newCString uri
  code <- ua_client_connect client endpoint
  free endpoint
  return client

main = do
  client <- create_client
  server_obj <- browse_service client (UaNodeIdNum 0 0 85)
  putStrLn $  show server_obj

  ua_client_delete client
