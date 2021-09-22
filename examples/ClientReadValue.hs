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

main = do
  putStrLn $ "Run client on " ++ uri

  client <- ua_client_new
  code <- ua_client_config_set_default (ua_client_get_config client)

  endpoint <- newCString uri
  code <- ua_client_connect client endpoint
  free endpoint

  let f = ns0_read_value client
  value <- sequence $ map f [2262..2267]
  putStrLn $ show value

  let read_class id = read_attr_class client (UaNodeIdNum 0 0 id) 2 id_NodeClassType
  let ids = [29, 32, 76, 69, 85]
  c <- sequence $ map read_class ids
  value <- sequence $ map f ids
  putStrLn $ show (map (("  "++). show) (zip value c))




  ua_client_delete client
