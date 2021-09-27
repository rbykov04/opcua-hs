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

  let ids = [29, 32, 76, 69, 85]
  Right c <- read_class_service client (map (UaNodeIdNum 0 0) ids)
  value <- sequence $ map f ids
  putStrLn $ show (map (("  "++). show) (zip value c))

  value <- read_value_service client [
                    (UaNodeIdNum 0 0 2261),
                    (UaNodeIdNum 0 0 2263)
                  ]
  putStrLn $  show value

  classes <- read_class_service client [
                    (UaNodeIdNum 0 0 10),
                    (UaNodeIdNum 0 0 2253),
                    (UaNodeIdNum 0 0 2261),
                    (UaNodeIdNum 0 0 2263)
                  ]


  putStrLn $  show classes

  server_obj <- browse_service client (UaNodeIdNum 0 0 2253)
  putStrLn $  show server_obj

  ua_client_delete client
