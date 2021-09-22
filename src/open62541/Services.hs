--{-# INCLUDE "opcua.h" #-}
module OpcUa.Services where
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Data.Int
import Foreign.Marshal.Alloc

import OpcUa.Types
import OpcUa.Bindings
import OpcUa.Storable

check_type :: Ptr UaClient -> UaNodeId -> IO ()
check_type client id = do
  (Right v) <- read_variant client id
  id <-get_type_value v
  putStrLn$ "type is " ++ show id
  free v

check_data_value :: Ptr UaDataType -> IO ()
check_data_value ptr = do
    if ptr == nullPtr
        then putStrLn "ptr is null"
        else do
            v <- peek ptr
            putStrLn $ show v

read_variant :: Ptr UaClient -> UaNodeId -> IO (Either String (Ptr UaVariant))
read_variant client id = do
  id_ptr <- malloc
  poke id_ptr id

  value    <- malloc
  valuestr <- malloc
  status <- ua_read_value  client id_ptr value
  status <- ua_read_value2 client id_ptr valuestr
  e <- peek valuestr
  check_data_value (getVariantType e)
  putStrLn $ "struct " ++ show e
  free id_ptr
  if status == 0
    then return (Right value)
    else do
       free value
       (return. Left .show) status

read_value :: Ptr UaClient -> UaNodeId -> IO (Either String UaVariant)
read_value client id = do
  v <- read_variant client id
  case v of
    Right var -> do
      value <- peek var
      free var
      return (Right value)
    Left code -> do return (Left code)

print_time :: UaDateTimeStruct -> IO ()
print_time time = do
        putStr $ "haskell result time: "
                ++ show (getMinutes time)++":"
                ++ show (getSeconds time) ++ "\n"

idBuildInfoName   = UaNodeIdNum 0 0 2261
idStartTime       = UaNodeIdNum 0 0 2257
idCurrentTime     = UaNodeIdNum 0 0 2258

readAndShowValue :: Ptr UaClient -> UaNodeId -> IO (UaVariant)
readAndShowValue client id = do
        Right value <- read_value client id
        putStrLn $ show value
        return value

ns0_read_value :: Ptr UaClient -> Int32 -> IO (Either String UaVariant)
ns0_read_value client id = read_value client (UaNodeIdNum 0 0 id)

opcua_test :: Int -> IO ()
opcua_test  i = do
  client <- ua_client_new
  code <- ua_client_config_set_default (ua_client_get_config client)
  putStr $ "code: SetConfig default " ++ show code ++ "\n"

  endpoint <- newCString "opc.tcp://localhost:4840"
  code <- ua_client_connect client endpoint

  let f = ns0_read_value client

  value <- sequence $ map f [2262..2267]
  putStrLn $ show (sequence value)


  ua_client_delete client
  free endpoint
