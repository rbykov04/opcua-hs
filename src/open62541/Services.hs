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


get_data_value :: Ptr UaDataType -> IO (Either String UaDataType)
get_data_value ptr | ptr == nullPtr = return $ Left "data-type ptr is null"
get_data_value ptr | otherwise = do
    v <- peek ptr
    return (Right v)

convert :: UaNodeId -> Ptr UaVariant -> IO (Either String UaVariant)
convert typeid ptr | typeid == id_String   = asString ptr
convert (UaNodeIdNum _ _ x) _ = return (Left "unknown type")
--convert _ _  = return (UaError "typeid is not type")

read_variant :: Ptr UaClient -> UaNodeId -> IO (Either String UaVariant)
read_variant client id = do
  id_ptr <- malloc
  poke id_ptr id

  value_ptr    <- malloc
  status <- ua_read_value client id_ptr value_ptr
  free id_ptr

  if status == 0
    then do
        value <- peek value_ptr
        free value_ptr

        t <- get_data_value (getVariantType value)
        case t of
          Right (UaDataType typeid)  -> do
            v <- convert typeid (getVariantData value)
            return v
          Left t -> return (Left t)
    else do
        free value_ptr
        return $ Left $ "read value status: " ++ show status

read_value :: Ptr UaClient -> UaNodeId -> IO (Either String UaVariant)
read_value = read_variant

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

