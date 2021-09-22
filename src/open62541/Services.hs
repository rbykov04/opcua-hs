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

find_uatype_ptr :: UaNodeId -> IO (Maybe (Ptr UaDataType))
find_uatype_ptr nodeid = do
  types  <- sequence $ map ua_data_type [1..200]
  typeids <- sequence $ map get_data_value types
  return ( lookup (Right (UaDataType nodeid)) (zip typeids types))

cast :: Ptr UaReadAttr -> IO (Ptr UaVariantStruct)
cast ptr  = return (castPtr ptr)

cast_int :: Ptr UaReadAttr -> IO (Ptr Int32)
cast_int ptr  = return (castPtr ptr)

read_attr_class :: Ptr UaClient -> UaNodeId -> Int32 -> UaNodeId ->IO (Either String UaReadAttr)
read_attr_class  client id attr id_type = do

  Just t <- find_uatype_ptr id_type

  id_ptr <- malloc
  value_ptr    <- malloc

  poke id_ptr id
  status <- ua_read_attribute client id_ptr attr value_ptr t
  free id_ptr
  if status == 0
    then do

        p <- cast_int value_ptr
        value <- peek p
        free value_ptr


        case toUaClass value of
              Right c -> return (Right (UaClass c))
              Left e -> return (Left e)
    else do
        free value_ptr
        return $ Left $ "read value status: " ++ show status

toUaClass :: Int32 -> Either String UaNodeClass
toUaClass c
  | c == 1    = Right UaObjectClass
  | c == 2    = Right UaVariableClass
  | c == 4    = Right UaMethodClass
  | c == 8    = Right UaObjectTypeClass
  | c == 16   = Right UaVariableTypeClass
  | c == 32   = Right UaRerenceTypeClass
  | c == 64   = Right UaDataTypeClass
  | c == 128  = Right UaViewClass
  | otherwise = Left $ "unknown class " ++ show c

read_attr_variant :: Ptr UaClient -> UaNodeId -> Int32 -> UaNodeId ->IO (Either String UaReadAttr)
read_attr_variant  client id attr id_type = do

  Just t <- find_uatype_ptr id_type

  id_ptr <- malloc
  value_ptr    <- malloc

  poke id_ptr id
  status <- ua_read_attribute client id_ptr attr value_ptr t
  free id_ptr
  if status == 0
    then do

        p <- cast value_ptr
        value <- peek p
        free value_ptr


        t <- get_data_value (getVariantType value)
        case t of
          Right (UaDataType typeid)  -> do
            v <- convert typeid (getVariantData value)
            case v of
              Right var -> return (Right (UaValue var))
              Left e -> return (Left e)
          Left t -> return (Left t)
    else do
        free value_ptr
        return $ Left $ "read value status: " ++ show status


read_value :: Ptr UaClient -> UaNodeId -> IO (Either String UaVariant)
read_value client id = do
  v <- read_attr_variant client id 13 id_VariantType
  case v of
    Left e -> return (Left e)
    Right (UaValue v) -> return (Right v)
    Right a -> return (Left $ "incorrect TypeAttr " ++ show a)

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

