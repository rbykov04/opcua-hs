--{-# INCLUDE "opcua.h" #-}
module OpcUa.Services where
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.Int
import Control.Applicative

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

statusToText :: Int32 -> IO String
statusToText x = do
    str <- ua_status_code_name x
    s <-peekCAString str
    return s

_UA_ATTRIBUTEID_VALUE :: Int32
_UA_ATTRIBUTEID_VALUE = 13

_UA_ATTRIBUTEID_NODECLASS :: Int32
_UA_ATTRIBUTEID_NODECLASS = 2

class ReadService a where
    attributeId_ ::a-> Int32

    convertTo   :: UaVariantStruct -> IO (Either String a)

    read_service ::
      Ptr UaClient -> [UaNodeId] -> IO (Either String [Either String a])

newtype UaReadValue = UaReadValue {getUaReadValue:: UaVariant}

instance ReadService UaNodeClass where
    attributeId_ = const _UA_ATTRIBUTEID_NODECLASS

    convertTo dv = do
        let ptr = getVariantData dv
        value <- (peek. castPtr) ptr
        case toUaClass value of
          Right c -> return (Right c)
          Left e -> return (Left e)

    read_service client ids = do
      Just request_t  <- find_uatype_ptr (UaNodeIdNum 0 0 629)
      Just response_t <- find_uatype_ptr (UaNodeIdNum 0 0 632)

      request <- calloc
      let read_ids =  map (\id ->(id, attributeId_ UaObjectClass)) ids
      poke request (UaReadRequest (map (\(id, attrid) -> UA_ReadValueId id attrid) read_ids))

      response <- calloc
      poke response (UaReadResponse {})

      ua_client_service client (castPtr request) request_t (castPtr response) response_t
      r <- peek response

      let status = (serviceResult. responseHeader) r
      if status == 0
        then do
          let count = resultsSize r
          vs <- peekArray count $ castPtr $resultValues r

          let ss = map getUaDataValueStatus vs
          kk   <-   sequence $ map statusToText ss
          vars <-   sequence $ map (convertTo . getUaDataValueVariant) vs

          return $Right vars
        else do
          status <- (statusToText) status
          return $Left $ "status: " ++  status



instance ReadService UaReadValue where
    attributeId_ = const _UA_ATTRIBUTEID_VALUE
    convertTo str = do
      v <-read_variant_ str
      case v of
        Left e -> return $ Left e
        Right var -> return $ Right $ UaReadValue var

    read_service client ids = do
      Just request_t  <- find_uatype_ptr (UaNodeIdNum 0 0 629)
      Just response_t <- find_uatype_ptr (UaNodeIdNum 0 0 632)

      request <- calloc
      let read_ids =  map (\id ->(id, attributeId_ (UaReadValue (UaString "")))) ids
      poke request (UaReadRequest (map (\(id, attrid) -> UA_ReadValueId id attrid) read_ids))

      response <- calloc
      poke response (UaReadResponse {})

      ua_client_service client (castPtr request) request_t (castPtr response) response_t
      r <- peek response

      let status = (serviceResult. responseHeader) r
      if status == 0
        then do
          let count = resultsSize r
          vs <- peekArray count $ castPtr $resultValues r

          let ss = map getUaDataValueStatus vs
          kk   <-   sequence $ map statusToText ss
          vars <-   sequence $ map (convertTo . getUaDataValueVariant) vs

          return $Right vars
        else do
          status <- (statusToText) status
          return $Left $ "status: " ++  status

(...) :: (c -> d) -> ( a -> b -> c) -> a -> b -> d
(...) f g a b = f (g a b)

read_value_service :: Ptr UaClient -> [UaNodeId] -> IO (Either String [Either String UaVariant])
read_value_service = ((fmap . fmap . fmap . fmap) getUaReadValue) ... read_service
                    --  IO    Either  []    Either

read_class_service :: Ptr UaClient -> [UaNodeId] -> IO (Either String [Either String UaNodeClass])
read_class_service = read_service

read_variant_ :: UaVariantStruct ->  IO (Either String UaVariant)
read_variant_ value = do
        t <- get_data_value (getVariantType value)
        case t of
          Right (UaDataType typeid)  -> do
            v <- convert typeid (getVariantData value)
            case v of
              Right var -> return (Right var)
              Left e -> return (Left e)
          Left t -> return (Left t)



read_value :: Ptr UaClient -> UaNodeId -> IO (Either String UaVariant)
read_value client id = do
  result <- read_value_service client [id]
  case result of
    Left e -> return $ Left e
    Right [] -> return $ Left "result is empty"
    Right (x:_) -> return $ x

print_time :: UaDateTimeStruct -> IO ()
print_time time = do
        putStr $ "haskell result time: "
                ++ show (getMinutes time)++":"
                ++ show (getSeconds time) ++ "\n"

readAndShowValue :: Ptr UaClient -> UaNodeId -> IO (UaVariant)
readAndShowValue client id = do
        Right value <- read_value client id
        putStrLn $ show value
        return value

ns0_read_value :: Ptr UaClient -> Int32 -> IO (Either String UaVariant)
ns0_read_value client id = read_value client (UaNodeIdNum 0 0 id)

