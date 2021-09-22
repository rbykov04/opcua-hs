--{-# INCLUDE "opcua.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include <open62541/types.h>
#include <open62541/types_generated.h>
module OpcUa.Storable where
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Data.Int
import Foreign.Marshal.Alloc

import OpcUa.Types
import OpcUa.Bindings


get_type_value :: Ptr UaVariant -> IO (Either String Int32)
get_type_value variant = do
  t  <- #{peek UA_Variant, type} variant
  if t == nullPtr
    then return (Left "Ua_Variant.type == NULL")
    else do
         id <- #{peek UA_DataType, typeId.identifier} t
         return (Right id)


asString :: Ptr UaVariant -> IO (UaVariant)
asString ptr = do
    d <- #{peek UA_Variant, data} ptr
    if d == nullPtr then return (UaError "Ua_Variant.data == NULL")
    else do
        str <- #{peek UA_String, data} d
        if str == nullPtr then return (UaError "Ua_String.data == NULL")
        else do str <- peekCAString str
                return (UaString str)

instance Storable UaVariant where
    sizeOf    _ = (#size UA_Variant)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      id <- get_type_value ptr
      case id of
        Left v -> return (UaError v)
        Right typeid -> do
          if typeid == 13
            then do
                d <- #{peek UA_Variant, data} ptr
                x <- peek d
                (return . UaDateTime) x
            else if typeid == 12
            then do
                v <- asString ptr
                return v
             else do (return . UaUnknownType) typeid

    poke ptr _  = undefined

instance Storable UaVariantStruct where
    sizeOf    _ = (#size UA_Variant)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        typ          <- #{peek UA_Variant, type} ptr
        storageType  <- #{peek UA_Variant, storageType} ptr
        len          <- #{peek UA_Variant, arrayLength} ptr
        d            <- #{peek UA_Variant, data} ptr
        arrayDemSize <- #{peek UA_Variant, arrayDimensionsSize} ptr
        arrayDems    <- #{peek UA_Variant, arrayDimensions} ptr
        return (UaVariantStruct
                  typ storageType len d arrayDemSize arrayDems)

    poke ptr _  = undefined

instance Storable UaDataType where
    sizeOf    _ = (#size UA_DataType)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
         id         <- #{peek UA_DataType, typeId} ptr
         return (UaDataType id)

    poke ptr _  = undefined

instance Storable UaDateTimeStruct where
    sizeOf    _ = (#size UA_DateTimeStruct)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        minutes <- #{peek UA_DateTimeStruct, min} ptr
        sec <- #{peek UA_DateTimeStruct, sec} ptr
        return (UaDateTimeStruct minutes sec)
    poke ptr _  = undefined


instance Storable UaNodeId where
    sizeOf    _ = (#size UA_NodeId)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        sp <- (#peek UA_NodeId, namespaceIndex) ptr
        t  <- (#peek UA_NodeId, identifierType ) ptr
        id <- (#peek UA_NodeId, identifier.numeric) ptr
        return (UaNodeIdNum sp t id)

    poke ptr (UaNodeIdNum space t id) = do
      (#poke UA_NodeId, namespaceIndex) ptr space
      (#poke UA_NodeId, identifierType ) ptr t
      (#poke UA_NodeId, identifier.numeric) ptr id
