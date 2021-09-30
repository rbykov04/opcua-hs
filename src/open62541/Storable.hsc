--{-# INCLUDE "opcua.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include <open62541/client.h>
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

asString :: Ptr UaVariant -> IO (Either String UaVariant)
asString d = do
    if d == nullPtr then return (Left "Ua_Variant.data == NULL")
    else do
        str <- #{peek UA_String, data} d
        if str == nullPtr then return (Left "Ua_String.data == NULL")
        else do str <- peekCAString str
                return (Right $ UaString str)

instance Storable UaReadAttr where
    sizeOf _ = (#size UA_Variant) -- alloc max
    alignment _ = alignment (undefined :: CString)
    peek _ = undefined
    poke _ _ = undefined


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

data UaExtensionObject =UaExtensionObject
  deriving Show
data UaDateTime =UaDateTime
  deriving Show
data UaDiagnosticInfo=UaDiagnosticInfo
  deriving Show
data UaStringStruct=UaStringStruct
  deriving Show
data UaResponseHeader = UaResponseHeader
  {
    --timestamp :: UaDateTime,
  --  requestHandle :: Int32,
    serviceResult :: Int32
 --   serviceDiagnostics :: UaDiagnosticInfo,
 --   stringTableSize:: CSize,
 --   stringTable :: Ptr (UaStringStruct),
 --   additionalHeader :: UaExtensionObject
  } deriving Show

data UA_BrowseDescription = UA_BrowseDescription
  {
    browseNodeId :: UaNodeId
  }
  deriving Show

_UA_BROWSERESULTMASK_ALL :: Int32
_UA_BROWSERESULTMASK_ALL = 63

instance Storable UA_BrowseDescription where
    sizeOf    _ = (#size UA_BrowseDescription)
    alignment _ = alignment (undefined :: CString)
    peek ptr = undefined
    poke ptr (UA_BrowseDescription id) = do
      (#poke UA_BrowseDescription, nodeId) ptr id
      (#poke UA_BrowseDescription, resultMask) ptr _UA_BROWSERESULTMASK_ALL

data UaBrowseRequest = UaBrowseRequest [UA_BrowseDescription]
  deriving Show

maxrefpernode :: CSize
maxrefpernode = 100
instance Storable UaBrowseRequest where
    sizeOf    _ = (#size UA_BrowseRequest)
    alignment _ = alignment (undefined :: CString)
    peek ptr = undefined
    poke ptr (UaBrowseRequest ns) = do
      let count = length ns
      arr <- callocArray count
      pokeArray arr ns
      (#poke UA_BrowseRequest, requestedMaxReferencesPerNode) ptr maxrefpernode
      (#poke UA_BrowseRequest, nodesToBrowse) ptr arr
      (#poke UA_BrowseRequest, nodesToBrowseSize) ptr count

data UaBrowseResponse = UaBrowseResponse {
    browseResponseHeader :: UaResponseHeader,
    browseResults :: Ptr (UA_BrowseResult),
    browseResultsSize :: Int
} deriving Show

instance Storable UaBrowseResponse where
    sizeOf    _ = (#size UA_BrowseResponse)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      header  <- #{peek UA_BrowseResponse, responseHeader} ptr
      size    <- #{peek UA_BrowseResponse, resultsSize} ptr
      res <- #{peek UA_BrowseResponse, results} ptr
      return (UaBrowseResponse {
                 browseResponseHeader = header,
                 browseResultsSize = size,
                 browseResults = res})

    poke ptr _ = undefined





data UaReadRequest = UaReadRequest [UA_ReadValueId]
  deriving Show

nsize :: CSize
nsize = 0


data UA_ReadValueId = UA_ReadValueId UaNodeId Int32
  deriving Show

instance Storable UA_ReadValueId where
    sizeOf    _ = (#size UA_ReadValueId)
    alignment _ = alignment (undefined :: CString)
    peek ptr = undefined
    poke ptr (UA_ReadValueId id attr) = do
      (#poke UA_ReadValueId, nodeId) ptr id
      (#poke UA_ReadValueId, attributeId) ptr attr



instance Storable UaReadRequest where
    sizeOf    _ = (#size UA_ReadRequest)
    alignment _ = alignment (undefined :: CString)
    peek ptr = undefined
    poke ptr (UaReadRequest ns) = do
      let count = length ns
      arr <- callocArray count
      pokeArray arr ns
      (#poke UA_ReadRequest, nodesToRead) ptr arr
      (#poke UA_ReadRequest, nodesToReadSize) ptr count

data UaReadResponse = UaReadResponse {
    responseHeader :: UaResponseHeader,
    resultValues :: Ptr (),
    resultsSize :: Int
} deriving Show

instance Storable UaResponseHeader where
    sizeOf    _ = (#size UA_ResponseHeader)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        result <- #{peek UA_ResponseHeader, serviceResult} ptr
        return (UaResponseHeader {serviceResult = result})

    poke ptr _ = undefined

instance Storable UaReadResponse where
    sizeOf    _ = (#size UA_ReadResponse)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      header  <- #{peek UA_ReadResponse, responseHeader} ptr
      size    <- #{peek UA_ReadResponse, resultsSize} ptr
      res <- #{peek UA_ReadResponse, results} ptr
      return (UaReadResponse {responseHeader = header, resultsSize = size, resultValues = res})


    poke ptr _ = do
      (#poke UA_ReadResponse, resultsSize) ptr nsize

data UaDataValueStruct = UaDataValueStruct
  {
    getUaDataValueStatus :: Int32,
    getUaDataValueVariant :: UaVariantStruct
  } deriving (Show)


instance Storable UaDataValueStruct where
    sizeOf    _ = (#size UA_DataValue)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      code  <- #{peek UA_DataValue, status} ptr
      var   <- #{peek UA_DataValue, value} ptr
      return (UaDataValueStruct {getUaDataValueStatus = code, getUaDataValueVariant = var})

    poke _ _ = undefined

data UA_BrowseResult = UA_BrowseResult
  {
    getBrowseResultSize :: Int,
    getBrowseResultReferences :: Ptr UaReferenceDescription
  }
  deriving (Show)

instance Storable UA_BrowseResult where
    sizeOf    _ = (#size UA_BrowseResult)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      size  <- #{peek UA_BrowseResult, referencesSize } ptr
      ref   <- #{peek UA_BrowseResult,  references} ptr
      return (UA_BrowseResult
              {
                getBrowseResultSize       = size,
                getBrowseResultReferences = ref
              })

    poke _ _ = undefined

data UaExpandedNodeId = UaExpandedNodeId {getUaNodeId :: UaNodeId} deriving (Show)
data UaReferenceDescription = UaReferenceDescription
  {
    getRefNodeId     :: UaExpandedNodeId,
    getRefDisplayName ::UA_LocalizedText,
    getRefBrowseName :: UaQualifiedName,
    tt ::Ptr ()

  } deriving (Show)


instance Storable UaReferenceDescription where
    sizeOf    _ = (#size UA_ReferenceDescription)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      id  <- #{peek UA_ReferenceDescription, nodeId.nodeId} ptr
      name <- #{peek UA_ReferenceDescription, browseName} ptr
      dname <- #{peek UA_ReferenceDescription, displayName} ptr
      t <- #{peek UA_ReferenceDescription, browseName.name.data} ptr
      return (UaReferenceDescription {getRefNodeId = UaExpandedNodeId id,
                                      getRefDisplayName = dname,
                                      getRefBrowseName = name,
                                      tt = t
                                     })

    poke _ _ = undefined

type UaName = UA_String
type NamespaceIndex = Int16

class HasUaName a where
    uaName ::a-> UaName

class HasUaNamespaceIndex a where
    namespaceIndex ::a-> NamespaceIndex

data UaQualifiedName = UaQualifiedName NamespaceIndex UA_String deriving Show

instance HasUaNamespaceIndex UaQualifiedName where
  namespaceIndex (UaQualifiedName idx _) = idx

instance HasUaName UaQualifiedName where
  uaName (UaQualifiedName _ name) = name


instance Storable UaQualifiedName where
    sizeOf    _ = (#size UA_QualifiedName)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        sp   <- (#peek UA_QualifiedName, namespaceIndex) ptr
        name <- (#peek UA_QualifiedName, name) ptr
        return (UaQualifiedName sp name)

    poke ptr _ = undefined

data UA_String = UA_String CSize CString deriving Show

instance Storable UA_String where
    sizeOf    _ = (#size UA_String)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        len <- (#peek UA_String, length) ptr
        d   <- (#peek UA_String, data) ptr
        return (UA_String len d)

    poke ptr _ = undefined

data UA_LocalizedText = UA_LocalizedText UA_String UA_String deriving Show

instance Storable UA_LocalizedText where
    sizeOf    _ = (#size UA_LocalizedText)
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
        locale <- (#peek UA_LocalizedText, locale) ptr
        text  <- (#peek UA_LocalizedText, text) ptr
        return (UA_LocalizedText locale text)

    poke ptr _ = undefined

ua_disable_log config = do
  (#poke UA_ClientConfig, logger.log) config nullPtr
