module OpcUa.Types where

import Foreign

data UaClient = UaClient
data UaClientConfig = UaClientConfig

data UaVariant =   UaString String
  deriving (Show)


data UaDateTimeStruct  = UaDateTimeStruct {
  getMinutes :: Int16,
  getSeconds :: Int16
  }
  deriving (Show, Eq)


data UaNodeId = UaNodeIdNum {
  getNamespaceIndex :: Int16,
  getNodeIdType :: Int32,
  getIdentifire :: Int32
  }
  deriving (Show, Eq)


data UaDataType = UaDataType
  {
    getUaDataTypeId               ::   UaNodeId
  }
  deriving (Show, Eq)


data UaVariantStruct =  UaVariantStruct
  {
    getVariantType                ::  Ptr UaDataType,
    getVariantStorageType         ::  Int16,
    getVariantArrayLength         ::  Int32,
    getVariantData                ::  Ptr UaVariant,
    getVariantArrayDemensionsSize ::  Int32,
    getVariantArrayDemensions     ::  Ptr Int32
  }
  deriving Show

data UaNodeClass = UaObjectClass
  | UaVariableClass
  | UaMethodClass
  | UaObjectTypeClass
  | UaVariableTypeClass
  | UaRerenceTypeClass
  | UaDataTypeClass
  | UaViewClass
  deriving Show

data UaReadAttr = UaClass UaNodeClass | UaValue UaVariant
  deriving Show


id_Boolean     = UaNodeIdNum 0 0 1
id_DateTime    = UaNodeIdNum 0 0 3
id_String      = UaNodeIdNum 0 0 12
id_VariantType = UaNodeIdNum 0 0 24
id_NodeClassType = UaNodeIdNum 0 0 257
id_NodeId      = UaNodeIdNum 0 0 17
