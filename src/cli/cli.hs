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
  ua_disable_log $ ua_client_get_config client

  endpoint <- newCString uri
  code <- ua_client_connect client endpoint
  free endpoint
  return client

main = do
  client <- create_client
  Right b <- browse_service client (UaNodeIdNum 0 0 85)
  ua_client_delete client


  obj <- sequence $ map (helper_g . helper_f) b
  sequence $ map (putStrLn . toText) obj

cstr (UA_String len d) =do
  if len == 0
    then return ""
    else do
      r <- peekCString d
      return r

toText ((s, id), name, (_, dname)) = sid ++ "\t"++ name where
  sid = show s ++" "++show id

localizet_text_to_str (UA_LocalizedText l t) =do
  locale <- cstr l
  text   <- cstr t
  return (locale, text)

helper_g ((s, id), name, dname) = do
  sname <- cstr name
  sdname <- localizet_text_to_str dname
  return ((s,id), sname, sdname)

helper_f x = f x where
    f x = ((s, id), name, dname)
    s  = (getNamespaceIndex . getUaNodeId . getRefNodeId) x
    id = (getIdentifire. getUaNodeId . getRefNodeId) x
    name = (uaName.  getRefBrowseName) x
    dname = getRefDisplayName x
