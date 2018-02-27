
-- Laboratory 10
-- Based on Real World Haskell, Chapter 5 and Chapter 6

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- read more about LANGUAGE Extensions https://wiki.haskell.org/Language_extensions

import SimpleJSON
import Data.Either
import Data.List 
import Test.QuickCheck

result1 :: JValue
result1 = (JObject [("foo", JNumber 1), ("bar", JBool False)])


result2 :: JValue
result2 = JObject [
  ("query", JString "awkward squad haskell"),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray [
     JObject [
      ("title", JString "Simon Peyton Jones: papers"),
      ("snippet", JString "Tackling the awkward ..."),
      ("url", JString "http://.../marktoberdorf/")
     ]])
  ]


  
renderJValue :: JValue -> String
renderJValue x = f x where
    f (JString s) = s
    f (JNumber n) = show n
    f (JBool True) = "true"
    f (JBool False) = "false"
    f (JNull) = "null"
    f (JObject o) = "{" ++ items o ++ "}" where
        items [] = ""
        items is = intercalate ", " (map renderItems is)
        renderItems (i,v) = i ++ ": " ++ f v
    f (JArray xs) = "[" ++ intercalate ", " (map f xs) ++ "]"

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a 

instance JSON JValue where
    toJValue = id
    fromJValue = Right
    
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"  
   
instance JSON Integer where
    toJValue = JNumber
    fromJValue (JNumber n) = Right (toInteger n)
    fromJValue _ = Left "not a JSON number" 

    
instance JSON String where
	toJValue = JString
	fromJValue (JString s) = Right s
	fromJValue _ = Left "not a JSON string"
	
instance (JSON a) => JSON [a] where
	toJValue xs = JArray [toJValue x | x<-xs]
	fromJValue (JArray xs) = Right [x |x<-xs]
	fromJValue _ = Left "not a JSON array"

-- instance (JSON a) => JSON [(String, a)] where
    -- toJValue = JObject
    -- fromJValue (JObject o) = Right o  
	-- --fromJValue _ = Left "not a JSON object"

   
    
   
