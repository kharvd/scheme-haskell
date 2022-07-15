module Environment
  ( Environment
  , insert
  , replace
  , resolve
  , create
  , parent
  ) where

import qualified Data.Map.Strict as Map

import Predefs
import Utils

type Scope a = Map.Map String a

data Environment a =
  Environment (Scope a) (Maybe (Environment a))
  deriving (Show)

create :: [(String, a)] -> Maybe (Environment a) -> Environment a
create scope = Environment (Map.fromList scope) 

mapScope :: (Scope a -> Scope a) -> Environment a -> Environment a
mapScope f env@(Environment scope parent) = Environment (f scope) parent

insert :: String -> a -> Environment a -> Environment a
insert name value = mapScope (Map.insert name value)

resolve :: String -> Environment a -> Maybe a
resolve name env@(Environment scope maybeParent) = recoverWith (resolveLocal name env) fallback
  where
    fallback = maybeParent >>= resolve name

resolveLocal :: String -> Environment a -> Maybe a
resolveLocal name (Environment scope maybeParent) = scope Map.!? name

parent :: Environment a -> Maybe (Environment a)
parent env@(Environment _ maybeParent) = maybeParent

replace :: String -> a -> Environment a -> Maybe (Environment a)
replace name value env@(Environment scope maybeParent) =
  case (resolveLocal name env, maybeParent) of
    (Just z, maybeParent) -> Just $ insert name value env
    (Nothing, Just parent) -> do
      newParent <- replace name value parent
      return $ Environment scope (Just newParent)
    (Nothing, Nothing) -> Nothing