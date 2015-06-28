{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Moonbase.DBus
  ( moonbaseBusName
  , moonbaseInterfaceName
  , moonbaseObjectPath
  , withInterface
  , withObjectPath
  , Ref
  , dbusMethod, dbusMethod_
  , dbusSignal
  , autoM, autoM_
  , wrap0, wrap1, wrap2, wrap3, wrap4
  , MoonDBusMethod(..)
  , dbusM
  , toMethod
  , runAction
  , Nameable(..)
  , on
  , withoutHelp
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Control.Concurrent.STM.TVar
import DBus.Client
import DBus

import Data.Maybe
import Data.Char (toLower)
import qualified Data.Map as M

import Moonbase.Core

moonbaseBusName :: BusName
moonbaseBusName = "org.moonbase"

moonbaseInterfaceName :: InterfaceName
moonbaseInterfaceName = "org.moonbase"

moonbaseObjectPath :: ObjectPath
moonbaseObjectPath = "/org/moonbase"

withInterface :: String -> InterfaceName
withInterface name = interfaceName_ $
    (formatInterfaceName moonbaseInterfaceName) ++ "." ++ name

withObjectPath :: String -> ObjectPath
withObjectPath name = objectPath_ $ (formatObjectPath moonbaseObjectPath ) ++ "/" ++ name


type Ref = TVar Runtime 
type DBusSignal = DBus.Signal

dbusMethod :: ObjectPath -> (Ref -> [Method]) -> Moon ()
dbusMethod objPath generator = do
  client <- view dbus <$> get
  ref    <- ask
  liftIO $ export client objPath $ generator ref

dbusMethod_ :: (Ref -> [Method]) -> Moon ()
dbusMethod_ = dbusMethod moonbaseObjectPath

dbusSignal :: MatchRule -> (DBusSignal -> Moon ()) -> Moon SignalHandler
dbusSignal match cmd = do
    client <- view dbus <$>  get
    ref <- ask
    liftIO $ addMatch client match $ \sig ->
      eval ref (cmd sig)

autoM_ :: AutoMethod fn => MemberName -> fn -> Method
autoM_ = autoMethod moonbaseInterfaceName

autoM :: AutoMethod fn => InterfaceName -> MemberName -> fn -> Method
autoM = autoMethod

wrap0 :: Ref -> Moon b -> IO b
wrap0 = eval

wrap1 :: (IsValue a0) => Ref -> (a0 -> Moon b) -> a0 -> IO b
wrap1 ref f arg0 = eval ref (f arg0)

wrap2 :: (IsValue a0, IsValue a1) => Ref -> (a0 -> a1 -> Moon b) -> a0 -> a1 -> IO b
wrap2 ref f arg0 arg1 = eval ref (f arg0 arg1)

wrap3 :: (IsValue a0, IsValue a1, IsValue a2) => Ref -> (a0 -> a1 -> a2 -> Moon b) -> a0 -> a1 -> a2 -> IO b
wrap3 ref f arg0 arg1 arg2 = eval ref (f arg0 arg1 arg2)

wrap4 :: (IsValue a0, IsValue a1, IsValue a2, IsValue a3) => Ref -> (a0 -> a1 -> a2 -> a3 -> Moon b) -> a0 -> a1 -> a2 -> a3 -> IO b
wrap4 ref f arg0 arg1 arg2 arg3 = eval ref (f arg0 arg1 arg2 arg3)



class MoonDBusMethod fn where
  genFunTypes :: fn -> ([Type], [Type])
  applyMethod :: Ref -> fn -> [Variant] -> Maybe (IO [Variant])


instance MoonDBusMethod (Moon ()) where
  genFunTypes _ = ([], [])

  applyMethod ref f [] = Just (eval ref f >> return [])
  applyMethod _   _ _  = Nothing


instance (IsValue a) => MoonDBusMethod (Moon a) where
  genFunTypes f = ([], case moonT f undefined of
                         (_, t) -> case t of
                                     TypeStructure ts -> ts
                                     _                -> [t])
    where
      moonT :: IsValue a => Moon a -> a -> (a, Type)
      moonT _ a = (a, typeOf a)

  applyMethod ref f [] = Just (do
                    var <- fmap toVariant (eval ref f)
                    case fromVariant var of
                      Just struct -> return (structureItems struct)
                      Nothing -> return [var])
  applyMethod _   _ _  = Nothing

instance (IsValue a, MoonDBusMethod fn) => MoonDBusMethod (a -> fn) where
  genFunTypes f = case valueT undefined of
                    (a, t) -> case genFunTypes (f a) of
                                (ts, ts') -> (t : ts, ts')
      where
        valueT :: IsValue a => a -> (a, Type)
        valueT a = (a, typeOf a)

  applyMethod ref f (v:vs) = case fromVariant v of
                           Just v' -> applyMethod ref (f v') vs
                           Nothing -> Nothing
  applyMethod _ _ []       = Nothing



dbusM :: (MoonDBusMethod fn) => Ref -> InterfaceName -> MemberName -> fn -> Method
dbusM ref iface name f = method iface name inSig outSig mo
  where
    (typesIn, typesOut) = genFunTypes f
    inSig               = case signature typesIn of
                            Just sig -> sig
                            Nothing  -> invalid "input"
    outSig              = case signature typesOut of
                            Just sig -> sig
                            Nothing  -> invalid "ouput"
    mo msg              = case applyMethod ref f (methodCallBody msg) of
                            Nothing  -> return (replyError invalidParams [])
                            Just io -> fmap replyReturn io

    invalidParams = errorName_ "org.freedesktop.DBus.Error.InvalidParameters"
    invalid label = error (concat
		[ "Method "
		, formatInterfaceName iface
		, "."
		, formatMemberName name
		, " has an invalid "
		, label
		, " signature."])

toMethod :: Ref -> Action -> Method
toMethod ref (MoonbaseAction name _ f) = dbusM ref (withInterface "Action") (memberName_ name) f

runAction :: Action -> [String] -> Moon String
runAction (MoonbaseAction _ _ f) args = f args

class Nameable a where
  prepareName :: a -> (Name, Name)

instance Nameable Name where
  prepareName n = (n, lower)
    where
      lower = map toLower n

instance Nameable (Name, Name) where
  prepareName (n, a) = (n, a)

on :: (Nameable a) => a -> String -> ([String] -> Moon String) -> Moon ()
on n help f = do
    actions . at key' ?= action'
    allActions <- M.toList <$> use actions

    dbusMethod (withObjectPath "Action") $ \ref -> map (toMethod ref . snd) allActions
  where
    (name', key')   = prepareName n 
    action'         = MoonbaseAction name' help f

withoutHelp :: String
withoutHelp = "No help is available."









{-

on "Quit" withoutHelp $ \args -> do
  ..
    ..
      ..


 on "Quit" $ \args -> do
   ...

 "Base" `grouped` 
 on "Quit" $ do
  onGroup "Base" "Quit" 

 on "Quit" $ do


-}

{-

aAction = dbusMethod_ $ \ref -> [dbusM ref (withInterface "Test") (memberName_ "Quit") quitMoonbase]
  where

installAction :: ActionGroup -> Action -> Moon ()
installAction group (Action aname helptext _) = actions . ix group . at aname ?= helptext
 
exportGroup :: ActionGroup -> [Action] -> Moon ()
exportGroup group acts = do
  actions' <- use actions
  unless (isJust $ actions' ^? ix group) (actions . at group ?= M.empty)
  
  mapM_ (installAction group) acts

  dbusMethod_ $ \ref -> map (actionToMethod ref group) acts


on :: (AutoMethod fn) => Name -> String -> (Ref -> fn) -> Action
on = Action  -}
