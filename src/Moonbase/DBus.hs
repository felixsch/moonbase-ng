{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

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
  , sanatizeName
  , on
  , withoutHelp
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           DBus
import           DBus.Client

import           Data.Char                   (toLower, toUpper)
import qualified Data.Map                    as M
import           Data.Maybe

import           Moonbase.Core

moonbaseBusName :: BusName
moonbaseBusName = "org.moonbase"

moonbaseInterfaceName :: InterfaceName
moonbaseInterfaceName = "org.moonbase"

moonbaseObjectPath :: ObjectPath
moonbaseObjectPath = "/org/moonbase"

withInterface :: String -> InterfaceName
withInterface name = interfaceName_ $
    formatInterfaceName moonbaseInterfaceName ++ "." ++ name

withObjectPath :: String -> ObjectPath
withObjectPath name = objectPath_ $ formatObjectPath moonbaseObjectPath ++ "/" ++ name


type Ref m = TVar (Runtime m)
type DBusSignal = DBus.Signal

dbusMethod :: (Moon m) => ObjectPath -> (Ref m -> [Method]) -> Moonbase m ()
dbusMethod objPath generator = do
  client <- view dbus <$> get
  ref    <- ask
  io $ export client objPath $ generator ref

dbusMethod_ :: (Moon m) => (Ref m -> [Method]) -> Moonbase m ()
dbusMethod_ = dbusMethod moonbaseObjectPath

dbusSignal :: MatchRule -> (DBusSignal -> Moonbase IO ()) -> Moonbase IO SignalHandler
dbusSignal match cmd' = do
    client <- view dbus <$>  get
    ref <- ask
    io $ addMatch client match $ \sig ->
      evalWith_ ref (cmd' sig)

autoM_ :: AutoMethod fn => MemberName -> fn -> Method
autoM_ = autoMethod moonbaseInterfaceName

autoM :: AutoMethod fn => InterfaceName -> MemberName -> fn -> Method
autoM = autoMethod

wrap0 :: (Moon m) => Ref m -> Moonbase m b -> m b
wrap0 = eval'

wrap1 :: (Moon m, IsValue a0) => Ref m -> (a0 -> Moonbase m b) -> a0 -> m b
wrap1 ref f arg0 = eval' ref (f arg0)

wrap2 :: (Moon m, IsValue a0, IsValue a1) => Ref m -> (a0 -> a1 -> Moonbase m b) -> a0 -> a1 -> m b
wrap2 ref f arg0 arg1 = eval' ref (f arg0 arg1)

wrap3 :: (Moon m, IsValue a0, IsValue a1, IsValue a2) => Ref m -> (a0 -> a1 -> a2 -> Moonbase m b) -> a0 -> a1 -> a2 -> m b
wrap3 ref f arg0 arg1 arg2 = eval' ref (f arg0 arg1 arg2)

wrap4 :: (Moon m, IsValue a0, IsValue a1, IsValue a2, IsValue a3) => Ref m -> (a0 -> a1 -> a2 -> a3 -> Moonbase m b) -> a0 -> a1 -> a2 -> a3 -> m b
wrap4 ref f arg0 arg1 arg2 arg3 = eval' ref (f arg0 arg1 arg2 arg3)



class MoonDBusMethod fn where
  genFunTypes :: fn -> ([Type], [Type])
  applyMethod :: Ref IO -> fn -> [Variant] -> Maybe (IO [Variant])


instance MoonDBusMethod (Moonbase IO ()) where
  genFunTypes _ = ([], [])

  applyMethod ref f [] = Just (eval ref f >> return [])
  applyMethod _   _ _  = Nothing


instance (IsValue a) => MoonDBusMethod (Moonbase IO a) where
  genFunTypes f = ([], case moonT f undefined of
                         (_, t) -> case t of
                                     TypeStructure ts -> ts
                                     _                -> [t])
    where
      moonT :: IsValue a => Moonbase IO a -> a -> (a, Type)
      moonT _ a = (a, typeOf a)

  applyMethod ref f [] = Just (do
                    var <- fmap toVariant (evalWith' ref f)
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



dbusM :: (MoonDBusMethod fn) => Ref IO -> InterfaceName -> MemberName -> fn -> Method
dbusM ref iface name f = method iface name inSig outSig mo
  where
    (typesIn, typesOut) = genFunTypes f
    inSig               = fromMaybe (invalid "input") $ signature typesIn
    outSig              = fromMaybe (invalid "output") $ signature typesOut
    mo msg              = case applyMethod ref f (methodCallBody msg) of
                            Nothing  -> return (replyError invalidParams [])
                            Just io -> fmap replyReturn io

    invalidParams = errorName_ "org.freedesktop.DBus.Error.InvalidParameters"
    invalid label = error (concat [ "Method "
                          , formatInterfaceName iface
                          , "."
                          , formatMemberName name
                          , " has an invalid "
                          , label
                          , " signature."])

toMethod :: Ref IO -> Action IO -> Method
toMethod ref (MoonbaseAction name _ f) = dbusM ref (withInterface "Action") (memberName_ name) f

runAction :: (Moon m) => Action m -> [String] -> Moonbase m String
runAction (MoonbaseAction _ _ f) = f

class Nameable a where
  prepareName :: a -> (Name, Name)

instance Nameable Name where
  prepareName n = (n, lower)
    where
      lower = map toLower n

instance Nameable (Name, Name) where
  prepareName (n, a) = (n, a)

on :: (Nameable a) => a -> String -> ([String] -> Moonbase IO String) -> Moonbase IO ()
on n help f = do
    actions . at key' ?= action'
    allActions <- M.toList <$> use actions

    dbusMethod (withObjectPath "Action") $ \ref -> map (toMethod ref . snd) allActions
  where
    (name', key')   = prepareName n
    action'         = MoonbaseAction name' help f

withoutHelp :: String
withoutHelp = "No help is available."

sanatizeName :: String -> String
sanatizeName []       = []
sanatizeName (' ':xs) = sanatizeName xs
sanatizeName ('-':x:xs) = toUpper x : sanatizeName xs
sanatizeName ('_':x:xs) = toUpper x : sanatizeName xs
sanatizeName (x:xs)     = x : sanatizeName xs
