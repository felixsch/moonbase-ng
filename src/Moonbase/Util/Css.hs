{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Moonbase.Util.Css 
  ( Scope(..)
  , Id(..)
  , combine, (&)
  , RuleM(..)
  , css, cssRules, (%), (#)
  ) where


import Moonbase.Theme
import Control.Monad.Writer

import Data.List
import Data.Monoid
import Data.String

data Scope = KleenStar
           | Class
           | Identifier
           | Type
           deriving (Show)

data Id = Id Scope String
        | Nested [Id]
        | Refinement Id [String]
        | Actions [String]
        deriving (Show)

instance IsString Id where
  fromString ('.': xs) = Id Class xs
  fromString ('#': xs) = Id Identifier xs
  fromString (':': xs) = Actions [xs]
  fromString str       = Id Type str


renderId :: Id -> String
renderId (Id KleenStar _)  = "*"
renderId (Id Class id)      = "." ++ id
renderId (Id Identifier id) = "#" ++ id
renderId (Id Type id)       = id
renderId (Nested n)         = intercalate ", " $ map renderId n
renderId (Actions xs)       = concatMap (":" ++) xs

renderId (Refinement (Nested ps) n)  = intercalate ", " $ map (\p ->
  renderId p ++ concatMap (":" ++) n ) ps

renderId (Refinement p n)            = renderId p ++ concatMap (":" ++) n


combine :: Id -> Id -> Id
combine (Actions x)       (Actions y)       = Actions (x ++ y)
combine (Refinement p xs) (Actions ys)      = Nested [Refinement p xs, Refinement p (xs ++ ys)]

combine (Nested x) (Nested y)               = Nested (x ++ y)

combine p (Actions xs)                      = Refinement p xs
combine (Actions xs) p                      = Refinement p xs

combine (Nested xs) p                       = Nested (p : xs)
combine p (Nested x)                        = Nested (map (combine p) x)

combine p a                                 = Nested (p : [a])

(&) :: Id -> Id -> Id
(&) = combine

type Key   = String
type Value = String

data Rule = Property String String
          | Rule Id [Rule]
          | Root [Rule]
          deriving (Show)

isProperty :: Rule -> Bool
isProperty (Property _ _) = True
isProperty _              = False

isRule :: Rule -> Bool
isRule (Rule _ _) = True
isRule _          = False

prepareRule :: Rule -> [Rule]
prepareRule (Root childs) = concatMap prepareRule childs
prepareRule (Property k v) = [Property k v]
prepareRule (Rule id sub) = withoutSubRules : concatMap apply subRules
  where
    withoutSubRules       = Rule id (filter isProperty sub)
    subRules              = filter isRule sub
    apply (Rule id' sub') = prepareRule $
      Rule (combine id id') sub'


renderRule :: Rule -> String
renderRule (Root childs)  = concatMap renderRule childs
renderRule (Property k v) = k ++ ": " ++ v ++ ";"
renderRule (Rule id subs) = renderId id ++ " {\n" 
                                        ++ concatMap renderRule subs
                                        ++ "}\n"

newtype RuleM a = R (Writer [Rule] a)
  deriving (Functor, Applicative, Monad)

type Css = RuleM ()

rule :: Rule -> Css
rule r = R (tell [r])

generate :: Css -> [Rule]
generate (R f) = execWriter f

cssRules :: Css -> Css
cssRules c = rule $ Root (generate c)

(%) :: Id -> Css -> Css
(%) id c = rule $ Rule id (generate c)

(#) :: Key -> Value -> Css
(#) k v = rule $ Property k v

css :: Css -> String
css c = concatMap renderRule $ concatMap prepareRule (generate c)
