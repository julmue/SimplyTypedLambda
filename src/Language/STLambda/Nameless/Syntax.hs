{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.STLambda.Nameless.Syntax where

import Control.Monad (ap)
import Control.Monad.Writer (Writer, writer, runWriter)

import Bound
import Prelude.Extras

data Alpha n = Alpha { runAlpha :: n }
    deriving (Show, Read, Functor, Foldable, Traversable)

instance Eq (Alpha n) where
    _ == _ = True

data Exp n a
    = Var a
    | App    (Exp n a) (Exp n a)
    | Lam    (Alpha n) (Type n)  (Scope () (Exp n) a)
    | Let    (Alpha n) (Exp n a) (Scope () (Exp n) a)
    | Pair   (Exp n a) (Exp n a)
    | ProjL  (Exp n a)
    | ProjR  (Exp n a)
    | InjL   (Exp n a) (Type n)
    | InjR   (Exp n a) (Type n)
    | Case   (Scope () (Exp n) a) (Scope () (Exp n) a) (Exp n a)
    | TypeOf (Exp n a) (Type n)
    | Unit
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data Type n =
      TVar n
    | Top
    | Bot
    | Arrow (Type n) (Type n)
    | Prod  (Type n) (Type n)
    | Sum   (Type n) (Type n)
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

-- is there some sensible ORD instance?

instance Eq n => Eq1 (Exp n)
instance Show n => Show1 (Exp n)
instance Read n => Read1 (Exp n)

instance Monad (Exp n) where
    return = Var
--  (>>=) :: Exp a -> (a -> Exp b) -> Exp b
    (Var a) >>= g = g a
    (f `App` a) >>= g = (f >>= g) `App` (a >>= g)
    (Lam n t s) >>= g = Lam n t (s >>>= g)
    (Pair l r) >>= g = Pair (l >>= g) (r >>= g)
    (ProjL e) >>= g = ProjL (e >>= g)
    (ProjR e) >>= g = ProjR (e >>= g)
    (InjL e t) >>= g = InjL (e >>= g) t
    (InjR e t) >>= g = InjR (e >>= g) t
    (Case l r e) >>= g = Case (l >>>= g) (r >>>= g) (e >>= g)
    (TypeOf e t) >>= g = TypeOf (e >>= g) t

instance Applicative (Exp n) where
    pure = return
    (<*>) = ap
