{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.STLambda.Syntax
    (
      Exp (..)
    , Type (..)
    , name
    , uname
    , nameT
    , unameT
    ) where

import Bound
import Bound.Unwrap (Fresh, Unwrap, unwrap, runUnwrap)

import qualified Language.STLambda.Nameless.Syntax as NL

data Exp a =
      Var a
    | App (Exp a) (Exp a)
    | Lam a (Type a) (Exp a)
    | Let a (Exp a) (Exp a)
    | Pair (Exp a) (Exp a)
    | ProjL (Exp a)
    | ProjR (Exp a)
    | InjL (Exp a) (Type a)
    | InjR (Exp a) (Type a)
    | Case a (Exp a) a (Exp a) (Exp a)
    | TypeOf (Exp a) (Type a)
    | Unit
    deriving (Read, Show, Functor, Foldable, Traversable)

data Type a =
      TVar a
    | Top
    | Bot
    | Prod  (Type a) (Type a)
    | Sum   (Type a) (Type a)
    | Arrow (Type a) (Type a)
    deriving (Eq, Show, Read, Functor, Foldable, Traversable)

instance Eq a => Eq (Exp a) where
   e1 == e2 = uname e1 == uname e2

uname :: Eq a => Exp a -> NL.Exp a a
uname (Var a) = NL.Var a
uname (f `App` a) = (uname f) `NL.App` (uname a)
uname (Lam n t e) = NL.Lam (NL.Alpha n) (unameT t) (abstract1 n (uname e))
uname (Pair l r) = NL.Pair (uname l) (uname r)
uname (ProjL e) = NL.ProjL (uname e)
uname (ProjR e) = NL.ProjR (uname e)
uname (InjL e t) = NL.InjL (uname e) (unameT t)
uname (InjR e t) = NL.InjR (uname e) (unameT t)
uname (Case n1 l n2 r e) = NL.Case (NL.Alpha n1) (abstract1 n1 l') (NL.Alpha n2) (abstract1 n2 r') e'
  where
    l' = uname l
    r' = uname r
    e' = uname e
uname (TypeOf e t) = NL.TypeOf (uname e) (unameT t)
uname Unit = NL.Unit

name :: Eq a => NL.Exp (Fresh a) (Fresh a) -> Exp (Fresh a)
name = runUnwrap . go
  where
    go :: NL.Exp (Fresh a) (Fresh a) -> Unwrap (Exp (Fresh a))
    go (NL.Var n) = return (Var n)
    go (f `NL.App` a) = App <$> go f <*> go a
    go (NL.Lam (NL.Alpha n) t s) = do
        (n', e) <- unwrap n s
        Lam <$> pure n' <*> pure (nameT t) <*> go e
    go (NL.Let (NL.Alpha n) d s) = do
        d' <- go d
        (n', e) <- unwrap n s
        Let <$> pure n' <*> pure d' <*> go e
    go (NL.Pair l r) = Pair <$> go l <*> go r
    go (NL.ProjL e) = ProjL <$> go e
    go (NL.ProjR e) = ProjR <$> go e
    go (NL.InjL e t) = InjL <$> go e <*> pure (nameT t)
    go (NL.InjR e t) = InjR <$> go e <*> pure (nameT t)
    go (NL.Case (NL.Alpha n1) l (NL.Alpha n2) r e) = do
        (n1', l') <- unwrap n1 l
        (n2', r') <- unwrap n2 r
        Case <$> pure n1' <*> go l' <*> pure n2' <*> go r' <*> go e
    go (NL.TypeOf e t) = TypeOf <$> go e <*> pure (nameT t)
    go NL.Unit = pure Unit

unameT :: Type a -> NL.Type a
unameT (TVar a) = NL.TVar a
unameT Top = NL.Top
unameT Bot = NL.Bot
unameT (Arrow c d) = NL.Arrow (unameT c) (unameT d)
unameT (Prod l r) = NL.Prod (unameT l) (unameT r)
unameT (Sum l r) = NL.Sum (unameT l) (unameT r)

nameT :: NL.Type a -> Type a
nameT (NL.TVar a) = TVar a
nameT NL.Top = Top
nameT NL.Bot = Bot
nameT (NL.Arrow c d) = Arrow (nameT c) (nameT d)
nameT (NL.Prod l r) = Prod (nameT l) (nameT r)
nameT (NL.Sum l r) = Sum (nameT l) (nameT r)

