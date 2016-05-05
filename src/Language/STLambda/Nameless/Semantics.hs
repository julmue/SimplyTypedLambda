{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.STLambda.Nameless.Semantics
    (
      normalOrder
    , normalOrder1
    , normalOrderTraced
    , normalOrderTracedLimit
    , TypeError
    , typeOf
    ) where

import Control.Monad.Writer (Writer, writer, runWriter)

import Bound

import Language.STLambda.Nameless.Syntax

-- ----------------------------------------------------------------------------
-- type checking

data TypeError a =
      NotInContext a (Context a)
    | DomainMismatch (Exp a a) (Exp a a)
    | NoFunction (Exp a a)
    | NoProd (Exp a a)
    | NoSum (Type a)
--    | NoTuple (Exp a a)
--    | NoRecord (Exp a a)
--    | NoVariant (Exp a a)
--    | OutOfBounds Int [(Exp a a)]
    | MultiDefs [(String,(Exp a a))]
    | NoIdentifier String [(String,(Exp a a))]
    | WrongType (Exp a a) (Type a)
    | WrongAscription (Exp a a) (Type a)
    | DefaultError
    deriving Show

type Context a = [(a, Type a)]

typeOf :: Eq a => Context a -> Exp a a -> Either (TypeError a) (Type a)
typeOf _ Unit = Right Top
typeOf ctx (Var n) = case lookup n ctx of
    Just t -> Right t
    Nothing -> Left (NotInContext n ctx)
typeOf ctx (f `App` a) = do
    f_ <- typeOf ctx f
    a_ <- typeOf ctx a
    case f_ of
        (dom_ `Arrow` cod_) -> if dom_ == a_
                           then Right cod_
                           else Left (DomainMismatch f a)
        _ -> Left (NoFunction f)
typeOf ctx (Lam (Alpha n) t s) = (Arrow t) <$> typeOf ctx' s'
   where
     ctx' = (n, t) : ctx
     s' = instantiate1 (Var n) s
typeOf ctx (Pair l r) = Prod <$> typeOf ctx l <*> typeOf ctx r
typeOf cxt (ProjL p) = do
    p_ <- typeOf cxt p
    case p_ of
        Prod l_ _ -> Right l_
        _ -> Left (NoProd p)
typeOf cxt (ProjR p) = do
    p_ <- typeOf cxt p
    case p_ of
        Prod _ r_ -> Right r_
        _ -> Left (NoProd p)
-- isn't that an error here?
typeOf ctx (InjL il s_) = case s_ of
    (Sum l_ _) -> do
        il_ <- typeOf ctx il
        if l_ == il_ then Right il_ else Left (WrongType il s_)
    _ -> Left (NoSum s_)
typeOf ctx (InjR ir s_) = case s_ of
    (Sum _ r_) -> do
        ir_ <- typeOf ctx ir
        if r_ == ir_ then Right ir_ else Left (WrongType ir s_)
    _ -> Left (NoSum s_)
typeOf ctx (TypeOf e t) = do
     e_ <- typeOf ctx e
     if e_ == t then Right e_ else Left (WrongAscription e t)


-- ----------------------------------------------------------------------------
-- evaluation

data StepResult n a =
      Normalform (Exp n a)
    | Reducible (Exp n a)

foldSR :: (Exp n a -> c) -> (Exp n a -> c) -> StepResult n a -> c
foldSR n _ (Normalform e) = n e
foldSR _ r (Reducible e) = r e

unStepResult :: StepResult n a -> Exp n a
unStepResult = foldSR id id

-- small-step normal order evaluation
normalOrder1 :: Exp n a -> Exp n a
normalOrder1 = unStepResult . stepNO

-- multi-step normal order evaluation
normalOrder :: Exp n a -> Exp n a
normalOrder = foldSR id normalOrder . stepNO

normalOrderTraced :: Exp n a -> (Exp n a, [Exp n a])
normalOrderTraced = tracedEval stepNO

normalOrderTracedLimit :: Int -> Exp n a -> (Exp n a, [Exp n a])
normalOrderTracedLimit = tracedEvalLimit stepNO

stepNO :: Exp n a -> StepResult n a
stepNO (App (Lam _ _ body) arg) = Reducible (instantiate1 arg body)
stepNO app@(App fun arg) = case stepNO fun of
    Normalform _ -> case stepNO arg of
        Normalform _ -> Normalform app
        Reducible arg' -> Reducible (App fun arg')
    Reducible fun' -> Reducible (App fun' arg)
stepNO fun@(Lam n t body) = case stepNO . fromScope $ body of
    Normalform _  ->  Normalform fun
    Reducible body' -> Reducible (Lam n t (toScope body'))
stepNO (Let _ d e) = Reducible (instantiate1 d e)
stepNO pair@(Pair e1 e2) = case stepNO e1 of
    Normalform _ -> case stepNO e2 of
        Normalform _ -> Normalform pair
        Reducible e2' -> Reducible (Pair e1 e2')
    Reducible e1' -> Reducible (Pair e1' e2)
stepNO (ProjL (Pair e1 _)) = Reducible e1
stepNO (ProjR (Pair _ e2)) = Reducible e2
stepNO injl@(InjL e t) = case stepNO e of
    Normalform _ -> Normalform injl
    Reducible e' -> Reducible (InjL e' t)
stepNO injr@(InjR e t) = case stepNO e of
    Normalform _ -> Normalform injr
    Reducible e' -> Reducible (InjR e' t)
stepNO (Case _ l _ _ (InjL e _)) = Reducible (instantiate1 e l)
stepNO (Case _ _ _ r (InjR e _)) = Reducible (instantiate1 e r)
stepNO (TypeOf e _) = Reducible e
stepNO Unit = Normalform Unit
stepNO n = Normalform n

tracedEval :: forall n a . (Exp n a -> StepResult n a) -> Exp n a -> (Exp n a, [Exp n a])
tracedEval step expr = runWriter $ writer (expr,[expr]) >>= go
  where
    go :: Exp n a -> Writer [Exp n a] (Exp n a)
    go e = case step e of
        Normalform e' -> writer (e', mempty)
        Reducible e' -> writer (e', [e']) >>= go

tracedEvalLimit :: forall n a . (Exp n a -> StepResult n a) -> Int -> Exp n a -> (Exp n a, [Exp n a])
tracedEvalLimit step counter expr = runWriter $ writer (expr,[expr]) >>= go counter
  where
    go :: Int -> Exp n a -> Writer [Exp n a] (Exp n a)
    go i e =
        if i > 0
        then case step e of
            Normalform e' -> writer (e', mempty)
            Reducible e' -> writer (e', [e']) >>= go (pred i)
        else writer (e, mempty)


