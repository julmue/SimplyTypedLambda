{-# LANGUAGE RankNTypes #-}

module Language.STLambda.Semantics
    (
      Printer
    , mkNormalOrder
    , mkNormalOrder1
    , mkNormalOrderTraced
    , mkNormalOrderTracedLimit
    , typeCheck
    ) where

import Data.Bifunctor

import qualified Bound.Unwrap as BU
import qualified Language.STLambda.Syntax as N
import qualified Language.STLambda.Nameless.Syntax as NL
import qualified Language.STLambda.Nameless.Semantics as NLS

type Printer a = (a, Int) -> a

refresh :: Functor f => f a -> f (BU.Fresh a)
refresh = fmap BU.name

eval :: Eq a =>
       Printer a
    -> (forall n b . NL.Exp n b -> NL.Exp n b)
    -> N.Exp a -> N.Exp a
eval p h = fmap (refreshPrinter p) . N.name. h . N.uname . refresh
  where

evalTraced :: Eq a =>
        Printer a
    -> (forall n b . NL.Exp n b -> (NL.Exp n b, [NL.Exp n b]))
    -> N.Exp a -> (N.Exp a, [N.Exp a])
evalTraced p h = bimap nm (map nm) . h . N.uname . refresh
  where
    nm = fmap (refreshPrinter p) . N.name

refreshPrinter :: Printer a -> BU.Fresh a -> a
refreshPrinter p fr = p (BU.uname fr, BU.fresh fr)

mkNormalOrder :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkNormalOrder p = eval p NLS.normalOrder

mkNormalOrder1 :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkNormalOrder1 p = eval p NLS.normalOrder1

mkNormalOrderTraced :: Eq a => Printer a -> N.Exp a -> (N.Exp a, [N.Exp a])
mkNormalOrderTraced p = evalTraced p NLS.normalOrderTraced

mkNormalOrderTracedLimit :: Eq a => Printer a -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkNormalOrderTracedLimit p i = evalTraced p (NLS.normalOrderTracedLimit i)

-- typeCheck :: Eq a => N.Exp a -> Either (NLS.TypeError a) (N.Type a)
-- typeCheck = either id N.nameT . NLS.typeOf [] . N.uname
typeCheck :: Eq a => N.Exp a -> Either (NLS.TypeError a) (N.Type a)
typeCheck = second N.nameT . NLS.typeOf [] . N.uname
