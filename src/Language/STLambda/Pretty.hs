module Language.STLambda.Pretty
    (
      prettyPrint
    ) where

import Prelude

import Text.PrettyPrint.HughesPJ

import Language.STLambda.Syntax

identifier :: Show a => a -> Doc
identifier = text . tail . init . show

prettyE :: Show a => Int -> Exp a -> Doc
prettyE _ (Var a) = identifier a
prettyE d (fun `App` arg) = maybeParens (d > appPrec) $
    prettyE appPrec fun <> space <> prettyE (succ appPrec) arg
prettyE d (Lam n t b) = maybeParens (d > lamPrec) $
    text "λ" <> identifier n <>
    text ":" <> prettyT 0 t <>
    text "." <> prettyE lamPrec b
prettyE d (Let a def term) = maybeParens (d > letPrec) $
    text "let "
    <> identifier a
    <> text "=" <> prettyE letPrec def
    $$ text "in " <> prettyE letPrec term
prettyE d (Pair e1 e2) = maybeParens (d > termPrec) $
    text "{" <> prettyE 0 e1 <> text ", " <> prettyE 0 e1 <> text "}"
prettyE d (ProjL e) = maybeParens (d > termPrec) $
    text "fst " <> prettyE termPrec e
prettyE d (ProjR e) = maybeParens (d > termPrec) $
    text "snd " <> prettyE termPrec e
prettyE d (InjL e t) = maybeParens (d > termPrec) $
     text "left " <> prettyE 0 e <>
     text ": " <> prettyT 0 t
prettyE d (InjR e t) = maybeParens (d > termPrec) $
    text "right " <> prettyE 0 e <>
    text ": " <> prettyT 0 t
prettyE d (Case n1 l n2 r e) = maybeParens (d > termPrec) $
    text "case " <> prettyE 0 e <>
    text "inl " <> identifier n1 <> text "⇒" <> prettyE 0 l <>
    text "inr " <> identifier n2 <> text "⇒" <> prettyE 0 r
prettyE d (TypeOf e t) = maybeParens (d > typeOfPrec) $
    prettyE typeOfPrec e <> text ": " <> prettyT typeOfPrec t
prettyE _ Unit = text "triv"

prettyT :: Show a => Int -> Type a -> Doc
prettyT _ (TVar a) = identifier a
prettyT _ Top = text "⊤"
prettyT _ Bot = text "⊥"
prettyT d (Prod t1 t2) = maybeParens (d > prodPrec) $
    prettyT prodPrec t1 <> space <> text "×" <> space <> prettyT (succ prodPrec) t2
prettyT d (Sum t1 t2) = maybeParens (d > sumPrec) $
    prettyT sumPrec t1 <> space <> text "+" <> space <> prettyT (succ sumPrec) t2
prettyT d (Arrow t1 t2) = maybeParens (d > arrowPrec) $
    prettyT arrowPrec t1 <> space <> text "→" <> space <> prettyT (succ arrowPrec) t2

-- precedence table
appPrec     = 9
lamPrec     = 6
letPrec     = 10
termPrec    = 10
typeOfPrec  = 0

prodPrec  = 6
sumPrec   = 5
arrowPrec = 4

prettyPrint :: Show a => Exp a -> String
prettyPrint = render . prettyE 0
