module Language.STLambda.Parser where

import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as S
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Expr as E

import Language.STLambda.Syntax

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
    keys = ["let", "in"
           , "True", "TRUE", "Top", "Unit"
           , "False", "FALSE", "Bot", "Zero"
           , "And", "AND"
           , "Or", "OR"
           , "fst", "snd"
           , "left", "right"
           ]
    ops = ["\\", "λ", ".", ":", "=", ","
          , "⇒", "=>"
          , "⊤"
          , "⊥"
          , "→", "->"
          , "×", "*", "/\\"
          , "+", "\\/"
          , "<*", "*>"
          , ">+", "+<"
          ]
    style = emptyDef
        { T.reservedNames = keys
        , T.reservedOpNames = ops
        , T.identStart = P.alphaNum <|> P.char '_'
        , T.identLetter = P.alphaNum <|> P.char '_'
        , T.commentLine = "--"
        , T.commentStart = "{-"
        , T.commentEnd  = "-}"
        }

parens :: S.Parser a -> S.Parser a
parens = T.parens lexer

braces :: S.Parser a -> S.Parser a
braces = T.braces lexer

colon :: S.Parser String
colon = T.colon lexer

comma :: S.Parser String
comma = T.comma lexer

dot :: S.Parser String
dot = T.dot lexer

symbol :: String -> S.Parser String
symbol = T.symbol lexer

reserved :: String -> S.Parser ()
reserved = T.reserved lexer

reservedOp :: String -> S.Parser ()
reservedOp = T.reservedOp lexer

identifier :: S.Parser String
identifier = T.identifier lexer

whiteSpace :: S.Parser ()
whiteSpace = T.whiteSpace lexer

-- parsing type expressions
variableTP :: S.Parser (Type String)
variableTP = TVar <$> identifier

atomTP :: S.Parser (Type String)
atomTP = variableTP <|> parens typeP

topP :: S.Parser (Type String)
topP = do
    (reservedOp "⊤" <|> reserved "Top" <|> reserved "True" <|> reserved "TRUE")
    return Top

botP :: S.Parser (Type String)
botP = do
    (reservedOp "⊥" <|> reserved "Bot" <|> reserved "False" <|> reserved "FALSE")
    return Bot

arrowP :: S.Parser (Type String)
arrowP = do
    t1 <- variableTP
    (reservedOp "→" <|> reservedOp "->")
    t2 <- variableTP
    return (t1 `Arrow` t2)

prodP :: S.Parser (Type String)
prodP = do
    t1 <- variableTP
    (reservedOp "×" <|> reservedOp "*")
    t2 <- variableTP
    return (t1 `Prod` t2)

sumP :: S.Parser (Type String)
sumP = do
    t1 <- variableTP
    (reservedOp "+")
    t2 <- variableTP
    return (t1 `Sum` t2)

typeP :: S.Parser (Type String)
typeP = E.buildExpressionParser typeOps
        (   parens typeP
        <|> variableTP
        <|> topP
        <|> botP
        )
  where
    typeOps =
        [
          [ E.Infix (reservedOp "*" >> return Prod) E.AssocRight
          , E.Infix (reservedOp "×" >> return Prod) E.AssocRight
          , E.Infix (reservedOp "/\\" >> return Prod) E.AssocRight
          ]
        , [ E.Infix (reservedOp "+" >> return Sum  ) E.AssocRight
          , E.Infix (reservedOp "\\/" >> return Sum) E.AssocRight
          ]
        , [ E.Infix (reservedOp "→" >> return Arrow) E.AssocRight
          , E.Infix (reservedOp "->" >> return Arrow ) E.AssocRight
          ]
        ]

-- parsing expressions
variableP :: S.Parser (Exp String)
variableP = Var <$> identifier

atomP :: S.Parser (Exp String)
atomP = variableP -- <|> parens exprP

appP :: S.Parser (Exp String)
appP = atomP `P.chainl1` pure App

lamP :: S.Parser (Exp String)
lamP = do
    reservedOp "\\" <|> reservedOp "λ"
    n <- identifier
    colon
    t <- typeP
    dot
    e <- expP
    return (Lam n t e)

letP :: S.Parser (Exp String)
letP = do
    reserved "let"
    n <- identifier
    reservedOp "="
    d <- expP
    reserved "in"
    term <- expP
    return (Let n d term)

pairP :: S.Parser (Exp String)
pairP = braces pP
  where
    pP = do
       e1 <- expP
       comma
       e2 <- expP
       return (Pair e1 e2)

projlP :: S.Parser (Exp String)
projlP = do
    (reservedOp "<*" <|> reserved "fst")
    e <- expP
    symbol "1"
    return (ProjL e)

projrP :: S.Parser (Exp String)
projrP = do
    (reservedOp "*>" <|> reserved "snd")
    e <- expP
    return (ProjR e)

injlP :: S.Parser (Exp String)
injlP = do
   (reservedOp ">+" <|> reserved "left")
   e <- expP
   t <- typeP
   return (InjL e t)

injrP :: S.Parser (Exp String)
injrP = do
   (reservedOp "+<" <|> reserved "right")
   e <- expP
   t <- typeP
   return (InjR e t)

caseP :: S.Parser (Exp String)
caseP = do
    n1 <- identifier
    l <- expP
    n2 <- identifier
    r <- expP
    e <- expP
    return (Case n1 l n2 r e)

expP :: S.Parser (Exp String)
expP = do
    whiteSpace
    postfixChain nonleft colonOp
  where
    nonleft = P.choice [P.try caseP, P.try injrP, P.try injlP, P.try projrP, P.try pairP, P.try letP, P.try lamP, P.try appP, atomP]
    colonOp = do
       colon
       t <- typeP
       return (`TypeOf` t)

expression :: String -> Either P.ParseError (Exp String)
expression = P.parse expP "ExpParser"

postfixChain :: S.Parser a -> S.Parser (a -> a) -> S.Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 rest $ f x) <|> return x

