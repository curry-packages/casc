{- |
    Module      :  AST.ASM
    Description :  Add Span Monad

    This module contains the Add Span Monad, which contains the token stream,
    some functions on the ASM and functions for parsing the token stream.
-}
module AST.ASM where

import AST.AST
import AST.Span  (Span)
import AST.Token

-- -----------------------------------------------------------------------------
-- *A*dd *P*osition *M*onad with functions
-- -----------------------------------------------------------------------------

-- |List containing tuples of positions and tokens: the token stream
type SpanTokens = [(Span, Token)]

-- |*A*dd *P*osition *M*onad
type ASM a = SpanTokens -> (Either String a, SpanTokens)

returnP :: a -> ASM a
returnP x ts = (Right x, ts)

failP :: String -> ASM a
failP s ts = (Left s, ts)

(>+=) :: ASM a -> (a -> ASM b) -> ASM b
m >+= f = \ts -> case m ts of
                   (Left s, ts')  -> (Left s, ts')
                   (Right a, ts') -> f a ts'

(>+) :: ASM a -> ASM b -> ASM b
m >+ m' = m >+= \ _ -> m'

(<$>) :: (a -> b) -> ASM a -> ASM b
f <$> m = m >+= \x -> returnP (f x)

(<*>) :: ASM (a -> b) -> ASM a -> ASM b
mf <*> mx = mf >+= \f -> mx >+= \x -> returnP (f x)

mapM :: (a -> ASM b) -> [a] -> ASM [b]
mapM _ []     = returnP []
mapM f (x:xs) = f x       >+= \ y  ->
                mapM f xs >+= \ ys ->
                returnP (y : ys)


-- -----------------------------------------------------------------------------
-- Functions for parsing the token stream
-- -----------------------------------------------------------------------------

-- |Read the next position without changing the state
readSpan :: ASM Span
readSpan ts@((p, _) : _) = returnP p ts
readSpan [] = failP "Token stream is empty." []

-- |Read the next token without changing the state
readToken :: ASM Token
readToken ts@((_, t) : _) = returnP t ts
readToken [] = failP "Token stream is empty." []

-- |Ensure a predicate. If predicate is `False`, fail.
ensure :: Show a => Bool -> a -> ASM a
ensure b x = if b
               then returnP x
               else failP $ "Function `ensure` failed (AST.ASM). "
                         ++ "This should have been returned: " ++ show x

-- |Return position of token t
tokenSpan :: Token -> ASM Span
tokenSpan t = getTokenSpan >+= \(p, t') -> ensure (t == t') p

-- |Get the next position, removing it from the state
getTokenSpan :: ASM (Span, Token)
getTokenSpan (t : ts) = returnP t ts
getTokenSpan [] = failP "Token stream is empty." []

-- |Get position of the next token which can be one of two possible alternatives
tokenSpanOneOf :: Token -> Token -> ASM Span
tokenSpanOneOf t1 t2 ((p,t') : ts)
  | t1 == t'  = returnP p ts
  | t2 == t'  = returnP p ts
  | otherwise = failP ("expected " ++ show t1 ++ " or "
                     ++ show t2 ++ ", got " ++ show t') ts
tokenSpanOneOf _ _ []  = failP "Token stream is empty." []

-- |Parse a chain of expressions, separated by a Token, e.g. a list: [1,2,3]
--  Process the expressions further with a function f, e.g. apLit
sepBy :: (a -> ASM b) -> ASM Span -> [a] -> ASM ([b], [Span])
sepBy f s ys = case ys of
  []     -> returnP ([], [])
  [x]    -> f x >+= \ px -> returnP ([px], [])
  (x:xs) -> f     x      >+= \ px         ->
            s            >+= \ ps         ->
            sepBy f s xs >+= \ (pxs, pss) ->
            returnP (px : pxs, ps : pss)

-- |Parse an expression surrounded by some kind of opening and closing symbols
between :: ASM a -> ASM b -> ASM c -> ASM (a, b, c)
between open f close =
  open  >+= \ po ->
  f     >+= \ pf ->
  close >+= \ pc ->
  returnP (po, pf, pc)

-- |Parse an expression that is surrounded by parens
parens :: ASM a -> ASM (Span, a, Span)
parens f = between (tokenSpan LeftParen) f (tokenSpan RightParen)

-- |Parse an expression that is surrounded by brackets
brackets :: ASM a -> ASM (Span, a, Span)
brackets f = between (tokenSpan LeftBracket) f (tokenSpan RightBracket)

-- |Parse an expression that is surrounded by braces
braces :: ASM a -> ASM (Span, a, Span)
braces f = between (tokenSpan LeftBrace) f (tokenSpan RightBrace)

-- |Apply function to `Maybe`
optional :: (a -> ASM b) -> Maybe a -> ASM (Maybe b)
optional _ Nothing  = returnP Nothing
optional f (Just x) = Just <$> f x

-- |Get position of next token which is optional and one of two alternatives
maybeOneOf :: Token -> Token -> ASM (Maybe Span)
maybeOneOf t1 t2 ts@((p,t) : ts')
  | t1 == t   = returnP (Just p) ts'
  | t2 == t   = returnP (Just p) ts'
  | otherwise = returnP Nothing ts
maybeOneOf _ _ []  = failP "Token stream is empty." []

-- |Return `Just` position of optional token or `Nothing`
maybeTokenSpan :: Token -> ASM (Maybe Span)
maybeTokenSpan t ts@((p,t') : ts')
  | t == t'   = returnP (Just p) ts'
  | otherwise = returnP Nothing ts
maybeTokenSpan _ []  = failP "Token stream is empty." []

-- |Parse an expression that might be surrounded
-- |by any kind of opening and closing symbols
maybeEnclosedBy :: [(Token, Token)] -> ASM a
                     -> ASM ((Maybe Span), a, (Maybe Span))
maybeEnclosedBy [] f = f >+= \ pf -> returnP (Nothing, pf, Nothing)
maybeEnclosedBy ((o, c):ocs) f =
  readToken >+= \t -> if (t == o)
                        then between (maybeTokenSpan o) f (maybeTokenSpan c)
                        else maybeEnclosedBy ocs f

-- |Parse an expression that might be surrounded by parens
maybeParens :: ASM a -> ASM ((Maybe Span), a, (Maybe Span))
maybeParens f = maybeEnclosedBy [(LeftParen, RightParen)] f

-- |Parse an expression non-deterministically. If the first parser fails, the
--  second is invoked. Otherwise the result of the first one is returned.
choose :: ASM a -> ASM a -> ASM a
choose f g = \ts -> case f ts of
                      (Left _, _) -> g ts
                      res         -> res
