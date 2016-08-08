
module Text.Lexer.Inchworm.Scanner.Char
        ( scanInteger
        , scanHaskellCommentBlock
        , scanHaskellCommentLine)
where
import Text.Lexer.Inchworm
import qualified Data.Char              as Char


-- Integers -------------------------------------------------------------------
-- | Scan a literal integer, with optional '-' and '+' sign specifiers.
scanInteger :: Scanner IO [Char] Integer
scanInteger 
 = munchPred Nothing matchInt acceptInt
 where
        matchInt  0 c  
         = c == '-' || c == '+' || Char.isDigit c

        matchInt  _ c           = Char.isDigit c

        acceptInt ('+' : cs)
         | null cs              = Nothing

        acceptInt ('-' : cs)
         | null cs              = Nothing

        acceptInt cs            = Just $ read cs



-- Comments -------------------------------------------------------------------
-- | Scan a Haskell block comment.
scanHaskellCommentBlock :: Scanner IO [Char] String
scanHaskellCommentBlock
 = munchFold Nothing match (' ', True) accept
 where
        match 0 '{' _                   = Just ('{', True)
        match 1 '-' ('{', True)         = Just ('-', True)

        match _  c  (_,     False)      = Nothing

        match ix  c  (cPrev, True)
         | ix < 2                       = Nothing
         | cPrev == '-' && c == '}'     = Just ('}', False)
         | otherwise                    = Just (c,   True)

        accept cc@('{' : '-' : cs)      = Just cc
        accept _                        = Nothing


-- | Scan a Haskell line comment.
scanHaskellCommentLine :: Scanner IO [Char] String
scanHaskellCommentLine 
 = munchPred Nothing match accept
 where
        match 0 '-'     = True
        match 1 '-'     = True
        match _ '\n'    = False
        match ix _       
         | ix < 2       = False
         | otherwise    = True

        accept cs       = Just cs


