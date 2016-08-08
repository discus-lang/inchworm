
import Text.Lexer.Inchworm
import qualified Data.Char      as Char

data Token
        = KPunc         String
        | KKeyWord      String
        | KVar          String
        | KCon          String
        | KOp           String
        | KLit          Lit
        deriving Show

data Lit
        = LInteger      Integer
        deriving Show


main
 = do   ss      <- makeListSourceIO "(box (run {:de:} -1234 + 37))"
        result  <- scanSourceToList ss scanner
        print result

scanner
 = skip Char.isSpace
 $ alts [ -- Punctuation.
          scanPunc

          -- Keywords and variables.
          -- These are handled in one scanner because keyword names like
          -- 'box' have the same lexical structure as variable names.
        , scanKeyVar

          -- Constructor names.
        , scanCon

          -- Literal integers.
          -- Needs to come before scanOp    so we don't take '-' independently.
        , scanLitInteger

          -- Operator names.
          -- Needs to come after LitInteger so we don't take '-' independently.
        , scanOp
        ]



-- Punctuation ----------------------------------------------------------------
puncs1 
 =      [ '(', ')'
        , '[', ']'
        , '{', '}'
        , '.', ',', ';' ]

puncs2 
 =      [ "[:", ":]", "{:", ":}" ]

-- | Scan a punctuation character.
scanPunc  :: Scanner IO [Char] Token
scanPunc   
 = alt  (munch (Just 2) matchPunc2  acceptPunc2)
        (from           acceptPunc1)
 where  
        acceptPunc1 :: Char -> Maybe Token
        acceptPunc1 c
         | elem c puncs1        = Just $ KPunc [c]
         | otherwise            = Nothing

        matchPunc2 :: Int -> Char -> Bool
        matchPunc2 0 c  = elem c ['[', '{', ':']
        matchPunc2 1 c  = elem c [']', '}', ':']
        matchPunc2 _ _  = False

        acceptPunc2 :: [Char] -> Maybe Token
        acceptPunc2 cs
                | elem cs puncs2        = Just $ KPunc cs
                | otherwise             = Nothing


-- Variables ------------------------------------------------------------------
keywords
 =      [ "import",     "export" 
        , "box",        "run"
        , "let",        "in"
        , "case",       "of"
        , "do"
        , "if",         "then",         "else"]


-- | Scan a keyword or variable.
scanKeyVar   :: Scanner IO [Char] Token
scanKeyVar
 = munch Nothing matchKeyVar acceptKeyVar
 where
        matchKeyVar  :: Int -> Char -> Bool
        matchKeyVar 0 c    = isVarStart c
        matchKeyVar _ c    = isVarBody  c

        acceptKeyVar :: [Char] -> Maybe Token
        acceptKeyVar cs
                | elem cs keywords      = Just $ KKeyWord cs
                | otherwise             = Just $ KVar     cs


isVarStart :: Char -> Bool
isVarStart c
 =  Char.isLower c
 || c == '?'

isVarBody  :: Char -> Bool
isVarBody c
 =  Char.isAlpha c
 || Char.isDigit c
 || c == '_' || c == '\'' || c == '$' || c == '#'


-- Constructors ---------------------------------------------------------------
scanCon :: Scanner IO [Char] Token
scanCon
 = munch Nothing matchCon acceptCon
 where  
        matchCon 0 c    = isConStart c
        matchCon _ c    = isConBody  c

        acceptCon cs    = Just $ KCon cs


-- | Character can start a constructor name
isConStart :: Char -> Bool
isConStart c
 = Char.isUpper c


-- | Character can be in the body of a constructor name.
isConBody  :: Char -> Bool
isConBody c
 =  Char.isAlpha c
 || Char.isDigit c 
 || c == '_' || c == '\'' || c == '#'


-- Operators ------------------------------------------------------------------
scanOp :: Scanner IO [Char] Token
scanOp 
 = munch Nothing matchOp acceptOp
 where
        matchOp 0 c     = isOpStart c
        matchOp _ c     = isOpBody  c

        acceptOp cs             = Just $ KOp cs


-- | Character can start an operator.
isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'                     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'                     || c == '/'     || c == '|'
        || c == '<'     || c == '>'


-- | Character can be part of an operator body.
isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'     || c == '^'     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'
        || c == '<'     || c == '>'


-- Literal Naturals -----------------------------------------------------------
scanLitInteger :: Scanner IO [Char] Token
scanLitInteger 
 = munch Nothing matchInt acceptInt
 where
        matchInt  0 c  
         = c == '-' || c == '+' || Char.isDigit c

        matchInt  _ c   = Char.isDigit c

        acceptInt ('+' : cs)
         | null cs              = Nothing

        acceptInt ('-' : cs)
         | null cs              = Nothing

        acceptInt cs            = Just $ KLit (LInteger (read cs))

