
import Text.Lexer.Inchworm
import qualified Data.Char      as Char

data Token
        = KKeyWord      String
        | KVar          String
        | KCon          String
        | KPunc         String
        deriving Show

main
 = do   ss      <- makeListSourceIO "(box (run {:de:}))"

        let scanner
                = skip Char.isSpace
                $ alts  [ scanPunc
                        , scanKeyVar
                        , scanCon
                        ]

        result  <- scanSourceToList ss scanner

        print result


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
 = Char.isLower c

isVarBody  :: Char -> Bool
isVarBody c
 =  Char.isAlpha c
 || Char.isDigit c
 || c == '_' || c == '\''


-- Constructors ---------------------------------------------------------------
scanCon :: Scanner IO [Char] Token
scanCon
 = munch Nothing matchCon acceptCon
 where  
        -- | Match a constructor name.
        matchCon  :: Int -> Char -> Bool
        matchCon 0 c    = isConStart c
        matchCon _ c    = isConBody  c

        -- | Accept a constructor name.
        acceptCon :: [Char] -> Maybe Token
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
 || c == '_' || c == '\''


