
module Text.Lexer.Inchworm.Scanner.Char
        ( scanInteger
        , scanHaskellishChar
        , scanHaskellCommentBlock
        , scanHaskellCommentLine)
where
import Text.Lexer.Inchworm
import qualified Data.Char                      as Char
import qualified Numeric                        as Numeric


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


-- Characters -----------------------------------------------------------------
-- | Scan a literal character, enclosed in ' characters.
--   
--   We handle the ascii, hex, octal and decimal 
--   escape codes listed in section 2.6 of the Haskell Report,
--   but not string gaps or the & null character.
--
scanHaskellishChar :: Scanner IO [Char] Char
scanHaskellishChar 
 = munchFold Nothing match (' ', True) accept
 where
        match 0 '\'' _                  = Just ('\'', True)
        match _ c   (_,     False)      = Nothing

        match ix c  (cPrev, True)
         | ix < 1                       = Nothing
         | c == '\'' && cPrev == '\\'   = Just ('\'', True)
         | c == '\''                    = Just (c,    False)
         | otherwise                    = Just (c,    True)

        accept ('\'' : c : cs)          = readChar (c : init cs)
        accept _                        = Nothing

readChar ('\\' : 'x' : cs)
 | [(x, [])]    <- Numeric.readHex cs   = Just $ Char.chr x
 | otherwise                            = Nothing

readChar ('\\' : 'o' : cs)
 | [(x, [])]    <- Numeric.readOct cs   = Just $ Char.chr x
 | otherwise                            = Nothing

readChar ('\\' : '^' : c : [])
 | c >= 'A' && c <= 'Z'                 = Just $ Char.chr (Char.ord c - 1)
 | c == '@'                             = Just $ Char.chr 0
 | c == '['                             = Just $ Char.chr 27
 | c == '\\'                            = Just $ Char.chr 28
 | c == ']'                             = Just $ Char.chr 29
 | c == '^'                             = Just $ Char.chr 30
 | c == '_'                             = Just $ Char.chr 31

readChar ('\\' : cs)
 | all Char.isDigit cs                  = Just $ Char.chr (read cs)

readChar ('\\' : cs)                    = lookup cs escapedChars
readChar [c]                            = Just c
readChar _                              = Nothing

escapedChars 
 =      [ ("a",   '\a'),   ("b", '\b'),     ("f",   '\f'),   ("n", '\n')
        , ("r",   '\r'),   ("t", '\t'),     ("v",   '\v'),   ("\\",  '\\')
        , ("\"",  '\"'),   ("\'",  '\'')
        , ("NUL", '\NUL'), ("SOH", '\SOH'), ("STX", '\STX'), ("ETX", '\ETX')
        , ("EOT", '\EOT'), ("ENQ", '\ENQ'), ("ACK", '\ACK'), ("BEL", '\BEL')
        , ("BS",  '\BS'),  ("HT",  '\HT'),  ("LF",  '\LF'),  ("VT",  '\VT')
        , ("FF",  '\FF'),  ("CR",  '\CR'),  ("SO",  '\SO'),  ("SI",  '\SI')
        , ("DLE", '\DLE'), ("DC1", '\DC1'), ("DC2", '\DC2'), ("DC3", '\DC3')
        , ("DC4", '\DC4'), ("NAK", '\NAK'), ("SYN", '\SYN'), ("ETB", '\ETB')
        , ("CAN", '\CAN'), ("EM",  '\EM'),  ("SUB", '\SUB'), ("ESC", '\ESC')
        , ("FS",  '\FS'),  ("GS",  '\GS'),  ("RS",  '\RS'),  ("US",  '\US')
        , ("SP",  '\SP'),  ("DEL", '\DEL')]


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


