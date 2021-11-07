{-# LANGUAGE BangPatterns #-}
-- | Character based scanners.
module Text.Lexer.Inchworm.Char
        ( module Text.Lexer.Inchworm

          -- * Driver
        , scanString
        , scanStringIO

          -- * Locations
        , Range (..), Location (..)
        , bumpLocationWithChar

          -- * Scanners
        , scanInteger
        , scanHaskellChar
        , scanHaskellString
        , scanHaskellCommentBlock
        , scanHaskellCommentLine)
where
import Text.Lexer.Inchworm
import Text.Lexer.Inchworm.Source
import System.IO.Unsafe
import qualified Data.Char              as Char
import qualified Data.List              as List
import qualified Numeric                as Numeric


-- Driver ---------------------------------------------------------------------
-- | Scan a string.
scanString
 :: String
 -> Scanner IO Location String a
 -> ([a], Location, String)

scanString str scanner
 = unsafePerformIO
 $ scanListIO
        (Location 0 0)
        bumpLocationWithChar
        str scanner


-- | Implementation for `scanString`,
--   that uses the IO monad to maintain internal state.
scanStringIO
 :: String
 -> Scanner IO Location String a
 -> IO ([a], Location, String)

scanStringIO str scanner
 = scanListIO
        (Location 0 0)
        bumpLocationWithChar
        str scanner


-- Locations ------------------------------------------------------------------
-- | Bump a location using the given character,
--   updating the line and column number as appropriate.
bumpLocationWithChar :: Char -> Location -> Location
bumpLocationWithChar c (Location line col)
 = case c of
        '\n'    -> Location (line + 1) 0
        _       -> Location line (col + 1)


-- Integers -------------------------------------------------------------------
-- | Scan a decimal integer, with optional @-@ and @+@ sign specifiers.
scanInteger
 :: Monad m
 => Scanner m loc [Char] (Range loc, Integer)

scanInteger
 = munchPred Nothing matchInt acceptInt
 where
        matchInt  0 !c
         = c == '-' || c == '+' || Char.isDigit c

        matchInt  _ !c  = Char.isDigit c

        acceptInt ('+' : cs)
         | null cs      = Nothing

        acceptInt ('-' : cs)
         | null cs      = Nothing

        acceptInt cs    = Just $ read cs

{-# SPECIALIZE INLINE
     scanInteger
        :: Scanner IO Location [Char] (Range Location, Integer)
  #-}

-- Strings --------------------------------------------------------------------
-- | Scan a literal string,    enclosed in double quotes.
--
--   We handle the escape codes listed in Section 2.6 of the Haskell Report.
--
scanHaskellString
 :: Monad   m
 => Scanner m loc [Char] (Range loc, String)

scanHaskellString
 = munchFold Nothing matchC (False, False) acceptC
 where
        -- Expect double quotes as first char.
        matchC 0 '\"' _                 = Just (False, False)

        -- Matcher is done.
        matchC _  _  (True, _bEscape)   = Nothing

        -- Match a character.
        matchC ix c  (False, bEscape)
         | ix < 1                       = Nothing
         | c == '"',  bEscape           = Just (False, False)
         | c == '"'                     = Just (True,  False)
         | c == '\\', bEscape           = Just (False, False)
         | c == '\\'                    = Just (False, True)
         | otherwise                    = Just (False, False)

        acceptC ('"' : cs)
         = case decodeString cs of
                (str, ('"' : []))       -> Just str
                _                       -> Nothing

        acceptC _                       = Nothing

{-# SPECIALIZE INLINE
     scanHaskellString
        :: Scanner IO Location [Char] (Range Location, String)
  #-}


-- Characters -----------------------------------------------------------------
-- | Scan a literal character, enclosed in single quotes.
--
--   We handle the escape codes listed in Section 2.6 of the Haskell Report.
--
scanHaskellChar
 :: Monad   m
 => Scanner m loc [Char] (Range loc, Char)

scanHaskellChar
 = munchFold Nothing matchC (False, False) acceptC
 where
        matchC 0 '\'' _                 = Just (False, False)
        matchC _  _  (True,  _bEscape)  = Nothing

        matchC ix c  (False,  bEscape)
         | ix < 1                       = Nothing
         | c == '\'', bEscape           = Just (False, False)
         | c == '\''                    = Just (True,  False)
         | c == '\\', bEscape           = Just (False, False)
         | c == '\\'                    = Just (False, True)
         | otherwise                    = Just (False, False)

        acceptC ('\'' : cs)
         = case readChar cs of
                -- Character literals do not support gaps or
                -- escape terminators
                Just (Just c, "\'")     -> Just c
                _                       -> Nothing

        acceptC _                       =  Nothing

{-# SPECIALIZE INLINE
     scanHaskellChar
        :: Scanner IO Location [Char] (Range Location, Char)
  #-}


-- Comments -------------------------------------------------------------------
-- | Scan a Haskell block comment.
scanHaskellCommentBlock
 :: Monad   m
 => Scanner m loc [Char] (Range loc, String)

scanHaskellCommentBlock
 = munchFold Nothing matchC (' ', True) acceptC
 where
        matchC 0 '{' _                  = Just ('{', True)
        matchC 1 '-' ('{', True)        = Just ('-', True)

        matchC _   _  (_,     False)    = Nothing

        matchC ix  c  (cPrev, True)
         | ix < 2                       = Nothing
         | cPrev == '-' && c == '}'     = Just ('}', False)
         | otherwise                    = Just (c,   True)

        acceptC cc@('{' : '-' : _)      = Just cc
        acceptC _                       = Nothing

{-# SPECIALIZE INLINE
     scanHaskellCommentBlock
        :: Scanner IO Location [Char] (Range Location, String)
  #-}


-- | Scan a Haskell line comment.
scanHaskellCommentLine
        :: Monad   m
        => Scanner m loc [Char] (Range loc, String)

scanHaskellCommentLine
 = munchPred Nothing matchC acceptC
 where
        matchC 0 '-'                    = True
        matchC 1 '-'                    = True
        matchC _ '\n'                   = False
        matchC ix _
         | ix < 2                       = False
         | otherwise                    = True

        acceptC ('-' : '-' : cs)        = Just cs
        acceptC _                       = Nothing

{-# SPECIALIZE INLINE
     scanHaskellCommentLine
        :: Scanner IO Location [Char] (Range Location, String)
  #-}


-------------------------------------------------------------------------------
-- | Decode escape codes in a string.
decodeString :: String -> (String, String)
decodeString ss0
 = go [] ss0
 where
        go !acc []
         = (reverse acc, [])

        go !acc ss@('\"' : _)
         = (reverse acc, ss)

        go !acc ss@(c : cs)
         = case readChar ss of
                Just (Just c', cs')  -> go (c' : acc) cs'
                Just (Nothing, cs')  -> go       acc  cs'
                Nothing              -> go (c  : acc) cs

-- | Result of reading a character: either a real char, or an empty string
--   that is a successful read, but contains no characters.
--   These empty strings are sometimes required to remove ambiguity:
--   for example,'\SO' and '\SOH' are both valid escapes.
--   To distinguish between the strings ['\SO', 'H'] and ['\SOH'],
--   it is necessary to explicitly terminate the escape for the former:
--   '\SO\&H' means ['\SO', 'H'].
type CharGap = Maybe Char

-- | Read a character literal, handling escape codes.
readChar :: String -> Maybe (CharGap, String)

-- Control characters defined by hex escape codes.
readChar ('\\' : 'x' : cs)
 | [(x, rest)]  <- Numeric.readHex cs   = Just (Just $ Char.chr x, rest)
 | otherwise                            = Nothing

-- Control characters defined by octal escape codes.
readChar ('\\' : 'o' : cs)
 | [(x, rest)]  <- Numeric.readOct cs   = Just (Just $ Char.chr x, rest)
 | otherwise                            = Nothing

-- Control characters defined by carret characters, like \^G
readChar ('\\' : '^' : c : rest)
 | c >= 'A' && c <= 'Z' = Just (Just $ Char.chr (Char.ord c - 1), rest)
 | c == '@'             = Just (Just $ Char.chr 0,  rest)
 | c == '['             = Just (Just $ Char.chr 27, rest)
 | c == '\\'            = Just (Just $ Char.chr 28, rest)
 | c == ']'             = Just (Just $ Char.chr 29, rest)
 | c == '^'             = Just (Just $ Char.chr 30, rest)
 | c == '_'             = Just (Just $ Char.chr 31, rest)

-- Control characters defined by decimal escape codes.
readChar ('\\' : cs)
 | (csDigits, csRest)   <- List.span Char.isDigit cs
 , not $ null csDigits
 = Just (Just $ Char.chr (read csDigits), csRest)

-- Escape terminator '\&': see CharGap above
readChar ('\\' : '&' : rest)
 = Just (Nothing, rest)

-- String gap: two backslashes enclosing whitespace.
-- As above, this is equivalent to an empty string.
readChar ('\\' : cs)
 -- At least one character of whitespace
 | (_:_, '\\' : rest) <- List.span Char.isSpace cs
 = Just (Nothing, rest)

-- Control characters defined by ASCII escape codes.
readChar ('\\' : cs)
 = let  go [] = Nothing
        go ((str, c) : moar)
         = case List.stripPrefix str cs of
                Nothing   -> go moar
                Just rest -> Just (Just c, rest)

   in   go escapedChars

-- Just a regular character.
readChar (c : rest)     = Just (Just c, rest)

-- Nothing to read.
readChar _              = Nothing

escapedChars :: [(String, Char)]
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
