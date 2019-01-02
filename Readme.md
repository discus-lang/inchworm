# Inchworm

Inchworm is a simple parser combinator framework specialized to
lexical analysis.
Tokens are specified via simple fold functions, and we include
baked in source location handling.

Matchers for standard tokens like comments and strings 
are in the `Text.Lexer.Inchworm.Char` module.

No dependencies other than the Haskell `base` library.

If you want to parse expressions instead of performing lexical
analysis then try the `parsec` or `attoparsec` packages, which
have more general purpose combinators.

## Minimal example

The following code demonstrates how to perform lexical analysis
of a simple LISP-like language. We use two separate name classes,
one for variables that start with a lower-case letter, 
and one for constructors that start with an upper case letter. 

Integers are scanned using the `scanInteger` function from the 
`Text.Lexer.Inchworm.Char` module.

The result of `scanStringIO` contains the list of leftover input
characters that could not be parsed. In a real lexer you should
check that this is empty to ensure there has not been a lexical
error.


```
import Text.Lexer.Inchworm.Char
import qualified Data.Char as Char

-- | A source token.
data Token 
        = KBra | KKet | KVar String | KCon String | KInt Integer
        deriving Show

-- | A thing with attached location information.
data Located a
        = Located FilePath (Range Location) a
        deriving Show

-- | Scanner for a lispy language.
scanner :: FilePath
        -> Scanner IO Location [Char] (Located Token)
scanner fileName
 = skip Char.isSpace
 $ alts [ fmap (stamp id)   $ accept '(' KBra
        , fmap (stamp id)   $ accept ')' KKet
        , fmap (stamp KInt) $ scanInteger 
        , fmap (stamp KVar)
          $ munchWord (\ix c -> if ix == 0 then Char.isLower c
                                           else Char.isAlpha c) 
        , fmap (stamp KCon) 
          $ munchWord (\ix c -> if ix == 0 then Char.isUpper c
                                           else Char.isAlpha c)
        ]
 where  -- Stamp a token with source location information.
        stamp k (range, t) 
          = Located fileName range (k t)

main :: IO ()
main 
 = do   let fileName = "Source.lispy"
        let source   = "(some (Lispy like) 26 Program 93 (for you))"
        let toks     = scanString source (scanner fileName)
        print toks
```
