
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
