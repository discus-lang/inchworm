
module Text.Lexer.Inchworm.Scanner
        ( Scanner (..)
        , scanSourceToList)
where
import Text.Lexer.Inchworm.Source


-- | Scanner of input tokens.
data Scanner m is a
        = Scanner
        { runScanner  :: Source m is -> m (Maybe a) }


scanSourceToList
        :: Monad m
        => Source m [i] -> Scanner m [i] a -> m [a]

scanSourceToList ss sn@(Scanner load)
 = go []
 where  go acc
         =  load ss >>= \result
         -> case result of
                Just x  -> go (x : acc)
                Nothing -> return $ reverse acc


