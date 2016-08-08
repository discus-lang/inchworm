
module Text.Lexer.Inchworm.Combinator
        ( satisfies
        , accept,  from
        , accepts, froms
        , alt, alts
        , munch
        , skip)
where
import Text.Lexer.Inchworm.Source
import Text.Lexer.Inchworm.Scanner
import qualified Data.Vector.Unboxed as U

-------------------------------------------------------------------------------
-- | Pull an input token if it matches the given predicate.
satisfies
        :: Monad m 
        => (i -> Bool) -> Scanner m i i

satisfies pred 
 =  Scanner $ \ss 
 -> sourcePull ss pred


-------------------------------------------------------------------------------
-- | Accept a fixed length sequence of tokens,
--   returning the given result value.
accepts  :: (Monad m, Eq i, U.Unbox i)
         => [i] -> a -> Scanner m i a
accepts is a
 = froms (length is)
         (\is' -> if is == is'
                        then Just a
                        else Nothing)


-- | Pull the next token if it is equal to the given one,
--   returning the given result value.
accept  :: (Monad m, Eq i)
        => i -> a -> Scanner m i a
accept i a
 = from (\i'   -> if i == i'
                        then Just a
                        else Nothing)


-------------------------------------------------------------------------------
-- | Use the given function to check whether to accept the next token,
--   returning the result it produces.
from    :: Monad m
        => (i -> Maybe a) -> Scanner m i a

from accept 
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mx       <- sourcePull ss (const True)
        case mx of
         Nothing -> return Nothing
         Just x  -> return $ accept x


-- | Use the given function to check whether to accept 
--   a fixed length sequence of tokens.
froms   :: (Monad m, U.Unbox i)
        => Int -> ([i] -> Maybe a) -> Scanner m i a

froms len accept
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mx      <- sourcePulls ss len (\i c -> True)
        case mx of
         Nothing -> return Nothing
         Just xs -> return $ accept (U.toList xs)


-------------------------------------------------------------------------------
-- | Combine two scanners into a new one that 
--   tries the first before the second.
alt     :: Monad m 
        => Scanner m i a -> Scanner m i a -> Scanner m i a
alt (Scanner scan1) (Scanner scan2)
 =  Scanner $ \ss
 -> do  mx              <- sourceTry ss (scan1 ss)
        case mx of
         Nothing        -> scan2 ss
         Just r         -> return (Just r)


-- | Combine a list of scanners into  new one.
alts    :: Monad m
        => [Scanner m i a] -> Scanner m i a
alts [] 
 = Scanner $ \ss -> return Nothing

alts (Scanner scan1 : xs)
 = Scanner $ \ss
 -> do  mx              <- sourceTry ss (scan1 ss)
        case mx of
         Nothing        -> runScanner (alts xs) ss
         Just r         -> return (Just r)


-------------------------------------------------------------------------------
munch   :: (U.Unbox i, Monad m)
        => Int
        -> (Int -> i -> Bool)
        -> ([i] -> Maybe a)
        -> Scanner m i a

munch n pred accept
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mr              <- sourcePulls ss n pred 
        case mr of
         Nothing        -> return Nothing
         Just vec       
          -> case accept (U.toList vec) of
                Nothing -> return Nothing
                Just x  -> return $ Just x


-------------------------------------------------------------------------------
-- | A scanner that skips tokens that match the given predicate,
--   before applying the given scanner.
skip    :: Monad m
        => (i -> Bool) -> Scanner m i a -> Scanner m i a
skip pred (Scanner scan1)
 =  Scanner $ \ss
 -> do  sourceSkip ss pred
        scan1 ss

