{-# LANGUAGE BangPatterns, RankNTypes, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Lexer.Inchworm.Source
        ( Source   (..), Range(..), Location (..)
        , Sequence (..)
        , makeListSourceIO)
where
import Data.IORef
import Data.Maybe
import qualified Data.List              as List
import Prelude  hiding (length)


---------------------------------------------------------------------------------------------------
-- | Class of sequences of things.
class Sequence is where
 -- | An element of a sequence.
 type Elem is

 -- | Yield the length of a sequence.
 length :: is -> Int


instance Sequence [a] where
 type Elem [a]  = a
 length         = List.length


-- | An abstract source of input tokens that we want to perform lexical analysis on.
--
--   Each token is associated with a source location @loc@.
--   A a sequence of tokens has type @input@, and a single token type (`Elem` input).
--
data Source m loc input
        = Source
        { -- | Skip over values from the source that match the given predicate.
          sourceSkip    :: (Elem input -> Bool) -> m ()

          -- | Try to evaluate the given computation that may pull values
          --   from the source. If it returns Nothing then rewind the 
          --   source to the original position.
        , sourceTry     :: forall a. m (Maybe a) -> m (Maybe a)

          -- | Pull a value from the source, provided it matches the given predicate.
          --   In the result we get the location if the first and final element tha
          --   matched.
        , sourcePull    :: (Elem input -> Bool)
                        -> m (Maybe (Range loc, Elem input))

          -- | Use a fold function to select a some consecutive tokens from the source
          --   that we want to process, also passing the current index to the fold
          --   function.
          -- 
          --   The maximum number of tokens to select is set by the first argument,
          --   which can be set to `Nothing` for no maximum.
          --
          --   In the result we get the location of the first and final element that
          --   matched.
        , sourcePulls   :: forall s
                        .  Maybe Int 
                        -> (Int -> Elem input -> s -> Maybe s)
                        -> s
                        -> m (Maybe (Range loc, input))

          -- | Bump the source location using the given element.
        , sourceBumpLoc   :: Elem input -> loc -> loc

          -- | Get the remaining input.
        , sourceRemaining :: m (loc, input)
        }


---------------------------------------------------------------------------------------------------
-- | Make a source from a list of input tokens,
--   maintaining the state in the IO monad.
makeListSourceIO 
        :: forall i loc. Eq i
        => loc                    -- ^ Starting source location.
        -> (i -> loc -> loc)      -- ^ Function to bump the current location by one input token.
        -> [i]                    -- ^ List of input tokens.
        -> IO (Source IO loc [i])

makeListSourceIO loc00 bumpLoc cs0
 =  do  refLoc <- newIORef loc00
        refSrc <- newIORef cs0
        return $ Source
                (skipListSourceIO  refLoc refSrc)
                (tryListSourceIO   refLoc refSrc)
                (pullListSourceIO  refLoc refSrc)
                (pullsListSourceIO refLoc refSrc)
                (bumpLoc)
                (remainingSourceIO refLoc refSrc)
 where
        -- Skip values from the source.
        skipListSourceIO refLoc refSrc fPred
         = do
                loc0    <- readIORef refLoc
                cc0     <- readIORef refSrc

                let eat !loc !cc
                     = case cc of
                        []      
                         -> do  writeIORef refLoc loc
                                writeIORef refSrc []
                                return ()

                        c : cs  
                         |  fPred c
                         -> eat (bumpLoc c loc) cs

                         | otherwise 
                         -> do  writeIORef refLoc loc
                                writeIORef refSrc (c : cs)
                                return ()

                eat loc0 cc0

        -- Try to run the given computation,
        -- reverting source state changes if it returns Nothing.
        tryListSourceIO refLoc refSrc comp 
         = do   loc     <- readIORef refLoc
                cc      <- readIORef refSrc
                mx      <- comp
                case mx of
                 Just i  
                  -> return (Just i)

                 Nothing 
                  -> do writeIORef refLoc loc
                        writeIORef refSrc cc
                        return Nothing

        -- Pull a single value from the source.
        pullListSourceIO refLoc refSrc fPred
         = do   locFirst <- readIORef refLoc
                cc       <- readIORef refSrc
                case cc of
                 []
                  -> return Nothing

                 c : cs 
                  |  fPred c 
                  -> do writeIORef refLoc (bumpLoc c locFirst)
                        writeIORef refSrc cs
                        return $ Just (Range locFirst locFirst, c)

                  | otherwise
                  ->    return Nothing

        -- Pull a vector of values that match the given predicate
        -- from the source.
        pullsListSourceIO 
         :: IORef loc -> IORef [i]
         -> Maybe Int -> (Int -> i -> s -> Maybe s) 
         -> s         -> IO (Maybe (Range loc, [i]))

        pullsListSourceIO refLoc refSrc mLenMax work s0
         = do   lFirst  <- readIORef refLoc
                cc0     <- readIORef refSrc

                let eat !ix !(mlPrev :: Maybe loc) !(lHere :: loc) !cc !acc !s
                     | Just mx  <- mLenMax
                     , ix >= mx
                     =      return (ix, mlPrev, lHere, cc, reverse acc)

                     | otherwise
                     = case cc of
                        []      
                         -> return (ix, mlPrev, lHere, cc, reverse acc)

                        c : cs
                         -> case work ix c s of
                             Nothing -> return (ix, mlPrev, lHere, cc, reverse acc)
                             Just s' -> eat  (ix + 1) (Just lHere) (bumpLoc c lHere)
                                             cs       (c : acc)    s'
                (len, mlPrev, lEnd, cc', acc) 
                 <- eat 0 Nothing lFirst cc0 [] s0

                case len of
                 0  -> return Nothing
                 _  -> do writeIORef refLoc lEnd
                          writeIORef refSrc cc'

                          -- If we have only matched a single character then
                          -- the final position is set to the same as the 
                          -- starting position, otherwise it's set to the final
                          -- position in the token, *not* the next char to test.
                          let lFinal = fromMaybe lFirst mlPrev 
                          return $ Just (Range lFirst lFinal, acc)

        -- Get the remaining input.
        remainingSourceIO
         :: IORef loc -> IORef [i]
         -> IO (loc, [i])

        remainingSourceIO refLoc refSrc
         = do   loc     <- readIORef refLoc
                src     <- readIORef refSrc
                return  (loc, src)


-------------------------------------------------------------------------------
-- | A range of locations in a source file.
data Range loc
        = Range !loc !loc
        deriving Show


-- | A location in a source file.
---
--   We define this here so that we can use it to specialize
--   makeListSourceIO.
--
data Location
        = Location   
                !Int    -- Line.
                !Int    -- Column.
        deriving Show


{-# SPECIALIZE INLINE
     makeListSourceIO 
        :: Location
        -> (Char -> Location -> Location)
        -> [Char]
        -> IO (Source IO Location [Char])
 #-}
