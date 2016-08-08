{-# LANGUAGE RankNTypes, TypeFamilies, BangPatterns #-}
module Text.Lexer.Inchworm.Source
        ( Source (..)
        , makeListSourceIO)
where
import Data.IORef
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM


-- | Source of data values of type 'i'.
data Source m i
        = Source
        { -- | Skip over values from the source that match the given predicate.
          sourceSkip    :: (i -> Bool) -> m ()

          -- | Pull a value from the source,
          --   provided it matches the given predicate.
        , sourcePull    :: (i -> Bool) -> m (Maybe i)

          -- | Pull a sequence of values from the source that match the given predicate,
          --   with the given maximum sequence length.
        , sourcePulls   :: U.Unbox i
                        => Int -> (Int -> i -> Bool) -> m (Maybe (U.Vector i))

          -- | Try to evaluate the given computation that may pull values
          --   from the source. If it returns Nothing then rewind the 
          --   source to the original position.
        , sourceTry     :: forall a. m (Maybe a) -> m (Maybe a) }



-- | Make a source from a list of values,
--   maintaining the state in the IO monad.
makeListSourceIO :: Eq i => [i] -> IO (Source IO i)
makeListSourceIO cs0
 =  newIORef cs0 >>= \ref
 -> return 
 $  Source 
        (skipListSourceIO  ref)
        (pullListSourceIO  ref)
        (pullsListSourceIO ref)
        (tryListSourceIO   ref)
 where
        -- Skip values from the source.
        skipListSourceIO ref pred
         = do
                cc0     <- readIORef ref
                let eat !cc
                     = case cc of
                        []      
                         -> return ()

                        c : cs  
                         | pred c
                         -> eat cs

                         | otherwise 
                         -> do  writeIORef ref (c : cs)
                                return ()

                eat cc0

        -- Pull a single value from the source.
        pullListSourceIO ref pred
         = do  cc      <- readIORef ref
               case cc of
                []
                 -> return Nothing

                c : cs 
                 |  pred c 
                 -> do writeIORef ref cs
                       return $ Just c

                 | otherwise
                 ->    return Nothing


        -- Pull a vector of values that match the given predicate
        -- from the source.
        pullsListSourceIO ref lenMax pred
         = do   cc0     <- readIORef ref

                mvec    <- UM.new lenMax

                let eat !ix !cc
                     | ix >= lenMax
                     = return (ix, cc)

                     | otherwise
                     = case cc of
                        []      
                         -> return (ix, cc)

                        c : cs
                         |  pred ix c
                         -> do  UM.write mvec ix c
                                eat (ix + 1) cs

                         |  otherwise
                         -> return (ix, cc)

                (len, cc') <- eat 0 cc0
                case len of
                 0      -> return Nothing
                 _      -> do
                        let mvec'  =  UM.slice 0 len mvec
                        writeIORef ref cc'
                        vec     <- U.unsafeFreeze mvec'
                        return  $ Just vec


        -- Try to run the given computation,
        -- reverting source state changes if it returns Nothing.
        tryListSourceIO ref comp 
         = do   cc      <- readIORef ref
                mx      <- comp
                case mx of
                 Just i  
                  -> return (Just i)

                 Nothing 
                  -> do writeIORef ref cc
                        return Nothing

