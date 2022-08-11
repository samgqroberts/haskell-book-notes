import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Functor.Identity

{-| Write the code -}

-- 1., 2.

rDec :: Num a => Reader a a
rDec =
    ReaderT $ Identity . ((-) 1)

-- 3., 4.

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

-- 5.

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStrLn $ "Hi: " <> (show a)
    return $ a + 1

-- 6.

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
    let asString = show s
    putStrLn $ "Hi: " <> (asString)
    return (asString, s + 1)

{-| Fix the code -}

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
    v <- getLine
    -- to use `guard` to translate isValid into a Maybe, the `do` needs to be in the Maybe context
    return $ do
        guard $ isValid v
        return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT $ maybeExcite

    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn ("Good, was very excite: " ++ e)

fx = Just 5
fy = Just "hi"
fz = Nothing

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Monad m => m () -> (() -> m b) -> m b

maybetest :: Maybe String
maybetest = do
    x <- Nothing
    y <- fy
    _ <- Nothing :: Maybe (Maybe (Maybe (Maybe Int)))
    return "hi"