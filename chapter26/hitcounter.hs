{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

{-|
    it's pretty annoying that in ghci I can't just import the imports if the module is broken.
    it doesn't even stick around if i load the imports successfully once then re-load with the module broken again.
    I have to find the right import and import that manually in ghci.
-}

{-|
    immediately got stuck, had to follow the suggestion to look at a scotty example on github.
    interestingly the example used a ConfigM newtype wrapper around Config to accomplish config-getting
    in the get handler. We can just do that with the ReaderT in the Scotty type definition and `asks`

    Looks like in the scotty examples they're using a new monad like ConfigM to perform what we're doing
    directly with ReaderT? And using TVar where we're using IORef it looks like too.
-}

data Config =
    Config {
        -- that's one, one click!
        -- two... two clicks!
        -- Three BEAUTIFUL clicks! ah ah ahhhh
        counts :: IORef (M.Map Text Integer)
        , prefix :: Text
    }


type Scotty =
    ScottyT Text (ReaderT Config IO)
type Handler =
    ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
    -> M.Map Text Integer
    -> (M.Map Text Integer, Integer)
bumpBoomp k m =
    (newMap, newCount)
    where
        newCount = maybe 1 (+1) $ M.lookup k m
        newMap = M.insert k newCount m

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key"
        prefixText <- lift $ asks prefix
        let
            key' :: Text
            key' = mappend prefixText unprefixed
        countsIORef <- lift $ asks counts
        let
            _check :: IORef (M.Map Text Integer)
            _check = countsIORef
        countsMap <- liftIO $ readIORef countsIORef
        let
            _check :: M.Map Text Integer
            _check = countsMap
            countResult :: (M.Map Text Integer, Integer)
            countResult = bumpBoomp key' countsMap
            newInteger :: Integer
            newInteger = snd countResult
        -- write new map value to IORef
        liftIO $ writeIORef countsIORef (fst countResult)
        html $
            mconcat [ "<h1>Success! Count was: "
                , TL.pack $ show newInteger
                , "</h1>"
                ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter $ TL.pack prefixArg
        -- not sure what this is or is meant to do... middleware or something?
        -- but the types line up.
        runR m = runReaderT m config 
    scottyT 3000 runR app