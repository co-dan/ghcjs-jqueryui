{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ImpredicativeTypes #-}

module JavaScript.JQuery.UI.Class
    ( -- * Main class
      Widget(..)
      -- * Helper functions
    , obj
    , (^=)
    ) where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Aeson          as A
import Data.Default
import Data.Monoid        (mconcat)
import Data.Text           (Text)
import GHCJS.Foreign       as F
import GHCJS.Marshal
import GHCJS.Types

class Show a => Widget a where
    data WidgetOpts  a
    -- data WidgetEvnts a
    defOpts :: WidgetOpts a
    default defOpts :: (Default (WidgetOpts a)) => WidgetOpts a
    defOpts = def
    optsObj :: WidgetOpts a -> IO (JSObject a)
    default optsObj :: (ToJSON (WidgetOpts a)) => WidgetOpts a -> IO (JSObject a)
    optsObj opts = castRef <$> toJSRef_aeson (toJSON opts)


-- * Helper functions
    
(^=) :: ToJSRef a => Text -> a -> (JSRef b -> IO (JSRef b))
p ^= v = \o -> do
    v' <- toJSRef v
    F.setProp p v' o
    return o

obj :: [JSRef a -> IO (JSRef a)] -> IO (JSObject b)
obj props = do
    o <- newObj
    o' <- foldr (>=>) return props o
    return $ (castRef o' :: JSObject b)
