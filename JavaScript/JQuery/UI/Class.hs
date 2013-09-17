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
    ( -- * Main classes
      Widget(..), Effect(..)
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

class (Show a, Default (WidgetOpts a)) => Widget a where
    data WidgetOpts  a
    widgetOptsObj :: WidgetOpts a -> IO (JSObject a)
    default widgetOptsObj :: (ToJSON (WidgetOpts a)) => WidgetOpts a -> IO (JSObject a)
    widgetOptsObj opts = castRef <$> toJSRef_aeson (toJSON opts)

class (Show a, Default (EffectOpts a)) => Effect a where
    data EffectOpts a
    effectOptsObj :: EffectOpts a -> IO (JSObject a)
    default effectOptsObj :: (ToJSON (EffectOpts a))  => EffectOpts a -> IO (JSObject a)
    effectOptsObj opts = castRef <$> toJSRef_aeson (toJSON opts)


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
