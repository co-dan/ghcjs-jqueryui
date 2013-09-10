{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}
module JavaScript.JQuery.UI where

import           Control.Applicative
import           Data.Aeson                    as A
import           Data.Default
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHCJS.Marshal
import           GHCJS.Foreign                 as F
import           GHCJS.Types
import           JavaScript.JQuery
import           JavaScript.JQuery.Internal

import           JavaScript.JQuery.UI.Internal

with :: Default a => a
with = def

initWidget :: (Widget a, Show a, ToJSON (WidgetOpts a)) => JQuery -> a -> WidgetOpts a -> IO ()
initWidget jq widget wopts = do
    opts <- optsObj wopts
    jq_setOptsWidget (toJSString (show widget)) opts jq

class Widget a where
    data WidgetOpts  a
    data WidgetEvnts a
    defOpts :: WidgetOpts a
    default defOpts :: (Default (WidgetOpts a)) => WidgetOpts a
    defOpts = def
    optsObj :: WidgetOpts a -> IO (JSObject a)
    default optsObj :: (ToJSON (WidgetOpts a)) => WidgetOpts a -> IO (JSObject a)
    optsObj opts = castRef <$> toJSRef_aeson (toJSON opts)

---------------------------

data ButtonWidget = Button

instance Show ButtonWidget where
    show _ = "button"
    
button :: ButtonWidget
button = Button

instance Widget ButtonWidget where
    data WidgetOpts ButtonWidget = ButtonOptions
                                   { buttonDisabled :: Bool
                                   , buttonIcons    :: A.Value
                                   , buttonLabel    :: String
                                   , buttonText     :: Bool
                                   } deriving (Show)
    data WidgetEvnts ButtonWidget = CreateEvent
    optsObj (ButtonOptions{..}) = do
        o <- newObj
        icons <- toJSRef_aeson buttonIcons
        let (^=) :: Text -> JSRef a -> IO ()
            p ^= v = F.setProp p v o
        "disabled" ^= toJSBool   buttonDisabled
        "icons"    ^= icons
        "label"    ^= toJSString buttonLabel
        "text"     ^= toJSBool   buttonText 
        return o


instance Default (WidgetOpts ButtonWidget) where
    def = ButtonOptions
          { buttonDisabled = False
          , buttonIcons    = toJSON ()
          , buttonLabel    = ""
          , buttonText     = True
          }
                             

instance ToJSON (WidgetOpts ButtonWidget) where
    toJSON (ButtonOptions{..}) = object [ "disabled" .= buttonDisabled
                                        , "icons"    .= buttonIcons
                                        , "label"    .= buttonLabel
                                        , "text"     .= buttonText
                                        ]
