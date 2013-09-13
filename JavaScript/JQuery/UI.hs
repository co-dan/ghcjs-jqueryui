{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
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
import           JavaScript.JQuery.UI.TH
import           JavaScript.JQuery.UI.Class

with :: Widget a => WidgetOpts a
with = defOpts

initWidget :: (Widget a, Show a) => JQuery -> a -> WidgetOpts a -> IO ()
initWidget jq widget wopts = do
    opts <- optsObj wopts
    jq_setOptsWidget (toJSString (show widget)) opts jq

---------------------------

data Button = Button

mkWidget ''Button
    [ "disabled" <::> [t|Bool     |]
                 <==> [e|False    |]
      
    , "icons"    <::> [t|A.Value  |]
                 <==> [e|toJSON ()|]
      
    , "label"    <::> [t|String   |]
                 <==> [e|""       |]
      
    , "text"     <::> [t|Bool     |]
                 <==> [e|True     |]
    ]
