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
with = def

initWidget :: (Widget a, Show a) => JQuery -> a -> WidgetOpts a -> IO ()
initWidget jq widget wopts = do
    putStrLn $ "InitWidget " ++ show widget
    opts <- widgetOptsObj wopts
    jq_setOptsWidget (toJSString (show widget)) opts jq

widgetMethod :: (Widget a) => JQuery -> a -> Text -> IO ()
widgetMethod jq widget method = jq_widgetMethod widget' method' jq
  where
    widget' = toJSString (show widget)
    method' = toJSString method

-- | If (Maybe a) is 'a nullable', than (Falsable a) is 'a falsable'
--
-- F = 'false', Val a = a
data Falsable a = F | Val a

instance (ToJSRef a) => ToJSRef (Falsable a) where
    toJSRef F       = return (castRef jsFalse)
    toJSRef (Val a) = castRef <$> toJSRef a


-- * Widgets
---------------------------

data Button = Button

mkWidget ''Button
    [ "disabled" <::> [t|Bool     |]
                 <==> [e|False    |]
      
    , "icons"    <::> [t|A.Value  |]
                 <==> [e|toJSON ()|]
      
    , "label"    <::> [t|Text     |]
                 <==> [e|""       |]
      
    , "text"     <::> [t|Bool     |]
                 <==> [e|True     |]
    ]

data Accordion = Accordion

mkWidget ''Accordion
    [ "active"      <::> [t|Int     |]
                    <==> [e|0       |]
      
    , "animate"     <::> [t|Text    |]
                    <==> [e|""      |]
      
    , "collapsible" <::> [t|Bool    |]
                    <==> [e|False   |]
      
    , "disabled"    <::> [t|Bool    |]
                    <==> [e|False   |]
    , "event"       <::> [t|Text    |]
                    <==> [e|"click" |]
      
    , "header"      <::> [t|Text    |]
                    <==> [e|
       "> li > :first-child,> :not(li):even"|]

    , "heightStyle" <::> [t|Text    |]
                    <==> [e|"auto"  |]

    , "icons"       <::> [t|A.Value |]
                    <==> [e|toJSON ()|]
    ]


data Autocomplete = Autocomplete

mkWidget ''Autocomplete
    [ "appendTo"  <::> [t|Text|]
                  <==> [e|""  |]
      
    , "autoFocus" <::> [t|Bool |]
                  <==> [e|False|]
      
    , "delay"     <::> [t|Int|]
                  <==> [e|300|]
      
    , "disabled"  <::> [t|Bool |]
                  <==> [e|False|]
      
    , "minLength" <::> [t|Int|]
                  <==> [e|1  |]
      
    , "position"  <::> [t|A.Value  |]
                  <==> [e|toJSON ()|]
    --- XXX: real source datatype
    , "source"    <::> [t|A.Value  |]
                  <==> [e|toJSON ()|]
    ]


data Datepicker = Datepicker

--- XXX: datepicker widget


data Dialog = Dialog

mkWidget ''Dialog
    [ "appendTo"       <::> [t|Text  |]
                       <==> [e|"body"|]
      
    , "autoOpen"       <::> [t|Bool|]
                       <==> [e|True|]
      
    , "buttons"        <::> [t|A.Value  |]
                       <==> [e|toJSON ()|]
      
    , "closedOnEscpae" <::> [t|Bool|]
                       <==> [e|True|]
      
    , "closeText"      <::> [t|Text   |]
                       <==> [e|"close"|]
      
    , "dialogClass"    <::> [t|Text|]
                       <==> [e|""  |]
      --- XXX: real height datatype (Either Int Auto)
    , "height"         <::> [t|Int|]
                       <==> [e|400|]
    , "hide"           <::> [t|Text|]
                       <==> [e|""  |]
    , "maxHeight"      <::> [t|Int|]
                       <==> [e|600|]
    , "maxWidth"       <::> [t|Int|]
                       <==> [e|800|]
    , "minHeight"      <::> [t|Int|]
                       <==> [e|150|]
    , "minWidth"       <::> [t|Int|]
                       <==> [e|150|]
    , "modal"          <::> [t|Bool |]
                       <==> [e|False|]
      --- XXX: Position datatype
    , "position"       <::> [t|Text    |]
                       <==> [e|"center"|]
    , "resizable"      <::> [t|Bool|]
                       <==> [e|True|]
    , "show"           <::> [t|Text|]
                       <==> [e|""  |]
    , "title"          <::> [t|Text|]
                       <==> [e|""  |]
    , "width"          <::> [t|Int |]
                       <==> [e|300 |]
    ]



data Menu = Menu

mkWidget ''Menu
    [ "disabled" <::> [t|Bool |]
                 <==> [e|False|]
      
    , "icons"    <::> [t|A.Value|]
        <==> [e|object ["submenu" .= ("ui-icon-carat-1-e" :: Text)]|]
      
    , "menus"    <::> [t|Text|]
                 <==> [e|"ul"|]
       --- XXX: Position datatype
    , "position" <::> [t|Text    |]
                 <==> [e|"center"|]
    
    , "role"     <::> [t|Text|]
                 <==> [e|""  |]
    ]


data Progressbar = Progressbar

mkWidget ''Progressbar
    [ "disabled" <::> [t|Bool |]
                 <==> [e|False|]
      
    , "max"      <::> [t|Int|]
                 <==> [e|100|]
      
    , "value"    <::> [t|Falsable Int|]
                 <==> [e|F           |]
    ]


    
data Slider = Slider

mkWidget ''Slider
    [ "animate"     <::> [t|Text |]
                    <==> [e|""   |]
    --- ^ XXX: proper speed type
    , "disabled"    <::> [t|Bool |]
                    <==> [e|False|]
      
    , "max"         <::> [t|Int|]
                    <==> [e|100|]
      
    , "min"         <::> [t|Int|]
                    <==> [e|0  |]

    , "orientation" <::> [t|Text        |]
                    <==> [e|"horizontal"|]

    , "range"       <::> [t|Bool |]
                    <==> [e|False|]

    , "step"        <::> [t|Int|]
                    <==> [e|0  |]

    , "value"       <::> [t|Int|]
                    <==> [e|0  |]

    , "values"      <::> [t|A.Value  |]
                    <==> [e|toJSON ()|]
    ]


data Spinner = Spinner

mkWidget ''Spinner
    [ "culture"     <::> [t|Text |]
                    <==> [e|""   |]
    , "disabled"    <::> [t|Bool |]
                    <==> [e|False|]

    , "icons"       <::> [t|A.Value|]
        <==> [e|object
                [ "down" .= ("ui-icon-triangle-1-s" :: Text)
                , "up"   .= ("ui-icon-triangle-1-n" :: Text)]|]
      
    , "incremental" <::> [t|Bool|]
                    <==> [e|True|]

    -- XXX
    -- , "max"         <::> [t|Int|]
    --                 <==> [e|0  |]
    -- , "min"         <::> [t|Int|]
    --                 <==> [e|0  |]

    , "numberFormat" <::> [t|Text|]
                     <==> [e|""  |]

    , "page"         <::> [t|Int|]
                     <==> [e|10|]

    , "step"         <::> [t|Int|]
                     <==> [e|1  |]
    ]


data Tabs = Tabs

mkWidget ''Tabs
    [ "active"      <::> [t|Int     |]
                    <==> [e|0       |]
      
    , "collapsible" <::> [t|Bool    |]
                    <==> [e|False   |]
      
    , "disabled"    <::> [t|Bool    |]
                    <==> [e|False   |]
    , "event"       <::> [t|Text    |]
                    <==> [e|"click" |]
      
    , "heightStyle" <::> [t|Text    |]
                    <==> [e|"content"  |]

    , "hide"        <::> [t|Text    |]
                    <==> [e|""  |]
    , "show"        <::> [t|Text    |]
                    <==> [e|""  |]
    ]

-- XXX: Tooltip
-- data Tooltip = Tooltip

-- mkWidget ''Tooltip
--     [ "content"      <::> [t|JSRef a |]
--                      <==> [e|""       |]      
--     ]

