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

-- * 'with' helper

-- | Example usage:
--
-- > initWidget .. Button with { buttonLabel = ... }
--
with :: Default a => a
with = def


-- * Widget functions

initWidget :: (Widget a) => JQuery -> a -> WidgetOpts a -> IO ()
initWidget jq widget wopts = do
    opts <- widgetOptsObj wopts
    jq_setOptsWidget (toJSString (show widget)) opts jq

widgetMethod :: (Widget a) => JQuery -> a -> Text -> IO (JSRef a)
widgetMethod jq widget method = jq_widgetMethod widget' method' jq
  where
    widget' = toJSString (show widget)
    method' = toJSString method

getWidgetOptions :: (Widget a, FromJSRef (WidgetOpts a))
                => JQuery -> a -> IO (Maybe (WidgetOpts a))
getWidgetOptions jq widget = fromJSRef . castRef
                           =<< jq_getOptsWidget widget' jq
  where widget' = toJSString (show widget)
    

-- * Effect functions

applyEffect :: (Effect a) => JQuery -> a -> EffectOpts a -> IO ()
applyEffect jq e eopts = do
    opts <- effectOptsObj eopts
    jq_effect (toJSString (show e)) opts jq
    
hideEffect :: (Effect a) => JQuery -> a -> EffectOpts a -> IO ()
hideEffect jq e eopts = do
    opts <- effectOptsObj eopts
    jq_hide (toJSString (show e)) opts jq

showEffect :: (Effect a) => JQuery -> a -> EffectOpts a -> IO ()
showEffect jq e eopts = do
    opts <- effectOptsObj eopts
    jq_show (toJSString (show e)) opts jq

toggleEffect :: (Effect a) => JQuery -> a -> EffectOpts a -> IO ()
toggleEffect jq e eopts = do
    opts <- effectOptsObj eopts
    jq_toggle (toJSString (show e)) opts jq

-- * Widgets
---------------------------

-- | If (Maybe a) is 'a nullable', than (Falsable a) is 'a falsable'
--
-- F = 'false', Val a = a
data Falsable a = F | Val a

instance (ToJSRef a) => ToJSRef (Falsable a) where
    toJSRef F       = return (castRef jsFalse)
    toJSRef (Val a) = castRef <$> toJSRef a

instance (FromJSRef a) => FromJSRef (Falsable a) where
    fromJSRef r = do
        mbool <- (fromJSRef $ castRef r) :: IO (Maybe Bool)
        case mbool of
            Just x  -> return $ if x then Nothing else Just F
            Nothing -> do
                ma <- fromJSRef $ castRef r
                return (Val <$> ma)

data Button = Button

mkWidget ''Button
    [ "disabled" <::> [t|Bool     |]
                 <==> [e|False    |]
      
    , "icons"    <::> [t|A.Value  |]
                 <==> [e|object []|]
      
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

    , "icons"       <::> [t|A.Value  |]
                    <==>
      [e|object [ "header" .= ("ui-icon-triangle-1-e" :: Text)
                , "activeHeader" .= ("ui-icon-triangle-1-s" :: Text)]|]
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
                  <==> [e|object []|]
    --- XXX: real source datatype
    , "source"    <::> [t|A.Value  |]
                  <==> [e|object []|]
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
                       <==> [e|object []|]
      
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


data Sortable = Sortable

mkWidget ''Sortable
    [ "appendTo" <::> [t|Selector|]
                 <==> [e|"parent"|]

    , "axis" <::> [t|Text|]
             <==> [e|"y" |]

    , "cancel" <::> [t|Selector                             |]
               <==> [e|"input,textarea,button,select,option"|]     

    , "connectWith" <::> [t|Falsable Selector|]
                    <==> [e|F      |]
      
    , "containment" <::> [t|Text|] -- *
                    <==> [e|""  |]

    , "cursor" <::> [t|String  |]
               <==> [e|"auto"|]

    , "cursorAt" <::> [t|A.Value|]
                 <==> [e|object []|]

    , "delay" <::> [t|Int|]
              <==> [e|0|]

    , "disabled" <::> [t|Bool|]
                 <==> [e|False|]

    , "distance" <::> [t|Int|]
                 <==> [e|1  |]

    , "dropOnEmpty" <::> [t|Bool|]
                    <==> [e|True |]

    , "forceHelperSize" <::> [t|Bool|]
                        <==> [e|False |]
    , "forcePlaceholderSize" <::> [t|Bool|]
                             <==> [e|False |]

    , "grid" <::> [t|Falsable (JSArray Double)|]
             <==> [e|F |]

    , "handle" <::> [t|Falsable Selector|]
               <==> [e|F  |]

      --- XXX: helper
    -- , "helper"     <::> [t|Either String|]
    --                  <==> [e|"original"  |]

    , "items" <::> [t|Selector|]
              <==> [e|"> *"|]


    , "opacity" <::> [t|Double|]
                <==> [e|1  |]

    , "placeholder" <::> [t|String|]
                    <==> [e|"" |]

    , "revert" <::> [t|Bool|]
               <==> [e|False  |]

    , "scroll" <::> [t|Bool|]
               <==> [e|True  |]
    , "scrollSensitivity" <::> [t|Int|]
                          <==> [e|20  |]
    , "scrollSpeed" <::> [t|Int|]
                    <==> [e|20  |]

    , "tolerance" <::> [t|String|]
                  <==> [e|"intersect" |]

    , "zindex" <::> [t|Int|]
               <==> [e|1000  |]
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

-- * Animation
-- XXX: use Clay for CSS?

data AnimateOpts = AnimateOpts { animateEasing   :: Text
                               , animateDuration :: Int
                               , animateCSS      :: A.Value }

instance ToJSRef AnimateOpts where
    toJSRef AnimateOpts{..} = castRef <$>
                              obj [ "easing"   ^= animateEasing
                                  , "duration" ^= animateDuration
                                  ]

instance Default AnimateOpts where
    def = AnimateOpts "linear" 400 (object [])
    
animate :: JQuery -> AnimateOpts -> IO JQuery
animate jq opts = do
    opts' <- castRef <$> toJSRef opts
    css   <- castRef <$> toJSRef (animateCSS opts)
    jq_animate css opts' jq

-- * Effects

data Blind = Blind

mkEffect ''Blind
    [ "direction"  <::> [t|Text|]
                   <==> [e|"up"|]
    ]

data Bounce = Bounce

mkEffect ''Bounce
    [ "times"    <::> [t|Int|]
                 <==> [e|3  |]
    , "distance" <::> [t|Int|]
                 <==> [e|20 |]
    ]

    
