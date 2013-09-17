{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Default
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           JavaScript.JQuery
import           JavaScript.JQuery.Internal
import           JavaScript.JQuery.UI
import           JavaScript.JQuery.UI.Class


data Box = Box { easing :: Text
               , box    :: JQuery }

addBox :: Text -> JQuery -> IO Box
addBox e tracks = do
    boxSpan <- select "<span>"
    addClass "box" boxSpan
    label <- select "<span>"
    addClass "label" label
    setText e label

    select "<div>"
        >>= appendJQuery boxSpan
        >>= appendJQuery label
        >>= appendToJQuery tracks

    return Box { easing = e, box = boxSpan }

main = do
    mkbtn Blind with { blindEasing    = "linear" }
    mkbtn Blind with { blindEasing    = "easeOutBounce" }
    mkbtn Blind with { blindEasing    = "easeInOutCirc" }

    tracks <- select "#tracks"
    boxes <- sequence $ map (flip addBox tracks)
             [ "linear", "swing", "easeOutBounce" ]
    width <- jq_getWidth tracks
    let handler _ = forM_ boxes $ \(Box easing box) -> forkIO $ do
            jq_stop True box
            setCss "left" "5px" box
            delta <- (-) <$> jq_getWidth tracks <*> jq_getWidth box
            let d = delta - 50
            animate box with { animateEasing   = easing
                             , animateCSS      = object [ "left" .= d ]
                             , animateDuration = 700 }
            threadDelay 1000000
            animate box with { animateEasing   = easing
                             , animateCSS      = object ["left" .= ("5px" :: String)]
                             , animateDuration = 700 }
            return ()

    select "#start-race"
        >>= click handler def
    return ()


mkbtn :: (Effect a) => a -> EffectOpts a -> IO JQuery
mkbtn eff effOpts = do
    body <- select "body"
    newbtn <- select "<button></button>"
    initWidget newbtn Button with { buttonLabel = "Will you click me?" }
    appendJQuery newbtn body
    let handler =  hideEffect newbtn eff effOpts
                >> showEffect newbtn eff effOpts
                >> return ()
    _ <- click (\_ -> handler) def newbtn
    append "<br /> <br />" body
    return newbtn
