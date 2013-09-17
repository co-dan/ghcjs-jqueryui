{-# LANGUAGE OverloadedStrings        #-}
module Main where

import JavaScript.JQuery
import JavaScript.JQuery.UI
import Data.Default

mkbtn handler = do
    body <- select "body"
    newbtn <- select "<button></button>"
    initWidget newbtn Button with { buttonLabel = "bla" }
    appendJQuery newbtn body
    _ <- click (\_ -> handler newbtn) def newbtn
    append "<br /> <br />" body
    return newbtn
    
main = do
    mkbtn $ \x -> applyEffect x Blind with { blindDirection = "right"
                                           , blindEasing    = "linear" }
    mkbtn $ \x -> applyEffect x Blind with { blindDirection = "right"
                                           , blindEasing    = "easeOutBounce" }
    mkbtn $ \x -> applyEffect x Blind with { blindDirection = "right"
                                           , blindEasing    = "easeInOutCirc" }

    mkbtn $ \x -> applyEffect x Bounce with { bounceTimes    = 5
                                            , bounceDistance = 100 }
    
    

    
    
