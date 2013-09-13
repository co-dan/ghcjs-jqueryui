{-# LANGUAGE OverloadedStrings        #-}
module Main where

import JavaScript.JQuery
import JavaScript.JQuery.UI
import JavaScript.JQuery.UI.Class
import Data.Default

main = do
    btn <- select "#bttn"    
    initWidget btn Button with { buttonLabel    = "Hello, world"
                               , buttonDisabled = True }

    div1 <- select "#accordion"
    initWidget div1 Accordion with { accordionEvent = "mouseover" }

    sliderDiv <- select "#slider"
    initWidget sliderDiv Slider with { sliderValue = 10 }

    pbarDiv <- select "#progressbar"
    initWidget pbarDiv Progressbar with { progressbarValue = 50 }

    menuDiv <- select "#menu"
    initWidget menuDiv Menu defOpts

    dialogDiv <- select "#dialog"
    opener    <- select "#opener"
    initWidget dialogDiv Dialog with { dialogAutoOpen = False }
    let handler _ = widgetMethod dialogDiv Dialog "open"
    _ <- click handler def opener 
    return ()
    
