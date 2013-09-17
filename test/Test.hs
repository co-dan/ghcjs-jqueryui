{-# LANGUAGE OverloadedStrings        #-}
module Main where

import JavaScript.JQuery
import JavaScript.JQuery.UI
import Data.Default

main = do
    body <- select "body"
    newbtn <- select "<button></button>"
    initWidget newbtn Button with { buttonLabel = "bla" }
    appendJQuery newbtn body
    
    btn <- select "#bttn"    
    initWidget btn Button with { buttonLabel    = "Hello, world"
                               , buttonDisabled = False }

    div1 <- select "#accordion"
    initWidget div1 Accordion with { accordionEvent = "mouseover" }

    sliderDiv <- select "#slider"
    initWidget sliderDiv Slider with { sliderValue = 10 }

    pbarDiv <- select "#progressbar"
    initWidget pbarDiv Progressbar with { progressbarValue = Val 10 }

    menuDiv <- select "#menu"
    initWidget menuDiv Menu def

    dialogDiv <- select "#dialog"
    opener    <- select "#opener"
    initWidget dialogDiv Dialog with { dialogAutoOpen = False }
    let handler _ = widgetMethod dialogDiv Dialog "open"
    _ <- click handler def opener

    -- tooltipP <- select "#ttip"
    -- initWidget tooltipP Tooltip with { tooltipContent = "HI" }

    spinnerInp <- select "#spinner"
    initWidget spinnerInp Spinner def
    return ()
    
