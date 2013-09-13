{-# LANGUAGE OverloadedStrings        #-}
module Main where

import JavaScript.JQuery
import JavaScript.JQuery.UI

main = do
    btn <- select "button"    
    initWidget btn Button with { buttonLabel    = "Hello, world"
                               , buttonDisabled = True }
    return ()
