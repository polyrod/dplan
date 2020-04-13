module Gui where

import Graphics.UI.WX
import Graphics.UI.WXCore

imageNone :: Int
imageNone = (-1)

showGui :: [String] -> IO ()
showGui ss = start $ do
    f <- frame [text := "DPlan" , position := Point 450 350]
    menu_datei <- menuPane [ text := "&Datei" ]
    menu_datei_beenden <- menuItem menu_datei [ text := "&Beenden\tCtrl+C" , help := "Programm beenden"]
    contentpanel <- panel f []
    quit <- button f [ text := "Beenden" , on command := close f ]
    let t = if null ss
              then []
              else head ss
    txt <- textCtrl f [enabled := False,wrap := WrapNone ,size := Size 700 500, font := fontDefault{ _fontFace = "Courier New", _fontSize = 7, _fontWeight = WeightBold }]
    spn <- spinCtrl f 1 (length ss) []
    set spn [on select := do s <- get spn selection 
                             updateTxt txt s ]
    set txt [ text := t]
    set f [ layout := margin 10 $ column 5 [ floatBottomLeft $ boxed "Lösungen" (margin 5 $ fill $ widget txt)
                                           ,hstretch $ alignBottomRight $ row 5 $ [widget spn,widget quit]]
           ,size := Size 900 400                                   
          ]
    statusbar <- statusField [text := "Statusbar"]
    return ()
  where 
    updateTxt t sel = do 
      set t [ text := ss !! (sel-1) ]


