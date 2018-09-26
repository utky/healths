{-# LANGUAGE OverloadedStrings #-}
module Healths.View where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Healths.Types

superagent :: Html
superagent = do
  H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/superagent/3.8.3/superagent.min.js" $ ""

  H.script $ "var request = window.superagent;"

frame
  :: Html -- ^ title
  -> Html -- ^ content
  -> Html -- ^ whole markup
frame t c =
  docTypeHtml $ do
    H.head $ do
      H.meta ! charset "UTF-8"
      H.link
        ! A.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
        ! A.rel "stylesheet"
      H.title t
    body $ do
      c
      superagent


profile :: UserId -> Profile -> Html
profile uid pf = frame (toHtml $ T.concat ["Health Tracker: ", uid]) $ do
  article $ weightEditor uid pf
  footer $ do
    H.div $ H.span "Copyright (C) Yutaka Imamura All Rights Reserved."


inputText
  :: AttributeValue -- ^ input id
  -> AttributeValue -- ^ input value
  -> Html           -- ^ label
  -> Html
inputText n v l =
  H.div ! class_ "field" $ do
    H.label
      ! class_ "label"
      ! A.for n $ l
    H.input
      ! class_ "control"
      ! A.id n
      ! A.name n
      ! type_ "text"
      ! value v

weightEditor :: UserId -> Profile -> Html
weightEditor uid pf =
  H.div $ do
    inputText "weight"             (toValue $ profileWeight pf)  "Weight: "
    inputText "bmi"                (toValue $ profileBMI pf)     "BMI: "
    inputText "body_fat"           (toValue $ profileBodyFat pf) "Body Fat: "
    inputText "muscle"             (toValue $ profileMuscle pf)  "Muscle: "
    inputText "visceral_fat_level" (toValue $ profileVisceralFatLevel pf)  "Visceral Fat Level: "
    H.div ! class_ "control" $ do
      H.button
        ! class_ "button is-link"
        ! A.onclick "sendState();" $ "save"
      H.script $ toHtml $ unlines
        [ "function sendState() {"
        , "  request"
        , "  .post(\"/profiles/" ++ (T.unpack uid) ++ "\")"
        , "  .send({"
        ,    "  weight: parseFloat(document.querySelector(\"#weight\").value)"
        ,    ", bmi:    parseFloat(document.querySelector(\"#bmi\").value)"
        ,    ", body_fat: parseFloat(document.querySelector(\"#body_fat\").value)"
        ,    ", muscle: parseFloat(document.querySelector(\"#muscle\").value)"
        ,    ", visceral_fat_level: parseFloat(document.querySelector(\"#visceral_fat_level\").value)"
        , "  })"
        , "  .end(function (err, res) { console.log(res) });"
        , "}"
        ]
