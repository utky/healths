{-# LANGUAGE OverloadedStrings #-}
module Healths.View where

import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Healths.Core

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
      H.title t
    body $ do
      c
      superagent


profile :: UserId -> Profile -> Html
profile uid pf = frame (toHtml uid) $ do
  header $ h1 "Health Tracker"
  article $ weightEditor uid pf
  footer $ do
    H.div $ H.span "Yutaka Imamura"


inputText
  :: AttributeValue -- ^ input id
  -> AttributeValue -- ^ input value
  -> Html           -- ^ label
  -> Html
inputText n v l =
  H.div $ do
    H.label
      ! A.for n $ l
    H.input
      ! A.id n
      ! A.name n
      ! type_ "text"
      ! value v

weightEditor :: UserId -> Profile -> Html
weightEditor uid pf =
  H.form $ do
    inputText "weight"             (toValue $ profileWeight pf)  "Weight: "
    inputText "bmi"                (toValue $ profileBMI pf)     "BMI: "
    inputText "body_fat"           (toValue $ profileBodyFat pf) "Body Fat: "
    inputText "muscle"             (toValue $ profileMuscle pf)  "Muscle: "
    inputText "visceral_fat_level" (toValue $ profileMuscle pf)  "Visceral Fat Level: "
    H.input
      ! type_ "button"
      ! A.value "save"
      ! A.onclick (sendForm uid)

sendForm :: UserId -> AttributeValue
sendForm uid = toValue $ concat
  [ "request"
  , ".post(\"/profiles/" ++ (T.unpack uid) ++ "\")"
  , ".send({"
  ,  "  weight: parseFloat(document.querySelector(\"#weight\").value)"
  ,  ", bmi:    parseFloat(document.querySelector(\"#bmi\").value)"
  ,  ", body_fat: parseFloat(document.querySelector(\"#body_fat\").value)"
  ,  ", muscle: parseFloat(document.querySelector(\"#muscle\").value)"
  ,  ", visceral_fat_level: parseFloat(document.querySelector(\"#visceral_fat_level\").value)"
  , "})"
  , ".end(function (err, res) { console.log(res) });"
  ]
