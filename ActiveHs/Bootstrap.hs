{-# LANGUAGE OverloadedStrings #-}

module ActiveHs.Bootstrap where

import           Data.Monoid ((<>), mconcat, mempty)
import qualified Lucid as L
import qualified Lucid.Base as L (makeAttribute)
import qualified Data.Text as T

type Identifier = T.Text
type Icon = String
type Style = String

type Html = L.Html ()

html :: Html -> Html -> Html
html title content = L.doctypehtml_ (htmlHead <> body)
  where
    htmlHead :: Html
    htmlHead = L.head_ (mconcat [charset, httpEquiv, viewPort, title, bootstrapCss])

    charset :: Html
    charset = L.meta_ [ L.charset_ "UTF-8" ]

    httpEquiv :: Html
    httpEquiv = L.meta_
                  [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]

    viewPort :: Html
    viewPort = L.meta_
                 [ L.name_ "viewport"
                 , L.content_ "width=device-width, initial-scale=1"
                 ]

    bootstrapCss :: Html
    bootstrapCss = L.link_
                     [ L.href_ "static/css/bootstrap.min.css"
                     , L.rel_  "stylesheet"
                     ]

    body :: Html
    body = L.body_ (jquery <> popper <> bootstrapJs <> content)

    jquery :: Html
    jquery = L.script_
               [ L.src_ "static/js/jquery.min.js"
               , L.type_ "javascript"
               ]
               (mempty :: Html)

    popper :: Html
    popper = L.script_
               [ L.src_ "static/js/popper.min.js"
               , L.type_ "javascript"
               ]
               (mempty :: Html)

    bootstrapJs :: Html
    bootstrapJs = L.script_
                    [ L.src_ "static/js/bootstrap.min.js"
                    , L.type_ "javascript"
                    ]
                    (mempty :: Html)

pageTitle :: T.Text -> Html
pageTitle = L.title_ . L.toHtml

bootstrapPage :: T.Text -> Html -> Html
bootstrapPage title body = html (pageTitle title) (bootstrapFrame body)

bootstrapFrame :: Html -> Html
bootstrapFrame = L.div_ [ L.class_ "container" ]

row :: Html -> Html
row = L.div_ [ L.class_ "row" ]

col :: Html -> Html
col = L.div_ [ L.class_ "col" ]

col4Offset4 :: Html -> Html
col4Offset4 = L.div_ [ L.class_ "col-4 offset-md-4" ]

rowCol :: Html -> Html
rowCol = row . col

textarea :: Html
textarea = L.textarea_
             [ L.class_ "form-control"
             , L.rows_ "5"
             ]
             mempty

button :: T.Text -> Html
button label = L.button_
                 [ L.class_ "btn btn-primary"
                 , L.type_ "button"
                 ]
                 (L.toHtml label)

progressbar :: Html
progressbar = L.div_
                [ L.class_ "progress" ]
                $ L.div_
                  [ L.class_ "progress-bar progress-bar-striped active"
                  , L.role_ "progressbar"
                  , L.makeAttribute "aria-valuenow" "100"
                  , L.makeAttribute "aria-valuemin" "0"
                  , L.makeAttribute "aria-valuemax" "100"
                  ]
                  mempty

textArea :: Identifier -> Html
textArea ident = L.textarea_
                   [ L.class_ "form-control"
                   , L.id_ ident
                   , L.rows_ "5"
                   ]
                   mempty

inputWithAddon :: Identifier -> Html -> Html
inputWithAddon ident addonContent =
  L.div_
    [ L.class_ "input-group" ]
    (addon <> textInput ident)
  where
    addon :: Html
    addon = L.span_
              [ L.class_ "input-group-addon" ]
              addonContent

textInput :: Identifier -> Html
textInput ident =
  L.input_
    [ L.id_ ident
    , L.type_ "text"
    , L.class_ "form-control"
    ]

textInputWithFeedback :: Identifier -> Style -> Icon -> T.Text -> Html
textInputWithFeedback ident style icon srText =
  L.div_
    [ L.class_ (T.pack $ style ++ "form-group has-feedback")
    , L.makeAttribute "aria-describedby" srAidIdent
    ]
    $ textInput ident <> feedbackIcon <> srAid
  where
    feedbackIcon :: Html
    feedbackIcon = L.span_
                     [ L.class_ (T.pack $ icon ++ "glyphicon form-control-feedback")
                     , L.makeAttribute "aria-hidden" "true"
                     ]
                     mempty

    srAidIdent :: Identifier
    srAidIdent = T.append ident "Status"

    srAid :: Html
    srAid = L.span_
               [ L.id_ srAidIdent
               , L.class_ "sr-only"
               ]
               $ L.toHtml srText

textInputSuccess :: Identifier -> Html
textInputSuccess ident = textInputWithFeedback ident "has-success" "glyphicon-ok" "success"

textInputFailure :: Identifier -> Html
textInputFailure ident = textInputWithFeedback ident "has-error" "glyphicon-remove" "error"

form :: T.Text -> Html -> Html
form action = L.form_ [ L.formaction_ action ]

card :: Html -> Html
card = L.div_
         [ L.class_ "card" ]
         . L.div_
             [ L.class_ "card-body" ]

data AlertColor
  = Success
  | Warning
  | Error

alertColorCata :: a -> a -> a -> AlertColor -> a
alertColorCata success warning error color =
  case color of
    Success -> success
    Warning -> warning
    Error   -> error

alert :: AlertColor -> Html -> Html
alert alertColor = L.div_ [ L.class_ "alert", L.class_ color, L.role_ "alert" ]
  where
    color :: T.Text
    color = alertColorCata
              "alert-success"
              "alert-warning"
              "alert-danger"
              alertColor
