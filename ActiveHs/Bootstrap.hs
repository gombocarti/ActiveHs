{-# LANGUAGE OverloadedStrings #-}

module ActiveHs.Bootstrap where

import Data.String (fromString)

import           Data.Monoid ((<>), mconcat, mempty)
import           Text.Blaze ((!))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type Identifier = String
type Icon = String
type Style = String

html :: H.Html -> H.Html -> H.Html
html title content = H.html (htmlHead <> body)
  where
    htmlHead :: H.Html
    htmlHead = H.head (mconcat [charset, httpEquiv, viewPort, title, bootstrapCss])

    charset :: H.Html
    charset = H.meta ! A.charset "UTF-8"

    httpEquiv :: H.Html
    httpEquiv = H.meta
                  ! A.httpEquiv "X-UA-Compatible"
                  ! A.content "IE=edge"

    viewPort :: H.Html
    viewPort = H.meta
                 ! A.name "viewport"
                 ! A.content "width=device-width, initial-scale=1"

    bootstrapCss :: H.Html
    bootstrapCss = H.link
                     ! A.href "static/css/bootstrap.min.css"
                     ! A.rel "stylesheet"

    body :: H.Html
    body = H.body (jquery <> bootstrapJs <> content)

    jquery :: H.Html
    jquery = H.script
               ! A.src "static/js/jquery.min.js"
               ! A.type_ "javascript"
               $ mempty

    bootstrapJs :: H.Html
    bootstrapJs = H.script
                    ! A.src "static/js/bootstrap.min.js"
                    ! A.type_ "javascript"
                    $ mempty

pageTitle :: String -> H.Html
pageTitle = H.title . fromString

textarea :: H.Html
textarea = H.textarea
             ! A.class_ "form-control"
             ! A.rows "5"
             $ mempty

button :: String -> H.Html
button label = H.button
                 ! A.class_ "btn btn-primary"
                 ! A.type_ "button"
                 $ fromString label

row :: H.Html -> H.Html
row = H.div ! A.class_ "row"

rowColMd12 :: H.Html -> H.Html
rowColMd12 = H.div ! A.class_ "col-md-12"

progressbar :: H.Html
progressbar = H.div
                ! A.class_ "progress"
                $ H.div
                  ! A.class_ "progress-bar progress-bar-striped active"
                  ! role "progressbar"
                  ! B.customAttribute "aria-valuenow" "100"
                  ! B.customAttribute "aria-valuemin" "0"
                  ! B.customAttribute "aria-valuemax" "100"
                  $ mempty

textArea :: Identifier -> H.Html
textArea ident = H.textarea
                   ! A.class_ "form-control"
                   ! A.id (fromString ident)
                   ! A.rows "5"
                   $ mempty

inputWithAddon :: Identifier -> H.Html -> H.Html
inputWithAddon ident addonContent =
  H.div
    ! A.class_ "input-group"
    $ addon <> textInput ident
  where
    addon :: H.Html
    addon = H.span
              ! A.class_ "input-group-addon"
              $ addonContent

textInput :: Identifier -> H.Html
textInput ident =
  H.input
    ! A.id (fromString ident)
    ! A.type_ "text"
    ! A.class_ "form-control"

textInputWithFeedback :: Identifier -> Style -> Icon -> String -> H.Html
textInputWithFeedback ident style icon srText = 
  H.div
    ! A.class_ (fromString (style ++ "form-group has-feedback"))
    ! B.customAttribute "aria-describedby" (fromString srAidIdent)
    $ textInput ident <> feedbackIcon <> srAid
  where
    feedbackIcon :: H.Html
    feedbackIcon = H.span
                     ! A.class_ (fromString (icon ++ "glyphicon form-control-feedback"))
                     ! B.customAttribute "aria-hidden" "true"
                     $ mempty

    srAidIdent :: Identifier
    srAidIdent = ident ++ "Status"

    srAid :: H.Html
    srAid = H.span
               ! A.id (fromString srAidIdent)
               ! A.class_ "sr-only"
               $ fromString (srText)

textInputSuccess :: String -> H.Html
textInputSuccess ident = textInputWithFeedback ident "has-success" "glyphicon-ok" "success"

textInputFailure :: String -> H.Html
textInputFailure ident = textInputWithFeedback ident "has-error" "glyphicon-remove" "error"

form :: String -> H.Html -> H.Html
form action = H.form ! A.action (fromString action)

well :: H.Html -> H.Html
well = H.div ! A.class_ "well well-sm"

role :: B.AttributeValue -> B.Attribute
role = B.customAttribute "role"

