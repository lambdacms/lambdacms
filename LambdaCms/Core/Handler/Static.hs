{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

module LambdaCms.Core.Handler.Static
       ( getNormalizeR
       , getBootstrapCssR
       , getBootstrapJsR
       , getJQueryR
       , getGlyphiconsWoffR
       , getGlyphiconsTtfR
       , getGlyphiconsEotR
       , getGlyphiconsSvgR
       ) where

import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)
import LambdaCms.Core.Import

getStaticFile :: ByteString -> ByteString -> CoreHandler TypedContent
getStaticFile mime efile = return $ TypedContent mime
                                  $ toContent efile

getNormalizeR :: CoreHandler TypedContent
getNormalizeR = getStaticFile "text/css" $(embedFile "static/css/normalize.css")

getBootstrapCssR :: CoreHandler TypedContent
getBootstrapCssR = getStaticFile "text/css" $(embedFile "static/css/bootstrap.min.css")

getBootstrapJsR :: CoreHandler TypedContent
getBootstrapJsR = getStaticFile "text/js" $(embedFile "static/js/bootstrap.min.js")

getJQueryR :: CoreHandler TypedContent
getJQueryR = getStaticFile "text/js" $(embedFile "static/js/jquery.min.js")

getGlyphiconsWoffR :: CoreHandler TypedContent
getGlyphiconsWoffR = getStaticFile "application/font-woff" $(embedFile "static/fonts/glyphicons.woff")

getGlyphiconsTtfR :: CoreHandler TypedContent
getGlyphiconsTtfR = getStaticFile "application/font-ttf" $(embedFile "static/fonts/glyphicons.ttf")

getGlyphiconsEotR :: CoreHandler TypedContent
getGlyphiconsEotR = getStaticFile "application/font-eot" $(embedFile "static/fonts/glyphicons.eot")

getGlyphiconsSvgR :: CoreHandler TypedContent
getGlyphiconsSvgR = getStaticFile "image/svg+xml" $(embedFile "static/fonts/glyphicons.svg")
