{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}


module LambdaCms.Core.Handler.Home
  ( getAdminHomeR
  ) where

import LambdaCms.Core.Import

getAdminHomeR :: CoreHandler Html
getAdminHomeR = lift $ adminLayout [whamlet|Welcome to the admin section!|]
