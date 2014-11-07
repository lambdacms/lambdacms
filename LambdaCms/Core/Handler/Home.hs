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
-- getAdminHomeR = lambdaCoreLayout $(whamletFile "templates/adminhome.hamlet")
getAdminHomeR = do
  tp <- getRouteToParent
  let route = tp AdminHomeR
  mcr <- getCurrentRoute
  tryoutLayout $(whamletFile "templates/adminhome.hamlet")
  -- tryoutLayout [whamlet|
  --               $# @{route}
  --               $maybe cr <- mcr
  --                 @{cr}
  --               |]
