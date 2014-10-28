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

-- getAdminHomeR :: CoreHandler Html
-- getAdminHomeR = do
--   toParent <- getRouteToParent
--   lift $ adminLayout $ do
--     lambdaAdminMenuWidget toParent
--     $(whamletFile "templates/adminhome.hamlet")

getAdminHomeR :: CoreHandler Html
getAdminHomeR = lambdaCoreLayout $(whamletFile "templates/adminhome.hamlet")
