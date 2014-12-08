{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module LambdaCms.Media
       ( module Export
       ) where

import           LambdaCms.Media.Foundation    as Export
import           LambdaCms.Media.Handler.Media as Export
import           LambdaCms.Media.Import
import           LambdaCms.Media.Models        as Export

instance LambdaCmsMedia master => YesodSubDispatch MediaAdmin (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesMediaAdmin)
