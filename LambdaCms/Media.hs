{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.Media
       ( module Export
       , LambdaCmsMedia (..)
       , defaultMediaAdminMenu
       ) where

import LambdaCms.Media.Import
import LambdaCms.Media.Models            as Export
import LambdaCms.Media.Routes            as Export
import LambdaCms.Media.Handler.MediaFile as Export

instance LambdaCmsMedia master => YesodSubDispatch Media (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesMedia)
