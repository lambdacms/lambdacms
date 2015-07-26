{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module %PACKAGE%.%MODEL%
       ( module Export
       )where

import %PACKAGE%.%MODEL%.Import
import %PACKAGE%.%MODEL%.Foundation        as Export
import %PACKAGE%.%MODEL%.Models            as Export
import %PACKAGE%.%MODEL%.Handler.%MODEL%   as Export


instance %PACKAGE%%MODEL% master => YesodSubDispatch %MODEL%Admin (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resources%MODEL%Admin)
