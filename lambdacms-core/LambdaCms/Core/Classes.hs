{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LambdaCms.Core.Classes
       ( PageHeadProperties (..)
       ) where

import           LambdaCms.Core.Import

import           Data.Text             (intercalate)

type Follow = Bool
type Index = Bool

class PageHeadProperties res where
    title :: res -> Text
    author :: res -> Maybe Text
    author _ = Nothing
    editor :: res -> Maybe Text
    editor _ = Nothing
    description :: res -> Maybe Text
    description _ = Nothing
    keywords :: res -> [Text]
    keywords _ = []
    robots :: res -> Maybe (Index, Follow)
    robots _ = Nothing

    otherMetas :: res -> [[(Text, Text)]]
    otherMetas _ = []

    facebook :: res -> [(Text, Text)]
    facebook _ = []
    twitter :: res -> [(Text, Text)]
    twitter _ = []
    googlePlus :: res -> [(Text, Text)]
    googlePlus _ = []

    pageHeadTags :: LambdaCmsAdmin master => res -> WidgetT master IO ()
    pageHeadTags res = toWidgetHead $(hamletFile "templates/pagehead.hamlet")
      where
        robotsText :: Maybe Text
        robotsText = case robots res of
            Just (index, follow) -> Just $ (if index then "Index" else "NoIndex")
                                        <> ", "
                                        <> (if follow then "Follow" else "NoFollow")
            Nothing -> Nothing
