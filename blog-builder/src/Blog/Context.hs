{-# LANGUAGE OverloadedStrings #-}

module Blog.Context where

import           Data.Monoid ((<>))
import           Hakyll


postCtx :: Context String
postCtx = modificationTimeField "mtime" "%U"
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

indexContext :: [Item String] -> Context String
indexContext posts = titleField "Home"
    <> listField "posts" postCtx (return posts)
    <> defaultContext
