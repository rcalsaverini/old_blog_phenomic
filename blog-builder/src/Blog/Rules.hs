{-# LANGUAGE OverloadedStrings #-}


module Blog.Rules where

import           Data.Monoid     ((<>))
import           Hakyll
import           Blog.Context
import           Blog.Compilers

postRules :: Rules ()
postRules = match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompilerToc
        >>= saveSnapshot "content"
        >>= return . fmap demoteHeaders
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= relativizeUrls


staticFilesRules :: Rules ()
staticFilesRules = do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    match ("images/*" .||. "codes/*") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compile templates
    match "templates/*" $ compile templateBodyCompiler


indexRules :: Rules ()
indexRules = match "index.html" $ do
    route idRoute
    compile $ do
        posts <- fmap (take 20) . recentFirst =<< loadAll "posts/*"
        let idxContext = indexContext posts
        getResourceBody
            >>= applyAsTemplate idxContext
            >>= loadAndApplyTemplate "templates/default.html" idxContext
            >>= relativizeUrls
