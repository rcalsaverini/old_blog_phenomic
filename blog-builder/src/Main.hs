{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Blog.Rules
import           Hakyll
import Data.Monoid ((<>))

config :: Configuration
config = defaultConfiguration {deployCommand = "mv _site/* ..; git commit; git push"}

main :: IO ()
main = hakyllWith config $ do
    staticFilesRules
    postRules
    indexRules
