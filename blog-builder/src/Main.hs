{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Blog.Rules
import           Hakyll
import Data.Monoid ((<>))

config :: Configuration
config = defaultConfiguration {deployCommand = "cp _site/* .. -R; git commit; git push"}

main :: IO ()
main = hakyllWith config $ do
    staticFilesRules
    postRules
    indexRules
