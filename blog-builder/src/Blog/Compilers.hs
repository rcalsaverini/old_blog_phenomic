{-# LANGUAGE OverloadedStrings #-}

module Blog.Compilers where

import qualified Text.Pandoc as Pandoc
import           Hakyll


pandocCompilerToc :: Compiler (Item String)
pandocCompilerToc = pandocCompilerWith defaultHakyllReaderOptions $
    defaultHakyllWriterOptions {
        Pandoc.writerTableOfContents = False,
        Pandoc.writerTemplate = "$toc$\n$body$",
        Pandoc.writerStandalone = True
    }
