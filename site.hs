{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Debug.Trace

import Control.Arrow ((>>>), (<<<))

import qualified Data.List
import qualified Control.Monad
import qualified Text.Pandoc.Shared

-- Helpful resource:
--   http://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/index.html

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

(|>) :: a -> (a -> b) -> b
x |> f =
  f x

directory :: FilePath -> FilePath
directory =
  Data.List.dropWhileEnd (/= '/') >>> init

--------------------------------------------------------------------------------
-- Hakyll Helpers
--------------------------------------------------------------------------------

removing :: String -> Routes
removing s =
  gsubRoute s (const "")

withoutDate :: Routes
withoutDate =
  removing "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-"

--------------------------------------------------------------------------------
-- Posts
--------------------------------------------------------------------------------

postContext :: Context String
postContext =
  constField "post" ""
    <> dateField "date" "%B %e, %Y"
    <> (mapContext directory (urlField "url"))
    <> defaultContext

shiftHeadings :: String -> String
shiftHeadings =
  Text.Pandoc.Shared.substitute "h5" "h6"
    >>> Text.Pandoc.Shared.substitute "h4" "h5"
    >>> Text.Pandoc.Shared.substitute "h3" "h4"
    >>> Text.Pandoc.Shared.substitute "h2" "h3"
    >>> Text.Pandoc.Shared.substitute "h1" "h2"
    >>> Text.Pandoc.Shared.substitute "<br />" ""

postCompiler :: Compiler (Item String)
postCompiler =
  pandocCompiler
    |> fmap (fmap shiftHeadings)

--------------------------------------------------------------------------------
-- Archive
--------------------------------------------------------------------------------

archiveContext :: Context String
archiveContext =
  listField "posts" postContext (loadAll "posts/*/index.md" >>= recentFirst)
    <> defaultContext

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "static/**" $ do
    route (removing "static/")
    compile copyFileCompiler

  match "posts/*/index.md" $ do
    route (composeRoutes withoutDate (setExtension "html"))
    compile $
      postCompiler
        >>= loadAndApplyTemplate "templates/main.html" postContext

  match ("posts/*/*" .&&. complement "posts/*/index.md") $ do
    route withoutDate
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile $
      getResourceBody
        >>= applyAsTemplate archiveContext
        >>= loadAndApplyTemplate "templates/main.html" archiveContext

  match "templates/*" $
    compile templateBodyCompiler
