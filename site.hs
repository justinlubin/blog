{-# LANGUAGE OverloadedStrings #-}

import Hakyll

import Data.List
import Debug.Trace

import Control.Arrow ((>>>), (<<<))

-- Helpful resource:
--   http://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/index.html

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

(|>) :: a -> (a -> b) -> b
x |> f =
  f x

chopEnding :: Eq a => [a] -> [a] -> [a]
chopEnding ending xs =
  if isSuffixOf ending xs then
    take (length xs - length ending) xs
  else
    xs

--------------------------------------------------------------------------------
-- Hakyll Helpers
--------------------------------------------------------------------------------

removing :: String -> Routes
removing s =
  gsubRoute s (const "")

withoutExtension :: Routes
withoutExtension =
  composeRoutes
    ( setExtension ""
    )
    ( customRoute $ \identifier ->
        case toFilePath identifier of
          "index" ->
            "index.html"

          name ->
            name ++ "/index.html"
    )

urlFieldWithoutIndex :: Context a
urlFieldWithoutIndex =
  mapContext
    (chopEnding "/index.html")
    (urlField "url")

withoutDate :: Routes
withoutDate =
  removing "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-"

--------------------------------------------------------------------------------
-- Posts
--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  constField "post" ""
    <> dateField "date" "%B %e, %Y"
    <> urlFieldWithoutIndex
    <> defaultContext

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "pages/*" $ do
    route (composeRoutes (removing "pages/") withoutExtension)
    compile copyFileCompiler

  match "static/**" $ do
    route (removing "static/")
    compile copyFileCompiler

  match "posts/*" $ do
      route (composeRoutes withoutDate withoutExtension)
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*" >>= recentFirst

      let
        archiveCtx =
          listField "posts" postCtx (return posts)
            <> defaultContext

      getResourceBody
        >>= applyAsTemplate archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "templates/*" $
    compile templateBodyCompiler
