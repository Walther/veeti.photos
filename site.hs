--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath
import           Data.List (isSuffixOf)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "blog/*.md" $ do
        route $ cleanRoute
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    match (fromList ["gallery.md"]) $ do
        route $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx    
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls
            
            
    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "blog/*.md" "content"
            let archiveCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Blog"                  `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 2) . recentFirst =<< loadAllSnapshots "blog/*.md" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "veeti.photos"          `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- http://reichertbrothers.com/blog/posts/2014-04-08-hakyll-teasers.html
-- https://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html
teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

-- https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                             where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"