--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import           Data.Monoid (mappend)
import           Hakyll

import Data.List.Split (splitOn)
import Data.List (intercalate, isInfixOf)
import System.FilePath.Posix (splitFileName, takeBaseName)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   folderRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route   dateRoute 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    create ["archive.html"] $ do
        route   folderRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    create ["atom.xml"] $ feeder 10 renderAtom 
    create ["rss.xml"]  $ feeder 10 renderRss

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateBodyCompiler


feeder count renderer = do 
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take count) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderer myFeedConfiguration feedCtx posts

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Hacka löken"
    , feedDescription = "Posts about programming, paradigms and IT-security"
    , feedAuthorName  = "Emil Gedda"
    , feedAuthorEmail = "emil.gedda@emilgedda.se"
    , feedRoot        = "http://blog.emilgedda.se"
    }

folderRoute = customRoute $ (++"/index.html") . takeBaseName . toFilePath

dateRoute = customRoute $ j . splitOn "-" . toFilePath
    where j x = (intercalate "/"$ take 3 x) 
                ++ '/':(takeWhile (/='.'). intercalate "-" $ drop 3 x)
                ++ "/index.html"

removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url
        where isLocal uri = not (isInfixOf "://" uri)

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
