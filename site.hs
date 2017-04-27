--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import           Data.Monoid (mappend)
import           Hakyll


import           System.FilePath (replaceExtension, takeDirectory)
import           Data.List.Split (splitOn)
import           Data.List (intercalate, isInfixOf)
import           System.FilePath.Posix (splitFileName, takeBaseName, 
                                        splitDirectories, takeDirectory)
import           System.Process  (system)
import qualified Text.Pandoc     as Pandoc

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
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    --Shamefully stolen from jaspervdj
    match "posts/*" $ version "pdf" $ do
        route   pdfRoute
        compile $ do getResourceBody
            >>= readPandoc
            >>= (return . fmap writeXeTex)
            >>= loadAndApplyTemplate "templates/post.tex" defaultContext
            >>= xelatex

    create ["archive.html"] $ do
        route   folderRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
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
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
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



writeXeTex = Pandoc.writeLaTeX Pandoc.def {Pandoc.writerTeXLigatures = False}

xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
    TmpFile texPath <- newTmpFile "xelatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- system $ unwords ["xelatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        _ <- system $ unwords ["xelatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath

feeder count renderer = do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take count) . recentFirst =<<
            loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
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

pdfRoute = dateRoute `composeRoutes` customRoute (f . takeDirectory . toFilePath)
    where g = last . splitDirectories 
          f x = (takeDirectory x) ++ "/" ++ g x ++ ".pdf"
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
