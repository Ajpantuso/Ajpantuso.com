--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Pandoc.Options
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "assets/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "assets/images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "assets/css/images/overlay.png" $ do
        route   idRoute
        compile copyFileCompiler

    match "downloads/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "CNAME" $ do
       route idRoute
       compile copyFileCompiler

    match "resume.html" $ do
        route idRoute
        compile copyFileCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = tag
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let ctx = constField "title" title
                    `mappend` listField "posts" teaserCtx (return posts)
                    `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "posts.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postsCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Posts"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "AJPantuso"
    , feedDescription = "Applied Mathematics and Computer Science"
    , feedAuthorName = "Andrew Pantuso"
    , feedAuthorEmail = "ajpantuso@gmail.com"
    , feedRoot = "http://ajpantuso.com"
}

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

teaserCtx = teaserField "teaser" "content" `mappend` postCtx

-- listContextWith :: Context String -> String -> Context a
-- listContextWith ctx s = listField s ctx $ do
--     identifier <- getUnderlying
--     metadata <- getMetadata identifier
--     let metas = maybe [] (map trim . splitAll ",") $ M.lookup s metadata
--     return $ map (\x -> Item (fromFilePath x) x) metas
--
-- listContext :: String -> Context a
-- listContext = listContextWith defaultContext
