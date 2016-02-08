--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                 (mappend)
import           Hakyll
import           Data.List                   (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Blaze.Html             (toHtml,toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
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

    photos <- buildPhotos "posts/*" (fromCapture "photos/*.html")
    tags   <- buildTags "posts/*"   (fromCapture "tags/*.html")

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

    tagsRules photos $ \tag pattern -> do
        let title = tag
        route idRoute
        compile $ do
            let ctx = constField "title" title
                    `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/photo.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithPhotosAndTags photos tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithPhotosAndTags photos tags)
            >>= relativizeUrls

    match "404.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithPhotosAndTags photos tags)
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

--Add Tags and Photos to Post Metadata and make accessible with keys
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

postCtxWithPhotosAndTags :: Tags -> Tags -> Context String
postCtxWithPhotosAndTags photos tags = photosField "photos" photos
                            `mappend` postCtxWithTags tags

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

--Modified "Tag" functions to look for and build photo metadata and keys
getPhotos :: MonadMetadata m => Identifier -> m [String]
getPhotos identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ M.lookup "photos" metadata

buildPhotos :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildPhotos = buildTagsWith getPhotos

photosField :: String -> Tags -> Context a
photosField = tagsFieldWith getPhotos simpleRenderLink (mconcat . intersperse ", ")

simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _ Nothing           = Nothing
simpleRenderLink tag (Just filePath) =
    Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag
