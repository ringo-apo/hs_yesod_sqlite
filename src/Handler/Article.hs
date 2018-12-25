{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Handler.Article where

import Import

import           Control.Monad.Logger
import           Data.Text               (Text)
import           Database.Persist.Sqlite
import           Yesod

import           Control.Monad.Logger
import           Data.Text               (Text)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Persist.Sqlite
import           Yesod
import qualified Data.Conduit.List as CL
import Data.Conduit (($=))


-- フォーム取得
articleForm :: Maybe Article -> Html -> MForm Handler (FormResult Article, Widget)
articleForm article extra = do
  (titleResult, titleView)            <- mreq textField "タイトル" (articleTitle <$> article)
  (publishedResult, publishedView)    <- mreq dayField "公開日" (articlePublished <$> article)
  (viewCountResult, viewCountView)    <- mopt intField "ビュー数" (articleViewCount <$> article)
  let result = Article
      <$> titleResult
      <*> publishedResult
      <*> viewCountResult
    widget = $(widgetFile "article-editor-form")
  return (result, widget)

-- 新規登録
getArticleR :: Handler Html
getArticleR = do
  let
    header = "Article新規登録" :: Text
  (widget, enctype) <- generateFormPost $ articleForm Nothing
  defaultLayout $(widgetFile "article")

-- 新規登録処理のポスト
postArticleR :: Handler Html
postArticleR = do
  ((result, widget),enctype) <- runFormPost $ articleForm Nothing let header = "Article新規登録" :: Text case result of FormSuccess article -> do
      -- Postされたデータが正常な場合
      articleId <- runDB $ insert article redirect ArticleListR FormFailure _ -> do
      -- 不正な入力値のデータが送信された場合(必須項目が未入力等)
      setMessage "不正なデータが送信されました。"
      defaultLayout $(widgetFile "article")
    FormMissing -> defaultLayout [whamlet|データが送信されませんでした。 |]
    _ -> undefined