{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ArticleList where

import Import

getArticleListR :: Handler Html
getArticleListR = do
    articles <- runDB $ selectList [] [Desc ArticlePublished]
    defaultLayout $(widgetFile "articleList")
