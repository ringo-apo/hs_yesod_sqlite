{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Hello where

import Import

getHelloR :: Handler Html
-- getHelloR = error "Not yet implemented: getHelloR"
getHelloR = defaultLayout $(widgetFile "hello")