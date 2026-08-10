{-# LANGUAGE TemplateHaskell #-}
module Main where

badSplice = error "intentional provisional splice failure"

main = $(badSplice)
