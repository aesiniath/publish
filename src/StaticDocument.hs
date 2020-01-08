{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module StaticDocument
    ( program
    )
where

import Core.Program
import Core.System
import Core.Text

import Environment (Env(..))

program :: Program Env ()
program = undefined