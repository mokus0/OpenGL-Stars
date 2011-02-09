#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
module Main where

import Stars

import Data.IORef
import Data.Random
import Foreign
import Graphics.UI.GLUT
import System.Random.Mersenne.Pure64

main = do
    initialize "test" []
    createWindow "foo"
    
    let n       = 1000000
        density = 0.375 -- stars per sq unit
        size    = 0.35
    starData    <- randomStars n density size
    putStrLn "generated stars"
    env         <- initGraphics n
    state       <- initDrawStarsState
    
    displayCallback $= do
        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer]
        drawStars env state starData
        flush
    
    reshapeCallback $= Just (\sz -> setWindowSize state sz >> postRedisplay Nothing)
    
    if n < 1000000  then idleCallback $= Just (postRedisplay Nothing)
                    else idleCallback $= Nothing
    
    mainLoop

randomStars n density starScale = do
    let vol = fromIntegral n / density
        edge = vol ** recip 3
        u = edge / 2
    
    mt <- newPureMT
    src <- newIORef mt
    
    starPositionData <- mallocArray n
    sequence_
        [ do
            x <- sampleFrom src (Data.Random.uniform (-u) u :: RVar Float)
            y <- sampleFrom src (Data.Random.uniform (-u) u :: RVar Float) 
            z <- sampleFrom src (Data.Random.uniform (-u) u :: RVar Float) 
            pokeElemOff (castPtr starPositionData) i (Vector4 x y z 1)
        | i <- [0..n-1]
        ]
    
    let starColorsData = Nothing
        haloDensity = starScale
    return StarData{..}