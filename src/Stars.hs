{-# LANGUAGE ParallelListComp, RecordWildCards #-}
module Stars
    ( initGraphics, StarData(..), drawStars, nBodyCount, initDrawStarsState, setWindowSize,
    vec4sz 
    ) where

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Data.IORef
import Graphics.Rendering.OpenGL hiding (attachedShaders)
import Graphics.Rendering.OpenGL.GL.Shaders.Geometry
import Foreign
import System.Exit
import System.IO

noOffset :: Ptr a
noOffset = nullPtr

byteOffset :: IntPtr -> Ptr a
byteOffset = intPtrToPtr

elemOffset :: Storable a => IntPtr -> Ptr a
elemOffset n = off
    where
        off = intPtrToPtr (n * fromIntegral sz)
        sz = sizeOf ((undefined :: Ptr a -> a) off)

-- phase 1: transliterate from c++
-- nbody.cpp
starSize = 4
haloSize = 300
data DrawStarsEnv = DrawStarsEnv
    { nBodyCount        :: Int
    , galaxiesShader    :: Program
    , starPositions     :: [BufferObject]
    , starColors        :: BufferObject
    , pointSizeUniform  :: UniformLocation
    , tex               :: TextureObject
    , tex2              :: TextureObject
    }
data DrawStarsState = DrawStarsState
    { rotateX           :: GLfloat
    , rotateY           :: GLfloat
    , rotate            :: Bool
    , rotationSpeed     :: GLfloat
    , viewDistance      :: GLdouble
    , windowSize        :: Size
    , starPositionIx    :: Int
    }
initialDrawStarsState = DrawStarsState
    { rotateX           = 0
    , rotateY           = 0
    , rotate            = True
    , rotationSpeed     = 0
    , viewDistance      = pi
    , windowSize        = Size 320 320
    , starPositionIx    = 0
    }
initDrawStarsState = newIORef initialDrawStarsState
setWindowSize state sz = do
    DrawStarsState{..} <- readIORef state
    writeIORef state DrawStarsState{windowSize = sz, ..}
    

data StarData = StarData
    { starScale         :: GLfloat
    , haloDensity       :: GLfloat
    , starPositionData  :: Ptr (Vector4 GLfloat)
    , starColorsData    :: Maybe (Ptr (Vector4 GLfloat))
    } 
drawStars DrawStarsEnv{..} drawState StarData{..} = do
    DrawStarsState{..} <- readIORef drawState
    let rotAccel = 0.06
        wrap x lo hi = go x
            where                              
                diff = hi - lo
                go x | x < lo    = go (x + diff)
                     | x > hi    = go (x - diff)
                     | otherwise = x
    rotateX <- return $ wrap rotateX (-180) 180
    rotateY <- return $ wrap rotateX (-180) 180
    
    rotationSpeed <- return $ if rotate
        then max pi (rotationSpeed + rotAccel)
        else min 0  (rotationSpeed - rotAccel)
    
    -- if (Resetting) ...
    
    matrixMode $= Projection
    loadIdentity
    let aspect (Size w h) = fromIntegral w / fromIntegral h
    perspective 60 (aspect windowSize) 0.1 10000
    matrixMode $= Modelview 0
    loadIdentity
    
    lookAt (Vertex3 (-viewDistance) 0 0) (Vertex3 0 0 0) (Vector3 0 1 0)
    
    viewport $= (Position 0 0, windowSize)
    
    blendFunc $= (One, One)
    blend $= Enabled
    
    currentProgram $= Just galaxiesShader
    
    activeTexture $= TextureUnit 0
    
    bindBuffer ArrayBuffer $= Just (starPositions !! starPositionIx)
    bufferSubData ArrayBuffer WriteToBuffer 0 (fromIntegral (nBodyCount * vec4sz)) starPositionData
    arrayPointer VertexArray $= VertexArrayDescriptor 4 Float 0 noOffset
    bindBuffer ArrayBuffer $= Nothing
    
    preservingMatrix $ do
        -- if (!Resetting) ...
        
        Graphics.Rendering.OpenGL.rotate rotateY (Vector3 1 0 0)
        Graphics.Rendering.OpenGL.rotate rotateX (Vector3 0 1 0)
        
        uniform pointSizeUniform $= Index1 (starSize * starScale)
        
        textureBinding Texture2D $= Just tex
        
        case starColorsData of
            Nothing -> do
                -- white stars
                color (Color3 0.8 0.8 0.8 :: Color3 GLfloat)
                drawArrays Points 0 (fromIntegral $ nBodyCount `div` 4 * 2)
                
                -- blue stars
                color (Color3 0.7 0.8 1.0 :: Color3 GLfloat)
                drawArrays Points (fromIntegral $ nBodyCount `div` 4 * 2) (fromIntegral $ nBodyCount `div` 4 * 1)
                
                -- red stars
                color (Color3 1.0 0.9 1.9 :: Color3 GLfloat)
                drawArrays Points (fromIntegral $ nBodyCount `div` 4 * 3) (fromIntegral $ nBodyCount `div` 4 * 1)
            Just colorData -> do
                clientState ColorArray $= Enabled
                
                fail "write me!"
                
                clientState ColorArray $= Disabled
        
        -- draw halos
        textureBinding Texture2D $= Just tex2
        
        uniform pointSizeUniform $= Index1 (haloSize * starScale)
        
        -- purple clouds
        color (fmap (*haloDensity) $ Color3 0.016 0.005 0.013 :: Color3 GLfloat)
        drawArrays Points (fromIntegral $ nBodyCount `div` 8 * 1) (fromIntegral $ nBodyCount `div` 16 * 1)
        
        -- blue clouds
        color (fmap (*haloDensity) $ Color3 0.009 0.005 0.016 :: Color3 GLfloat)
        drawArrays Points (fromIntegral $ nBodyCount `div` 8 * 5) (fromIntegral $ nBodyCount `div` 16 * 1)
        
    
    color (Color3 1 1 1 :: Color3 GLfloat)
    
    currentProgram $= Nothing
    textureBinding Texture2D $= Nothing
    
    writeIORef drawState DrawStarsState{..}

vec4sz = sizeOf (Vector4 0 0 0 0 :: Vector4 GLfloat)
initGraphics nBodyCount = do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    
    clientState VertexArray $= Enabled
    
    vbufs@[vertexBuffer0, vertexBuffer1, colorBuffer] <- genObjectNames 3
    sequence_
        [ do
            bindBuffer ArrayBuffer $= Just vbuf
            let arrayBufSz = nBodyCount * vec4sz
            bufferData ArrayBuffer $= (fromIntegral arrayBufSz, noOffset, DynamicDraw)
            arrayPointer VertexArray $= VertexArrayDescriptor 4 Float 0 noOffset
            bindBuffer ArrayBuffer $= Nothing
        | vbuf <- vbufs
        ]
    
    tex <- loadTexture "star.png" True
    
    [tex2] <- genObjectNames 1
    do
        let texRes = 32
        texData <- createGaussianMap (fromIntegral texRes)
        textureBinding Texture2D $= Just tex2
        generateMipmap Texture2D $= Enabled
        textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
        texImage2D Nothing NoProxy 0 Luminance8 (TextureSize2D texRes texRes) 0 (PixelData Luminance UnsignedByte texData)
        
        free texData
    
    textureBinding Texture2D $= Nothing
    
    galaxiesShader <- loadShader "nbody.vsh" "nbody.fsh" "nbody.gsh" InPoints OutTriangleStrip 4
    
    currentProgram $= Just galaxiesShader
    texLoc <- get (uniformLocation galaxiesShader "splatTexture")
    uniform texLoc $= Index1 (0 :: GLint)
    currentProgram $= Nothing
    
    let sync = 1
    -- CGLSetParameter(CGLGetCurrentContext(), kCGLCPSwapInterval, &sync);
    
    let starPositions   = [vertexBuffer0, vertexBuffer1]
        starColors      = colorBuffer
    pointSizeUniform <- get (uniformLocation galaxiesShader "pointSize")
    return DrawStarsEnv{..} {- everything, in a record type -}

-- geometry.cpp
loadTexture path mipmap = do
    mbImage <- loadImage path
    case mbImage of
        Left err -> do
            hPutStrLn stderr ("loadTexture: " ++ err)
            exitWith (ExitFailure 3)
        Right image -> do
            texture <- makeSimpleBitmapTexture image
            
            textureBinding Texture2D $= Just texture
            
            if mipmap
                then do
                    generateMipmap Texture2D $= Enabled
                    textureFilter Texture2D $= ((Linear', Nothing), Linear')
--                    textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
                else do
                    textureFilter Texture2D $= ((Linear', Nothing), Linear')
            
            textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
            textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
            
            textureBinding Texture2D $= Nothing
            return texture

createShader fileName = do
    src <- readFile fileName
    
    [shader] <- genObjectNames 1
    shaderSource shader $= [src]
    compileShader shader
    
    compiled <- get (compileStatus shader)
    if compiled
        then return shader
        else do
            log <- get (shaderInfoLog shader)
            hPutStrLn stderr ("shader compilation failed for " ++ fileName)
            hPutStrLn stderr log
            exitWith (ExitFailure 1)

-- no native geometry shader support in OpenGL-2.4.0.1, so we use a simple
-- extension i built (unfortunately it can't add 'instance Shader GeometryShader'
-- because the implementation of the Shader class is totally hidden).  So,
-- this function is essentially a duplicate of createShader but with
-- "names changed to protect the innocent".
createGeomShader fileName = do
    src <- readFile fileName
    
    [shader] <- genObjectNames 1
    geometryShaderSource shader $= [src]
    compileGeometryShader shader
    
    compiled <- get (geometryShaderCompileStatus shader)
    if compiled
        then return shader
        else do
            log <- get (geometryShaderInfoLog shader)
            hPutStrLn stderr ("shader compilation failed for " ++ fileName)
            hPutStrLn stderr log
            exitWith (ExitFailure 1)

loadShader vsFileName fsFileName gsFileName inType outType verticesOut = do
    vertexShader   <- createShader      vsFileName
    geometryShader <- createGeomShader  gsFileName
    fragmentShader <- createShader      fsFileName
    
    [program] <- genObjectNames 1
    
    attachedShaders program $= ([vertexShader], [fragmentShader], [geometryShader])
    
    geometryInputType   program $= inType
    geometryOutputType  program $= outType
    geometryVerticesOut program $= verticesOut
    
    linkProgram program
    
    linked <- get (linkStatus program)
    if linked
        then return program
        else do
            log <- get (programInfoLog program)
            hPutStrLn stderr ("program link failed for shaders: " ++ show [vsFileName, fsFileName, gsFileName])
            hPutStrLn stderr log
            exitWith (ExitFailure 2)

createGaussianMap :: Int -> IO (Ptr GLubyte)
createGaussianMap n = do
    b <- mallocArray (n^2)
    
    let incr = 2 / fromIntegral n
    sequence_
        [ do
            let yy = fromIntegral y * incr - 1
                xx = fromIntegral x * incr - 1
                dist2 = min 1 (xx^2 + yy^2)
                dist3 = dist2 * sqrt dist2
                
                m = 2 * dist3 - 3 * dist2 + 1
                
            pokeElemOff b j (truncate (m * 255))
        | y <- [0..n-1], x <- [0..n-1]
        | j <- [0..]
        ]
    
    return b
