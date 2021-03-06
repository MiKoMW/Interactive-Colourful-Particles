import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.GLUtil
import Graphics.GLUtil.JuicyTextures

import Data.Colour.SRGB.Linear hiding (blend)
import Data.Colour hiding (blend)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Data.IORef

import System.Random
import System.Exit

texname = "funny.bmp"
winSize = Size 800 800

type ParticleList = [Particle]
data Particle = Particle { particleHue :: GLfloat,
							particleSat :: GLfloat,
							particleVal :: GLfloat,
							particleSize :: GLfloat,
							particlePosx :: GLfloat,
							particlePosy :: GLfloat,
							particlePosz :: GLfloat,
							particleVelx :: GLfloat,
							particleVely :: GLfloat,
							particleVelz :: GLfloat,
							particleTTL :: Int} deriving (Eq, Show, Read)

gravity = 0.00 :: GLfloat

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Funny??"
  plist <- newIORef ([] :: ParticleList)
  currcol <- newIORef (0.0 :: GLfloat) 
  windowSize $= winSize
  displayCallback $= (display plist)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  idleCallback $= Just (idle plist currcol)
  spritetex <- loadTex
  mainLoop

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing

keyboardMouse (Char '\ESC') Down _ _ = exitSuccess
keyboardMouse key state mods pos = putStrLn $ show key 

extract :: (IO (Either String a)) -> IO a
extract act = do
				e <- act
				case e of
					Left err -> error err
					Right val -> return val

drawParticle :: Particle -> IO ()
drawParticle p = do
				texCoord $ TexCoord2 z z
				vertex $ Vertex3    (particlePosx p - particleSize p) (particlePosy p - particleSize p) (particlePosz p)
				texCoord $ TexCoord2 z o
				vertex $ Vertex3  (particlePosx p - particleSize p) (particlePosy p + particleSize p) (particlePosz p)
				texCoord $ TexCoord2 o o
				vertex $ Vertex3  (particlePosx p + particleSize p) (particlePosy p + particleSize p) (particlePosz p)
				texCoord $ TexCoord2 o z
				vertex $ Vertex3 (particlePosx p + particleSize p) (particlePosy p - particleSize p) (particlePosz p)
				where
				o = 0 :: GLfloat
				z = 1 :: GLfloat

loadTex = do
		imgresult <- readTexture texname
		finaltexture <- extract $ readTexInfo texname loadTexture
		texture Texture2D $= Enabled
		activeTexture $= TextureUnit 0
		textureBinding Texture2D $= Just finaltexture
		textureFilter   Texture2D   $= ((Linear', Just Nearest), Linear')
		textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
		textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
		blend $= Enabled
		blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
		generateMipmap' Texture2D
		return finaltexture

display :: IORef ParticleList -> IO ()
display plist = do
  clear [ ColorBuffer ]
  particles <- get plist
  mapM_ (renderPrimitive Quads . drawParticle) particles
  flush
  postRedisplay Nothing 

idle :: IORef ParticleList -> IORef GLfloat -> IO ()
idle plist currcol = do
			particles <- get plist
			let updateparticles = map updateParticle particles
			newparticles <- sequence (replicate 1 (newParticle currcol))
			plist $= filter (\p -> particleTTL p >= 0)  (newparticles ++ updateparticles)
	
updateParticle :: Particle -> Particle
updateParticle p =
				p { particleVely = particleVely p - gravity,
					particlePosx = particlePosx p + particleVelx p,
					particlePosy = particlePosy p + particleVely p,
					particlePosz = particlePosz p + particleVelz p,
					particleTTL = particleTTL p - 1,
					particleSat = particleSat p * 1.1,
					particleVal = particleVal p - 0.01,
					particleSize = particleSize p * 1.01}

newParticle :: IORef GLfloat -> IO Particle
newParticle huebaseRef = do
                        
                        pozang <-  randomRIO (-pi, pi)
                        pozlen <-  randomRIO (-0.8, 0.8)
			sat <- randomRIO (0.0001, 0.1 :: GLfloat)
			psize <- randomRIO (0.04, 0.04) 
			pvz <- return 0.02
			ptime <- randomRIO (2000, 2000)
			return $ Particle { particlePosx = pozlen * (sin pozang),
							particlePosy = pozlen * (cos pozang),
							particlePosz = -1 ,
							particleHue = 0,
							particleSat = sat,
							particleVal = 1,
							particleSize = psize,
							particleVelx = if pozlen < 0.1 then  pozlen * (sin pozang)*0.06 else pozlen * (sin pozang)*0.06,
							particleVely = if pozlen < 0.1 then  pozlen * (cos pozang)*0.06 else pozlen * (cos pozang)*0.06,
							particleVelz = pvz,
							particleTTL =  ptime}



colmod deg
		| deg > 360 = colmod (deg - 360)
		| otherwise = deg

