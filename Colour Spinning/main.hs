import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GLU.Matrix
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
texname = "sprite.png"
winSize = Size 800 800

type ParticleList = [Particle]
data Particle = Particle { particleHue :: GLfloat,
							particleSat :: GLfloat,
							particleVal :: GLfloat,
							particleSize :: GLfloat,
							particlePosx :: GLfloat,
							particlePosy :: GLfloat,
							particlePosz :: GLfloat,
                                                        particlePosa :: GLfloat,
                                                        particlePosl :: GLfloat,
							particleVelx :: GLfloat,
							particleVely :: GLfloat,
							particleVelz :: GLfloat,
							particleVelc :: GLfloat,
                                                        particleVela :: GLfloat,
							particleTTL :: Int} deriving (Eq, Show, Read)

gravityy = 0.00 :: GLfloat
gravityx = 0.00 :: GLfloat
gravityz = 0.0  :: GLfloat
gravityc = 0.02 :: GLfloat 
gravitya = 0.01 :: GLfloat

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Colour Spinning"
  plist <- newIORef ([] :: ParticleList)
  currcol <- newIORef (0.1 :: GLfloat)
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
				color $ getGlColour (particleHue p) (particleSat p) (particleVal p)
				texCoord $ TexCoord2 z z
				vertex $ Vertex3 (particlePosx p - particleSize p) (particlePosy p - particleSize p) (particlePosz p)
				texCoord $ TexCoord2 z o
				vertex $ Vertex3 (particlePosx p - particleSize p) (particlePosy p + particleSize p) (particlePosz p)
				texCoord $ TexCoord2 o o
				vertex $ Vertex3 (particlePosx p + particleSize p) (particlePosy p + particleSize p) (particlePosz p)
				texCoord $ TexCoord2 o z
				vertex $ Vertex3 (particlePosx p + particleSize p) (particlePosy p - particleSize p) (particlePosz p)
				where
				o = 1 :: GLfloat
				z = 0 :: GLfloat

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

display ::IORef ParticleList -> IO ()
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
			newparticles <- sequence (replicate 25 (newParticle currcol))
			plist $= filter (\p -> particleTTL p >= 0)  (newparticles ++ updateparticles)
			huebase <- get currcol
			currcol $= colmod (huebase + 1.2)
			putStrLn $ show $ huebase

updateParticle :: Particle -> Particle
updateParticle p =
				p {     particleVely = particleVely p - gravityy,
                                        particleVelx = particleVelx p - gravityx,
					particleVelz = particleVelz p - gravityz,
                                        particleVelc = particleVelc p - gravityc,
                                        particleVela = particleVela p - gravitya,
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
                        pozlen <-  randomRIO (0.05, 0.05)
			huebase <- get huebaseRef
			hue <- randomRIO (huebase,huebase + 20)
			sat <- randomRIO (0.0001, 0.1 :: GLfloat)
			psize <- randomRIO (0.01, 0.05) 
			pvxyang <- randomRIO (1, 2)
			pvlen <- randomRIO (0.025, 0.06)
			pvz <- return 0 
			ptime <- randomRIO (50, 100)
			return $ Particle { particlePosx =  pozlen * (sin pozang),
							particlePosy =   pozlen * (cos pozang),
							particlePosz = 0,
							particleHue = colmod hue,
							particleSat = sat,
							particleVal = 1,
							particleSize = psize,
							particleVelc = pvlen,
                                                        particleVela = 0,
                                                        particleVelx = ((cos pozang) *pozlen * pvxyang) + (pvlen * (sin pvxyang)),
							particleVely = (-(sin pozang) * pozlen * pvxyang) + (pvlen * ( cos pvxyang)),
							particleVelz = pvz,
                                                        particlePosa = pozang,
                                                        particlePosl = pozlen,
							particleTTL = ptime}

getGlColour :: GLfloat -> GLfloat -> GLfloat  -> Color3 GLfloat
getGlColour h s v = Color3 (channelRed rgbcol) (channelGreen rgbcol) (channelBlue rgbcol)
			where 
			rgbcol = hsv h s v

colmod deg
		| deg > 360 = colmod (deg - 360)
		| otherwise = deg

