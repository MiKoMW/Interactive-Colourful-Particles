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

texname = "sprite.png"
winSize = Size 1200 1200

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
  createWindow "Colour Comet"
  posi <- newIORef (0, 0, 0)
  gra <- newIORef (0, 0)
  pen <- newIORef 1
  plist <- newIORef ([] :: ParticleList)
  currcol <- newIORef (0.0 :: GLfloat) 
  angle <- newIORef 1
  windowSize $= winSize
  displayCallback $= (display angle plist)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse pen gra posi)
  idleCallback $= Just (idle pen gra posi plist currcol)
  spritetex <- loadTex
  mainLoop

 
keyboardMouse ::  IORef GLfloat -> IORef (GLfloat, GLfloat)-> IORef (GLfloat, GLfloat,GLfloat) -> KeyboardMouseCallback
keyboardMouse q o p key Down _ _ = case key of
  (Char 'e' ) -> q $~! \(c) -> (c + 1)
  (Char 'q' ) -> q $~! \(c) -> (c * 0)
  (Char 'w' ) -> o $~! \(a,b) -> (a,b+0.001)
  (Char 's' ) -> o $~! \(a,b) -> (a,b-0.001)
  (Char 'a' ) -> o $~! \(a,b) -> (a-0.001,b)
  (Char 'd' ) -> o $~! \(a,b) -> (a+0.001,b)
  (Char '+' ) -> p $~! \(x,y,z) -> (x,y,z+0.01)
  (Char '-' ) -> p $~! \(x,y,z) -> (x,y,z-0.01)
  (SpecialKey KeyLeft ) -> p $~! \(x,y,z) -> (x-0.05,y,z)
  (SpecialKey KeyRight) -> p $~! \(x,y,z) -> (x+0.05,y,z)
  (SpecialKey KeyUp   ) -> p $~! \(x,y,z) -> (x,y+0.05,z)
  (SpecialKey KeyDown ) -> p $~! \(x,y,z) -> (x,y-0.05,z)
  _ -> return ()
keyboardMouse  _ _ _ _ _ _ _  = return ()

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing


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

display :: IORef GLfloat ->  IORef ParticleList -> IO ()
display angle  plist = do
  clear [ ColorBuffer ]
  particles <- get plist
  mapM_ (renderPrimitive Quads . drawParticle) particles
  flush
  postRedisplay Nothing 

idle ::   IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef (GLfloat, GLfloat, GLfloat) ->  IORef ParticleList -> IORef GLfloat -> IO ()
idle pen gra posi plist currcol = do
			particles <- get plist
                        (x' , y' , z') <- get posi
                        (a', b') <- get gra
                        (c') <- get pen

			let updateparticles = map (updateParticle (a',b')) particles
			newparticles <- sequence (replicate (if c'==0 then 25 else 0) (newParticle posi currcol))
			plist $= filter (\p -> particleTTL p >= 0)  (newparticles ++ updateparticles)
			huebase <- get currcol
			currcol $= colmod (huebase + 0.2)
			putStrLn $ show $ huebase

updateParticle ::  (Float, Float)-> Particle -> Particle
updateParticle (a,b) p =
				p { particleVely = particleVely p - gravity +b,
                                    particleVelx = particleVelx p + a,
					particlePosx = particlePosx p + particleVelx p ,
					particlePosy = particlePosy p + particleVely p ,
					particlePosz = particlePosz p + particleVelz p,
					particleTTL = particleTTL p - 1,
					particleSat = particleSat p * 1.1,
					particleVal = particleVal p - 0.01,
					particleSize = particleSize p * 1.02}


newParticle :: IORef (GLfloat, GLfloat, GLfloat) ->  IORef GLfloat -> IO Particle
newParticle posi huebaseRef = do
			huebase <- get huebaseRef
                        (x', y',z') <- get posi
			hue <- randomRIO (huebase,huebase + 200)
			sat <- randomRIO (0.0001, 0.1 :: GLfloat)
			psize <- randomRIO (0.01, 0.05) 
			pvx <- randomRIO (-0.01, 0.01)
			pvy <- randomRIO (-0.01, 0.01)
			pvz <- randomRIO (0.04, 0.04) 
			ptime <- randomRIO (5, 100)
			return $ Particle { particlePosx = x',
							particlePosy = y',
							particlePosz = 0,
							particleHue = colmod hue,
							particleSat = sat,
							particleVal = 1,
							particleSize = psize,
							particleVelx = pvx,
							particleVely = pvy,
							particleVelz = pvz + z',
							particleTTL = ptime}

getGlColour :: GLfloat -> GLfloat -> GLfloat  -> Color3 GLfloat
getGlColour h s v = Color3 (channelRed rgbcol) (channelGreen rgbcol) (channelBlue rgbcol)
			where 
			rgbcol = hsv h s v

colmod deg
		| deg > 360 = colmod (deg - 360)
		| otherwise = deg

