-- Interactive Colour Explosion 
-- ==========================================

--This program will create a space where you can explode the 'colour' by your control. It is mainly driven by OpenGL and Particle system and it's controled by your keyboard.


--Here are the instructions

--How to Play
--  Key            Function
--  (Char 'q' ) -> Create a coloured source (start erupting)
--  (Char 'e' ) -> Cease the source (stop erupting) 
--  (SpecialKey KeyLeft ) -> Move source to the left
--  (SpecialKey KeyRight) -> Move source to the right
--  (SpecialKey KeyUp   ) -> Move source upwards
--  (SpecialKey KeyDown ) -> Move source downwards
--  (Char 'a' ) -> Move canvas to the left
--  (Char 'd' ) -> Move canvas to the right
--  (Char 'w' ) -> Move canvas upwards
--  (Char 's' ) -> Move canvas downwards
--  (Char '+' ) -> Increasing the density of source (by decrease the Z velocity)
--  (Char '-' ) -> Decreasing the density of source (by increase the Z velocity)

--If you feel a little bored with the Color Point (sprite.png), why not try changing the texname to "funny.bmp"? :) Or you can choose any texture you want.




--Library imported

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

--import texture file here 

--texname = "funny.bmp"
texname = "sprite.png"

--Window Size
winSize = Size 1200 1200

--Particle system
type ParticleList = [Particle]
data Particle = Particle {                             
                                                        particleHue :: GLfloat,
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

--Gravity in y direction but can be similarly controled by 'w' 's'
gravity = 0.00 :: GLfloat


main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Colour Explosion"
  posi <- newIORef (0, 0, 0) -- initial position of pen 
  gra <- newIORef (0, 0) -- initial gravity
  pen <- newIORef 1 -- initial pen situation (1 grab a pen; 0 grab nothing)
  plist <- newIORef ([] :: ParticleList)
  currcol <- newIORef (0.0 :: GLfloat) -- Sweeping hue change
  angle <- newIORef 1
  windowSize $= winSize
  displayCallback $= (display angle plist)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse pen gra posi)
  idleCallback $= Just (idle pen gra posi plist currcol)
  spritetex <- loadTex
  mainLoop

--keyboard control  
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

--reshapcallback
reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing

--load texture of particles and draw particles
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


--display function

display :: IORef GLfloat ->  IORef ParticleList -> IO ()
display angle  plist = do
  clear [ ColorBuffer ]
  particles <- get plist
  mapM_ (renderPrimitive Quads . drawParticle) particles
  flush
  postRedisplay Nothing -- Force GLUT to refresh screen


--This function refreshes the state of particles and update their parameters and positions. Controls are also added here.
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

-- update the particles which have arleady created before
updateParticle ::  (Float, Float)-> Particle -> Particle
updateParticle (a,b) p =
				p { particleVely = particleVely p - gravity ,
					particlePosx = particlePosx p + particleVelx p +a,
					particlePosy = particlePosy p + particleVely p +b,
					particlePosz = particlePosz p + particleVelz p,
					particleTTL = particleTTL p - 1,
					particleSat = particleSat p * 1.1,
					particleVal = particleVal p - 0.01,
					particleSize = particleSize p * 1.005}

-- create new particles and the initial parameters are recorded here
newParticle :: IORef (GLfloat, GLfloat, GLfloat) ->  IORef GLfloat -> IO Particle
newParticle posi huebaseRef = do
			huebase <- get huebaseRef
                        (x', y',z') <- get posi
			hue <- randomRIO (huebase,huebase + 200)
			sat <- randomRIO (0.0001, 0.1 :: GLfloat)
			psize <- randomRIO (0.01, 0.05) 
			pvx <- randomRIO (-0.01, 0.01)
			pvy <- randomRIO (-0.01, 0.01)
			pvz <- randomRIO (0.0, 0.0) 
			ptime <- randomRIO (200, 600) --longevity of the particles
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
--colourized the particles and enable colour changing
getGlColour :: GLfloat -> GLfloat -> GLfloat  -> Color3 GLfloat
getGlColour h s v = Color3 (channelRed rgbcol) (channelGreen rgbcol) (channelBlue rgbcol)
			where -- Pull out RGBs from hsv and opengl-ize them
			rgbcol = hsv h s v

colmod deg
		| deg > 360 = colmod (deg - 360)
		| otherwise = deg

