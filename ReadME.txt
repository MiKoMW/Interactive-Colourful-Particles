=============================================================================
                  Interactive Colourful Particles
   Main Authors : Songbo HU (s1647079) Adam LI (s1603732)
=============================================================================

>>>Brief Introduction

We have built a colourful particle system with an interactive control system based on OpenGL. This project includes five programs: Colour Spring, Colour Spinning, Colour Comet, Colour Explosion and Start Your Own Painting. In addition, there is a funny surprise which is the initial inspiration of our project.

In order to understand codes better, there are comments and explanations in file "Colour Explosion". 

In addition, there are also examples in gif form to perform the codes.

>>>How to Start

In order to run these programs, several packages are required previously for a DICE machine. 
The OpenGL package
The GLUT package
The GLUtil package

In order to install these packages, just simply run these commands with terminal:

cabal update 
cabal install OpenGL
cabal install GLUT
cabal install GLUtil

Before you running a program, it is firstly required to open the image(texture) files (e.g. sprite.png, funny.bmp or any file you choose) in order to help haskell figure it out.

>>>How to Play

>For the two non-interactive programs:

Colour Spinning and Colour Spring, just easily type main to see the animations.

>For the interactive programs:

Colour Comet and Colour Explosion

Here are the instructions of Controls

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

If you feel a little bored with the Color Point (sprite.png), why not try changing the texname to "funny.bmp"? :) Or you can choose any texture you want.

>After that, you can also try drawing a colourful picture by yourself.

Open the folder: "Start your own painting". Then, open "main.hs" and type main to run.

For the controls, they are similar with previous one.

--How to Play
--  Key            Function
--  (Char 'q' ) -> Grab a coloured pen (start painting)
--  (Char 'e' ) -> Throw the pen (stop painting) 
--  (SpecialKey KeyLeft ) -> Move pen to the left
--  (SpecialKey KeyRight) -> Move pen to the right
--  (SpecialKey KeyUp   ) -> Move pen upwards
--  (SpecialKey KeyDown ) -> Move pen downwards
--  (Char 'a' ) -> Move canvas to the left
--  (Char 'd' ) -> Move canvas to the right
--  (Char 'w' ) -> Move canvas upwards
--  (Char 's' ) -> Move canvas downwards

Actually, this program runs in 3D. However we don't offer the 3D movement due to the inconvenience of controls.

>>>Special Thanks

Special thanks to Xiaobin (s1581377), who helps us solve technical problems with DICE, HASKELL and massage XD, which considerably saved our time.

Thanks to Professor Mikael Johanssons who provided the OpenGL tutorials for Haskell which helped us learn the fundamental OpenGL.

Thanks to Vikrem who presented a very good tutorial for particle systems.

Thanks to Chenran whose animation inspired me to start this project.


>>>Other Notes

Note that there is a funny surprise prepared :). 

For further questions, please contact us.

