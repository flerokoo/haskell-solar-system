module Main where

import Prelude hiding (interact)
import Lib
import Data.List 
import Control.Monad hiding (step)

gc = 6.67 / (10^(11)) * 10/10
moonMass = (7.36*10^22)
earthMass = (5.97*10^24)
moonToEarthDist = (384400*10^3)
moonLinearVelocity = 1023.056
dt = 86.4*2
numiterations = 5*10000

data Vector = Vector Float Float
Vector x1 y1 |+| Vector x2 y2 = Vector (x1+x2) (y1+y2)
Vector x1 y1 |-| Vector x2 y2 = Vector (x1-x2) (y1-y2)
Vector x1 y1 |.| Vector x2 y2 = Vector (x1*x2) (y1*y2)
Vector x1 y1 |/| Vector x2 y2 = Vector (x1*y2) (x2*y1)
n |*| Vector x y = Vector (n*x) (n*y)
magn (Vector x y) = sqrt(x*x+y*y)
dist v1@(Vector x1 y1) v2@(Vector x2 y2) = magn (v1 |-| v2)
dir v1 v2 = let dst = dist v2 v1 in (1/dst) |*| (v2 |-| v1)
norm v = (1 / magn v) |*| v

type Position = Vector
type Velocity = Vector 
type Acceleration = Vector
type Mass = Float
type Name = String
type Time = Float

data SpaceObject = Static Name Mass Position | Dynamic Name Mass Position Velocity
data World = World Time [SpaceObject]

class Steppable a where
  step::Float->a->a

instance Steppable SpaceObject where
  step dt obj = case obj of
    Static _ _ _ -> obj
    Dynamic name mass pos vel -> Dynamic name mass ((|+|) pos . (|*|) dt $ vel) vel

instance Show SpaceObject where
  show (Static n m p) = "Static " ++ n ++ " of mass " ++ show m ++ " and position " ++ show p
  show (Dynamic n m p v) = "Dynamic object " ++ n ++  " of mass " ++ show m 
    ++ ", position " ++ show p ++ " and velocity " ++ show v  

instance Steppable World where
  step dt (World lifetime objs) = case objs of
    [] -> World (lifetime+dt) []
    objs -> World (lifetime+dt) nextobjs
      where 
        nextobjs = map (step dt) [ x `attractedBy` (delete x objs) | x <- objs ]
        x `attractedBy` objs = case x of 
          Dynamic nameX massX posX velX -> Dynamic nameX massX posX newVelX
            where
              newVelX = velX |+| (dt |*| acceleration)
              acceleration = foldl (\prev cur -> prev |+| cur) (Vector 0 0) accelerations
              accelerations = map calcAcceleration $ objs
              calcAcceleration from = 
                let 
                  (massY, posY) = case from of
                    Static _ m p -> (m, p)
                    Dynamic _ m p _ -> (m, p)
                in (gc * massY / (dist posY posX)^2) |*| (dir posX posY)
          static -> static

      
instance Show World where 
  show (World _ []) = "Empty world"
  show (World t xs) = "World exists for " ++ show t ++  " seconds:\n" ++ (intercalate "\n" 
    . zipWith (\n s -> show n ++ " -- " ++ show s) [1..] $ xs)

instance Show Vector where
  show (Vector x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Vector where
  Vector x1 y1 == Vector x2 y2 = (x1==x2)&&(y1==y2)

instance Eq SpaceObject where
  Static n1 _ _ == Static n2 _ _ = (n1==n2)
  Static {} == Dynamic {} = False
  d@(Dynamic {}) == s@(Static {}) = s == d
  Dynamic n1 _ _ _  == Dynamic n2 _ _ _  = (n1==n2)



moon = Dynamic "Moon" moonMass (Vector moonToEarthDist 0) (Vector -moonLinearVelocity*0.3) moonLinearVelocity) 
earth = Static "Earth" earthMass (Vector 0 0)
unknown = Static "Unkown celestial object" (earthMass) (Vector (1.2*moonToEarthDist) (1.2*moonToEarthDist))
wrld = World 0 [moon, earth, unknown]

-- main = putStrLn . intercalate "\n\n" . map show . take 3 . iterate (step dt) $ wrld
main:: IO ()
main = simulation wrld numiterations dt

simulation :: (Num a, Ord a) => World -> a -> Float -> IO ()
simulation world iterations dt = do
  let 
    getName o = case o of
      Static n _ _ -> n
      Dynamic n _ _ _ -> n
    getByName name (World _ objs) = find (\o -> (getName o) == name) objs
    getPositionOf name world = maybe
      Nothing
      (\o -> case o of
        Static _ _ p -> Just p
        Dynamic _ _ p _ -> Just p)
      (getByName name world)
    simulate world iter = do
      let 
        newworld = step dt world      
        logPositionOf name world = case (getPositionOf name world) of
          Nothing -> putStrLn ("No object named " ++ name)
          Just (Vector x y) -> putStrLn (show x ++ ", " ++ show y)
      logPositionOf "Moon" newworld
      when (iter < iterations) (simulate newworld (iter+1))     
  simulate world 0

