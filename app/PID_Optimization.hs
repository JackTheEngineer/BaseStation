module PID_Optimization where
import Data.Fixed
import System.Random

data Quaternion = Quaternion { q0 :: Float
                             , q1 :: Float
                             , q2 :: Float
                             , q3 :: Float} deriving Show

toQuat :: [Float] -> Quaternion
toQuat f = Quaternion (f !! 0) (f !! 1) (f !! 2) (f !! 3)

errOfQuaternion q = acos(q0 q)

calculateError :: [Quaternion] -> Float -> Float
calculateError qs time =
  let sumerr = foldr1 (+) (map errOfQuaternion qs) in
    sumerr/time

listOfVectorToTest :: Float -> Float -> Float -> IO([(Float, Float)])
listOfVectorToTest pParam dParam stepsize = do
  angle <- getStdRandom (randomR (0.0, 2*pi)) :: IO(Float)
  let angles = angle : [(angle + pi*(2/3)*x) `mod'` 2*pi | x <- [1,2]]
      s = stepsize
  return [((pParam + cos(a))*stepsize,(dParam + sin(a))*stepsize) | a <- angles]


