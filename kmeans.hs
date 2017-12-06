import Data.List(minimumBy, foldl')
import Data.Ord(comparing)
import Debug.Trace

points = [
    (11,11) , (10,10) , (12,12)
  , (4,4) , (3,3) , (5,5) 
  , (7,7) , (6,6) , (8,8)
  ]
cutoff = 10

main :: IO ()
main =
    let x = kmeans 3 points
    in putStrLn $ show x

type Point = (Double,Double)

kmeans :: Int -> [Point] -> [Point]
kmeans k ps = g' where
    g = initialGuess k ps
    iters = replicate cutoff (refine ps) 
    g' = foldl' (\x f->f x) g iters

initialGuess :: Int -> [Point] -> [Point]
initialGuess k ps = fmap perturb $ zip ps $ replicate k $ centroid ps
    
perturb :: (Point,Point) -> Point
perturb ((bx,by),(x,y)) = (x + bx*1e-3, y + by*1e-3)

refine :: [Point] -> [Point] -> [Point]
refine ps g = trace ("g: "++show g) g' where
    ls = zip ps $ (labelByMinDist g) <$> ps
    genKthC = \x -> (fmap fst) $ (filter ((==x) . snd)) ls
    clusters = trace ("\t\t\t\t\tls: "++show (snd <$> ls)) $
                genKthC <$> [0..(length g)-1]
    g' = fmap centroid $ (\(cen,cs)->cen:cs) <$> (zip g clusters)

labelByMinDist :: [Point] -> Point -> Int
labelByMinDist cs p = l where
    ldists = zip [0..] $ (distance2 p) <$> cs
    (l,_) = minimumBy (comparing snd) ldists

distance2 :: Point -> Point -> Double
distance2 (a,b) (c,d) = (a-c)^2 + (b-d)^2

centroid :: [Point] -> Point
centroid cs = (x,y) where
    x = (sum $ fst <$> cs) / fromIntegral (length cs)
    y = (sum $ snd <$> cs) / fromIntegral (length cs)
