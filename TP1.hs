import qualified Data.List
import Data.List (groupBy, sortOn, nub, maximumBy)
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--helper functions
-- used in rome
inDegree :: RoadMap -> City -> Int
inDegree roadMap c = length [(c1, c2) | (c1, c2, _) <- roadMap, c1 == c || c2 == c]
-- used in isStronglyConnected
-- uses adjacentCities
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited 
    | city `elem` visited = visited
    | otherwise = foldl (\acc nextCity -> dfs roadMap nextCity acc) (city:visited) adjacentCities
        where
            adjacentCities = map fst (adjacent roadMap city)


--1
cities :: RoadMap -> [City]
cities roadMap = nub [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]

--2
-- go through each tuple checking if the cities are the ones we're looking for
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 =
    any (\(cityA, cityB, _) -> (city1 == cityA && city2 == cityB) || (city1 == cityB && city2 == cityA)) roadMap    

--3
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case [(d) | (c1, c2, d) <- roadMap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] of
        [d] -> Just d
        _   -> Nothing

--4
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, d) | (c1, c2, d) <- roadMap, c1 == city] ++ [(c1, d) | (c1, c2, d) <-roadMap, c2 == city]

--5
-- uses distance
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (c1:c2:cs) =
    case distance roadMap c1 c2 of
        Just d -> case pathDistance roadMap (c2:cs) of
                    Just rest -> Just (d + rest)
                    Nothing -> Nothing
        Nothing -> Nothing

--6
-- uses inDegree and cities
rome :: RoadMap -> [City]
rome roadMap = [c | (c, degree) <- degrees, degree == maxDegree]
    where 
        allCities = cities roadMap
        degrees = [(city, inDegree roadMap city) | city <- allCities]
        maxDegree = maximum (map snd degrees)

--7
-- uses dfs and cities
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = length visitedFromFirst == length allCities
    where
        allCities = cities roadMap
        visitedFromFirst = dfs roadMap (head allCities) []

--8
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]