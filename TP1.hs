import qualified Data.List
import Data.List (groupBy, sortOn, nub, maximumBy, permutations, minimumBy)
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--1
-- Takes a road map as an argument and produces all cities in it, using list comprehension
cities :: RoadMap -> [City]
cities roadMap = nub [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]

--2
-- Goes through each tuple in the road map checking if the cities are the ones we're looking for
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 =
    any (\(cityA, cityB, _) -> (city1 == cityA && city2 == cityB) || (city1 == cityB && city2 == cityA)) roadMap    

--3
-- Using list comprehension, extracts distance if an edge between two cities is found
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case [(d) | (c1, c2, d) <- roadMap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] of
        [d] -> Just d
        _   -> Nothing

--4
-- Joins two list comprehensions of adjacent cities, due to bidirectional nature of the road map
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, d) | (c1, c2, d) <- roadMap, c1 == city] ++ [(c1, d) | (c1, c2, d) <-roadMap, c2 == city]

--5
-- Using the distance function, adds distance between each two cities in the path
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
-- the degree is the number of cities one city is connected to
-- count the number of tuples the desired city appears
inDegree :: RoadMap -> City -> Int
inDegree roadMap c = length [(c1, c2) | (c1, c2, _) <- roadMap, c1 == c || c2 == c]
-- using inDegree, calculate the highest degree and find cities with that degree
rome :: RoadMap -> [City]
rome roadMap = [c | (c, degree) <- degrees, degree == maxDegree]
    where 
        allCities = cities roadMap
        degrees = [(city, inDegree roadMap city) | city <- allCities]
        maxDegree = maximum (map snd degrees) -- highest of all second elements of (city, its degree) tuples

--7
-- Depth first search
-- takes in a road map, a starting point and a list of visited cities
--
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited 
    | city `elem` visited = visited  -- city already visited
    | otherwise = foldl (\acc nextCity -> dfs roadMap nextCity acc) (city:visited) adjacentCities
        where
            adjacentCities = map fst (adjacent roadMap city)
-- uses dfs and cities
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = length visitedFromFirst == length allCities
    where
        allCities = cities roadMap
        visitedFromFirst = dfs roadMap (head allCities) []

--8
-- Uses BFS to find all shortest paths between two cities
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end
    | start == end = [[start]]  -- If the origin and destination cities are the same, the shortest route is to the city itself
    | otherwise = bfs [[start]] [] -- We use BFS from the city of origin
  where
    -- BFS helper function to explore paths
    bfs :: [Path] -> [Path] -> [Path]
    bfs [] foundPaths = foundPaths  -- When the queue is empty, we finish and return the found paths
    bfs (path:queue) foundPaths
        | currentCity == end =
            -- If we reach the destination, we add the current path to the list of found paths
            bfs queue (addPath foundPaths path)
        | otherwise =
            -- Otherwise, we continue to explore adjacent paths
            let newPaths = [path ++ [nextCity] | (nextCity, _) <- adjacent roadMap currentCity,
                                                nextCity `notElem` path] -- We add the next city to the current path
                extendedQueue = queue ++ newPaths -- We added the new paths to the queue
                sortedQueue = sortOn pathLength extendedQueue -- We order the queue by the paths with the shortest distance
            in bfs sortedQueue foundPaths
      where
        currentCity = last path  -- The current city is the last element of the current path

    -- Helper function to add a path to the list of found paths, keeping only the shortest ones
    addPath :: [Path] -> Path -> [Path]
    addPath [] newPath = [newPath] -- If the path list is empty, we add the new path
    addPath foundPaths newPath
        | pathLength newPath < pathLength (head foundPaths) = [newPath]  -- If the new path is shorter, we discard the previous ones
        | pathLength newPath == pathLength (head foundPaths) = newPath : foundPaths -- If it is the same length, we add
        | otherwise = foundPaths  -- Otherwise, we keep the paths already found

    -- Helper function to calculate the length of a path
    pathLength :: Path -> Distance
    pathLength path = case pathDistance roadMap path of
                        Just d -> d
                        Nothing -> maxBound :: Int -- We set the value too high for invalid paths

--9
-- Solution to the Traveling Salesman Problem (TSP)
travelSales :: RoadMap -> Path
travelSales roadMap
    | null allCities = []  -- If there are no cities, returns an empty list
    | otherwise = case validPaths of
                    [] -> []  -- If there are no valid paths, returns an empty list
                    _  -> minimumBy comparePaths validPaths  -- Returns the path with the shortest total distance
  where
    allCities = cities roadMap  -- Get all cities
    startingCity = head allCities  -- We chose the first city as a starting point
    possibleRoutes = map (\perm -> startingCity : perm ++ [startingCity]) (permutations (tail allCities))  -- Generates all possible routes (permutations of cities)
    
    -- Filters only valid paths (those where all cities are connected)
    validPaths = filter (isValidRoute roadMap) possibleRoutes

    -- Function to compare two paths by their total length
    comparePaths :: Path -> Path -> Ordering
    comparePaths p1 p2 = compare (totalDistance p1) (totalDistance p2)

    -- Calculates the total distance of a path
    totalDistance :: Path -> Distance
    totalDistance path = case pathDistance roadMap path of
                           Just d -> d
                           Nothing -> maxBound  -- Invalid paths will have a maximum distance

-- Checks if a route is valid (all cities are connected)
isValidRoute :: RoadMap -> Path -> Bool
isValidRoute roadMap path = case pathDistance roadMap path of
                              Just _ -> True
                              Nothing -> False

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

testTravelSales1 = travelSales gTest1

testTravelSales2 = travelSales gTest2

testTravelSales3 = travelSales gTest3