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
-- Takes a road map as an argument 
-- Produces all cities in it, using list comprehension
cities :: RoadMap -> [City]
cities roadMap = nub [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]

--2
-- Takes in a road map and two cities
-- Goes through each tuple in the road map checking if the cities are the ones we're looking for
-- If they are on the same tuple, they are adjacent
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 =
    any (\(cityA, cityB, _) -> (city1 == cityA && city2 == cityB) || (city1 == cityB && city2 == cityA)) roadMap    

--3
-- Takes in a road map and two cities
-- Using list comprehension, extracts distance if an edge between two cities is found
-- in other words, if a tuple with the two cities exists, extract distance
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case [(d) | (c1, c2, d) <- roadMap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] of
        [d] -> Just d
        _   -> Nothing

--4
-- Takes in a road map and a city
-- Finds all tuples where the desired city appears
-- Joins two list comprehensions of adjacent cities, due to bidirectional nature of the road map
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, d) | (c1, c2, d) <- roadMap, c1 == city] ++ [(c1, d) | (c1, c2, d) <-roadMap, c2 == city]

--5
-- Takes in a road map and a list of cities
-- Recursively traverses each tuple
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
-- using cities, extract all cities
-- using inDegree, calculate all cities degrees
-- extract highest degree
-- traverse the degrees list and find cities with the highest degree
rome :: RoadMap -> [City]
rome roadMap = [c | (c, degree) <- degrees, degree == maxDegree]
    where 
        allCities = cities roadMap
        degrees = [(city, inDegree roadMap city) | city <- allCities]
        maxDegree = maximum (map snd degrees) -- highest of all second elements of (city, its degree) tuples

--7
-- Depth first search
-- Takes in a road map, a starting point and a list of visited cities
-- If a city is on the visited list, return that list (the search is over)
-- If not, extract all adjacent cities to the current one
-- and add it to the visited list
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited 
    | city `elem` visited = visited  
    | otherwise = foldl (\acc nextCity -> dfs roadMap nextCity acc) (city:visited) adjacentCities
        where
            adjacentCities = map fst (adjacent roadMap city)
-- Takes in a road map
-- Extracts all cities
-- And performs a dfs, returning all visited cities
-- Then, returns true if the number of visited cities is the same as the total number of cities
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = length visitedFromFirst == length allCities
    where
        allCities = cities roadMap
        visitedFromFirst = dfs roadMap (head allCities) []

--8
-- shortestPath :: RoadMap -> City -> City -> [Path]
-- Uses BFS to find all shortest paths between two cities.
-- Arguments:
--   roadMap - A RoadMap representing the network of cities and distances between them.
--   start - The starting city.
--   end - The destination city.
-- Returns:
--   A list of all possible shortest paths (lists of cities) from the start city to the end city.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end
    | start == end = [[start]]  -- If the origin and destination cities are the same, the shortest route is to the city itself
    | otherwise = bfs [[start]] [] -- We use BFS from the city of origin
  where
    -- BFS helper function to explore paths
    -- bfs :: [Path] -> [Path] -> [Path]
    -- Arguments:
    --   queue - A list of paths that need to be explored.
    --   foundPaths - A list of paths that have reached the destination.
    -- Returns:
    --   An updated list of found paths that reach the destination.
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

    -- addPath :: [Path] -> Path -> [Path]
    -- Helper function to add a path to the list of found paths, keeping only the shortest ones.
    -- Arguments:
    --   foundPaths - A list of already found shortest paths.
    --   newPath - The path that we are adding if it has the shortest distance.
    -- Returns:
    --   An updated list of shortest paths.
    addPath :: [Path] -> Path -> [Path]
    addPath [] newPath = [newPath] -- If the path list is empty, we add the new path
    addPath foundPaths newPath
        | pathLength newPath < pathLength (head foundPaths) = [newPath]  -- If the new path is shorter, we discard the previous ones
        | pathLength newPath == pathLength (head foundPaths) = newPath : foundPaths -- If it is the same length, we add
        | otherwise = foundPaths  -- Otherwise, we keep the paths already found

    -- pathLength :: Path -> Distance
    -- Helper function to calculate the length of a path.
    -- Arguments:
    --   path - The path whose length is being calculated.
    -- Returns:
    --   The total distance of the path as a Distance.
    pathLength :: Path -> Distance
    pathLength path = case pathDistance roadMap path of
                        Just d -> d
                        Nothing -> maxBound :: Int -- We set the value too high for invalid paths

--9
-- travelSales :: RoadMap -> Path
-- Solves the Traveling Salesman Problem (TSP) by finding the shortest path that visits all cities exactly once and returns to the starting city.
-- Arguments:
--   roadMap - A RoadMap representing the network of cities and distances between them.
-- Returns:
--   A path representing the shortest possible route that visits each city exactly once and returns to the starting city, or an empty list if no such path exists.
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
    
    -- validPaths :: [Path]
    -- Filters only valid paths (those where all cities are connected).
    validPaths = filter (isValidRoute roadMap) possibleRoutes

    -- comparePaths :: Path -> Path -> Ordering
    -- Function to compare two paths by their total length.
    -- Arguments:
    --   p1, p2 - Paths to be compared based on their distances.
    -- Returns:
    --   The ordering based on the total distances of p1 and p2.
    comparePaths :: Path -> Path -> Ordering
    comparePaths p1 p2 = compare (totalDistance p1) (totalDistance p2)

    -- totalDistance :: Path -> Distance
    -- Calculates the total distance of a path.
    -- Arguments:
    --   path - The path whose total distance is being calculated.
    -- Returns:
    --   The total distance of the path, or a maximum bound if the path is invalid.
    totalDistance :: Path -> Distance
    totalDistance path = case pathDistance roadMap path of
                           Just d -> d
                           Nothing -> maxBound  -- Invalid paths will have a maximum distance

-- isValidRoute :: RoadMap -> Path -> Bool
-- Checks if a route is valid (all cities are connected).
-- Arguments:
--   roadMap - A RoadMap representing the network of cities and distances between them.
--   path - The path being checked for connectivity.
-- Returns:
--   True if the route is valid (all cities are connected), False otherwise.
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