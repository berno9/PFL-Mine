# PFL - Haskell Coursework

## Group T05_G03

### Group Members

- **Bernardo Marques Soares da Costa**
- **Lara Inês Alves Cunha**

### Contributions

| Name                              | Percentage | Tasks Performed |
|----------------------------------- |------------|-----------------|
| Bernardo Marques Soares da Costa   | 50%        | Implemented functions 1 to 7. Worked on the initial structure of the code and developed the graph manipulation functionalities, such as listing cities, checking adjacency, calculating distances, and checking the graph’s connectivity. |
| Lara Inês Alves Cunha              | 50%        | Implemented functions 8 and 9. Focused on developing the shortest path functionality (`shortestPath`) and solving the Traveling Salesman Problem (`travelSales`). Also responsible for writing the README.md file. |

### Description of Implemented Functions

- **Functions 1 to 7**:
    - Implemented by **Bernardo Marques Soares da Costa**, these functions handle basic graph operations like listing all cities (`cities`), checking if two cities are directly connected (`areAdjacent`), calculating the distance between two cities (`distance`), and verifying the graph’s strong connectivity (`isStronglyConnected`), among others.

- **Functions 8 to 9**:
    - Implemented by **Lara Inês Alves Cunha**, the `shortestPath` function uses a Breadth-First Search (BFS) approach to find all the shortest paths between two cities in the roadmap and implement the `travelSales` function solves the Traveling Salesman Problem (TSP). It returns the shortest route that visits all cities exactly once and returns to the starting city. The function generates all possible permutations of city routes and selects the one with the shortest total distance.

### `shortestPath` Function

This function was implemented using a **Breadth-First Search (BFS)** algorithm to explore all possible paths between two cities and return all the shortest paths. BFS was chosen because it systematically explores all neighbors level by level, which guarantees that the first time a destination city is reached, it is through the shortest path in terms of the number of edges.

#### Auxiliary Data Structures

- **Queue (`[Path]`)**: The BFS algorithm makes use of a queue to explore paths incrementally. Each element in the queue represents a path from the starting city to a potential destination. The queue ensures that the algorithm explores all possible routes in the correct order, starting with shorter paths.
- **List of Found Paths (`[Path]`)**: This structure stores all the paths found that reach the destination city. Only the paths with the shortest distance are kept, and longer paths are discarded.
- **Distance Calculation (`pathLength`)**: For each path, the function `pathLength` calculates the total distance using the previously implemented function `pathDistance`. This is used to sort paths in the queue by their total distance, ensuring that the shortest paths are processed first.

#### Algorithm

1. **Base Case**: If the starting city is the same as the destination city, the shortest path is trivially `[start]`, which is returned immediately.
2. **BFS Exploration**: The algorithm initializes a queue with the starting city and begins exploring all possible adjacent cities. Each time a new city is added to a path, the queue is updated with the extended path.
3. **Path Validation**: Each time the destination city is reached, the path is added to the list of valid paths, as long as its length is equal to or shorter than the paths already found.
4. **Shortest Path Selection**: The queue is ordered by path length using the `sortOn` function, ensuring that paths with the lowest total distance are explored first.
5. **Final Result**: Once the queue is exhausted, the function returns all the shortest paths that connect the starting city to the destination.

### `travelSales` Function

The `travelSales` function was implemented to solve the **Traveling Salesman Problem (TSP)**, which seeks to find the shortest possible route that visits every city exactly once and returns to the starting city. The function generates all possible routes (i.e., permutations of the cities) and selects the one with the shortest total distance.

#### Auxiliary Data Structures

- **List of Cities (`[City]`)**: This list stores all the cities from the `RoadMap` graph. It is used to generate all the possible permutations of city routes.
- **List of Valid Paths (`[Path]`)**: The `validPaths` list filters the possible routes to ensure that only those where all cities are connected are considered.
- **Distance Calculation (`totalDistance`)**: For each path, the function `totalDistance` calculates the total distance using the function `pathDistance`, which was previously implemented. Paths with invalid connections (disconnected cities) are ignored.
  
#### Algorithm

1. **Base Case**: If there are no cities in the roadmap, the function returns an empty list (`[]`).
2. **Route Generation**: The function generates all possible routes using **permutations** of the list of cities, ensuring that each route starts and ends at the same city (the first city in the list).
3. **Path Validation**: The function `isValidRoute` checks whether a given route is valid, meaning all cities are connected. This is done by checking if the `pathDistance` function returns a valid total distance for that route.
4. **Shortest Path Selection**: Once all valid routes are generated, the function uses `minimumBy` to select the path with the shortest total distance, comparing paths using the `totalDistance` function.
5. **Final Result**: If no valid route is found (i.e., if the graph is not fully connected), the function returns an empty list. Otherwise, it returns the shortest valid route.

