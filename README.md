# minesweeper-fs
A Minesweeper game implemented in F#. I implemeted this for a meetup called Functional Dojo in Dallas.

This is the first real program I have implemented in F# and I used it to experiment with various patterns in F#.

Included are 3 projects:
* minesweeper.game - core game logic
* minesweeperfs - console program for playing the game
* minesweeper.solvers - console program for running a few minsweeper solvers against a large number of games and displaying statistics on the results.

## Screenshots

Main Menu:
![Main Menu](/imgs/GamePlay1.png)

Game Play:
* Yellow `?`'s are flagged cells considered to be mines by users.
* Green `#`'s is the current cursor position. The gray cells around it are a UI hint to help users to see the hidden cells arond the cursor.
* White `.`'s are hidden cells
* `{number}`'s and empty cells are exposed cells with the number representing the number of mines around that cell (empty means 0)

![Game Play](/imgs/GamePlay2.png)

Lost Game:
This shows a lost game where the user swept a cell with a mine. This reveals the location of the mines as red `*`'s.
![Lost Game](/imgs/GamePlay3.png)

## Solvers

### Random
The Random solver works by randomly sweeping cells until loss or completion. Needless to say, this was a very poor solver, and usually always fails (I've seen it have a .04% success rate occasionally). I wrote it first to effectively prove the solver framework was working and to use it as a control case.

### Probability Solver
This is my best algorithm so far. It does not work well and is relatively naive. It usually solves 50% of games. The algorithm is as follows:
1. For all hidden cells, find the min and max probability of that mine being a mine. Do this by:
    * For all exposed neighbors of the cell, the probablity of the hidden cell is `(numSurroundingCell - numFlaggedCells) / (numUnflaggedHiddenCells)`. This equation is based on the neighbors counts.
    * If a mine has no exposed neighbors, it's probability is `total remaining mines / total hidden cells`
2. Take all cells which have a max probability of 100% (definitely a mine) and flag them as a mine.
3. If no cells in step 2, find all cells with a min probability of 0% (definitely not a mine) and sweep all of them.
4. If no cells in step 3, pick 1 random cell from the cells with the lowest minimum probility
5. Recursively process the game by returning to step 1 with the result of this iteration.

This algorithm resulted in moderate success and could definitely be improved. For example, it is not intelligent enough to handle the below scenario where the middle H cell cannot be a mine and the other to H cells must be mines. The algorithm calculates the probabilities of a cell from it's neighbors independently. 

```
000
121
HHH 
```

### Machine Learning Solver
I wanted to implement a machine learning solver where it was trained on test cases with a 5x5 slice of cells (25 inputs) and returned whether the central cell was a mine or not. Generating the inputs to this model would be quite easy to do. This route would absolutely be able to learn the scenarios in that caused the probability sovler to fail. 

Unfortunately I was unable to implement this for the following reasons:
* I am still learning ML.
* F# ML libaries are not simple to use or understand without a lot of background in ML. I did not find anything simple like Keras from python.
* Due to the above points, this would take considerable time.
* Lost interest due to the previous point.

### Solver statistics
These results are based on the same 2000 games processed by each solver.

![Solver Stats](/imgs/SolverStats.png)

A perfect sweep is a sweep where the solver was 100% sure a cell was not a mine. An imperfect sweep was when the solver was unsure if a cell was a mine. Minimizing imperfect sweeps should result in a better sovler. The random solver does not track a number of the statistics, only wins/losses stats are relevant.g

## Lessons Learned

### F# is really easy to learn, especially if you know C#
F# was very easy to pickup. The syntax is fairly simple (yet not as simple as a LISP). Knowing C# and the .NET BCL accelerated the learning curve quite a bit. I felt at home in a functional language where I haven't before. Clojure was difficult to get into since it is based on the JVM and I am not familiar with that ecosystem. I understood how to write clojure but struggled with dealing with libraries, the runtime, etc.

### F# type inference and compiler are awesome
I was surprised at how infrequently I had to provide type signatures in my functions. It reduced the noise in my code quite a bit, but still provided type safety. Initially I found myself having to provide type signatures on my functions, but eventually learned patterns to avoid doing so. These patterns are not something I could explain at this point, but I clearly learned them without having to think about them.

I was discussing this with a coworker who claimed that he liked type signatures and found they provided a lot of value and did not think he would enjoy the lack of type signatures in F#. I argued that there are numerous dynamic languages in high use without type signatures at all which does not get in the way of people understanding code. I futher argued that F# is great because it allows you to not provide type signatures, but the compiler will infer and validate types for you.

### Code structure in F# was not immediately apparent
How do you organize your functions? I went through a few iterations and had to do research to feel like I was doing the right thing.

The ubiquitous [F# for fun and profit book](<https://fsharpforfunandprofit.com/posts/recipe-part3/>) led me in the right direction. I ended up implementing the "second approach" where types are declared in a top level, and declare all functions for that type in a sibling module with the same name as the type. This led to code like the following where the pipelined functions usually are in the form of `{SomeType}.{someFunction}` like `Game.testWin`

```fsharp
let sweep x y game = 
let index = Coordinates.getArrayIndex x y game.GameSize
game 
|> Game.tryPlaceMines index
|> sweepCells [index]
|> Game.testWin
|> Game.testLoss index
```

Instead of the following which is a little less clear and readable. It allows readers to understand where a function is implemented and on what it operates on.
```fsharp
let sweep x y game = 
let index = getArrayIndex x y game.GameSize
game 
|> tryPlaceMines index
|> sweepCells [index]
|> testWin
|> testLoss index
```

This style should be further extended to the entire project and I do not think I completed this process.

### Data Structures
F# has a handful of built in data structures. It is important to choose the correct one. 

Within the code, there is a record type called `Game` which originally had an arrayfield `Mines: Cell[]`. Anytime the new state of a game needed to be created and a cell's state changed, it required a new array to be created each time. When playing a game, this may occur tens if not hundreds of times depending on the size of the game. This resulted in creating numerous arrays (of decent size: 100 - 1000 length) where the majority of the references were identical.

I then remebered that in many functional languages, there are built in **persistent data structures**. I had remembered this from doing Clojure and Clojurescript development. Persistent data structures are effectively immutable data structures that have "mutation" functions available which returns a new data structure that references the previous data structure. Due to this, "mutations" are much cheaper. Adding, altering, deleting elements is optimal.

There are a number of persitent data structures in F#.
* `Map<TKey, TValue>`
* `Set<TValue>`
* `list` (implemented as a singly linked list)

I ended up using a `Map<int,Cell>` where the key was the index of the cell. This allowed creating the game's cell's by reusing the same data structure.

### GC
Watching the Visual Studio Diagnostic tools, my code allocated lots of memory and there were numerous GC events. This is likely due to immutability in F#. F#'s record copy-and-update `with` statement causes numerous objects to be created. I am unsure of whether my code or F# itself was the culprit, but I assume both are to blame.

### Async
Async in F# is quite easy, due to language support and immutability in F#. It requires using a computation expression which has interesting syntax (explicit `return`, `let!`s and `use!`s for declarations)

### System.Console only has 16 colors
I wanted to use arbitrary colors for the game but was limited  to those that are included in `System.ConsoleColor`. This was an annoyance, and I did not want to go through the effort of including a library to provide additional colors.

### Fully Rewriting System.Console contents is slow
Early iterations of the UI console had poor performance. My initial implemenation simply cleared the console contents and rewrote them. The poor performance was worsened when I added colors to the UI. When a user held down the arrow keys to move the cursor, the console would flicker rapidly, lag by a few seconds, and feel sluggish. This was due the user input not being buffered. Each character press caused the entire screen to be reprinted. 

To solve this, I changed the UI to only rewrite the characters that changed. This required a decent amount of code to diff the previous state and the new state of the game and move the cursor to those position of differing characters. This resulted in a few bugs that I have not resolved yet. (When returning to the main menu, all empty cells have a different background color. The first character of the last line being changed by user input. etc)

### My best algorithm for solving is bad
It took a lot of work to implement the probability solver, and it was disappointing. 

### Solving Minesweeper is an NP-hard problem
Nearing the end of this project, I searched for algorithms and found these

<https://mrgris.com/projects/minesweepr/>
<http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ordmsw.htm>


### Anonymous functions are hard to write and hard to read

I do not like how anonymous functions are declared in F#. I find it hard to read compared to C#'s lambdas. 

```fsharp
let neighbors = 
game
|> Game.getNeighborCells cell
|> Seq.filter (fun x -> x.State = CellState.Hidden)
|> List.ofSeq
```

vs C#

```csharp
neighbors.Where(x => x.State == CellState.Hidden)
```

This usually leads to me not using anonymous functions and pulling this logic into a declared and named function.