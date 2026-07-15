# Phase 1: Bottleneck Removal (The "Quick Wins")

## The Problem
When simulating an NFL game, the computer has to run thousands of calculations. Our initial version was "inefficient" because it performed tasks that are very slow for a computer inside the tightest part of the loop (the play-by-play simulation). Specifically:
1. **Reading from the Hard Drive:** Opening and reading `.json` files for every single game.
2. **Heavy Data Structures:** Creating a Pandas DataFrame for every single prediction and every single stat harvest.
3. **Sequential Execution:** Running one game at a time on a single CPU core, while the other cores sat idle.

## How We Fixed It

### 1. In-Memory Asset Loading
**How:** We moved all `json.load()` calls out of the `NFLGameEngine` and into the `BatchSimulator`. We now load the DNA, rosters, and coach tendencies *once* at the start of the batch, and pass them down as in-memory Python dictionaries.
**Why:** Reading from a hard drive (Disk I/O) is incredibly slow compared to reading from RAM. By loading them once, we save the computer from asking the hard drive for the same data 10,000 times.

### 2. Stripping Pandas from the Loop
**How:** We removed all `pd.DataFrame()` calls during the play execution phase. We modified the model inference files to take raw Python dictionaries and convert them directly into 2D NumPy arrays (`np.array([...])`).
**Why:** Pandas is fantastic for analyzing data, but it is very "heavy." Every time you create a DataFrame, Pandas does a lot of background checks and metadata creation. NumPy arrays are "closer to the metal"—they are just raw blocks of numbers in memory, which XGBoost models can process in fractions of a millisecond.

### 3. Optimizing Data Harvesting
**How:** We stopped using `iterrows()` to extract player stats. Instead, the engine now returns a flat list of dictionaries at the end of the game, and we only convert that list to a Pandas DataFrame once the *entire batch* is complete.
**Why:** Iterating through a Pandas DataFrame row-by-row is a notorious anti-pattern because it reconstructs Pandas objects for every row. A list of dictionaries is a native Python data structure that is extremely fast to append to.

### 4. Implementing Multiprocessing
**How:** We wrapped the game simulation loop in a `concurrent.futures.ProcessPoolExecutor`.
**Why:** Modern computers have multiple "brains" (CPU cores). Standard Python only uses one core at a time. Multiprocessing splits the 10,000 games into chunks and hands them to all your CPU cores simultaneously, effectively dividing the run time by the number of cores you have.

### 5. Dynamic Roster-to-Slot Mapping (The KeyError: 'Slot' Resolution)
**How:** We implemented an automated mapping engine inside `BatchSimulator.__init__`. It inspects each team's dynamic rosters once at startup, classifies players into positions, ranks them descending by target or carry share, and uses common surname string-matching (e.g., matching `Kelce`, `Kincaid`, `Knox`) to separate TEs from WRs. We inject this precomputed map into our worker threads, which stamp the correct `'Slot'` tag (e.g., `'WR1'`, `'TE1'`, `'RB1'`) on each player's statistical row at the end of the simulation.
**Why:** In early drafts, the simulation hardcoded slots as names. When we transitioned to using realistic roster datasets with real-world names (like `"K.Shakir"`, `"J.Cook"`), downstream visualization scripts broke because they expected rigid fantasy slots. Dynamic mapping gives us the best of both worlds: we get to simulate named real-world players, but the output still plugs seamlessly into our analytics dashboard.

---

## Vocabulary & Concepts Sheet

* **Bottleneck:** The slowest part of a system that holds up the entire process. No matter how fast your models are, if they have to wait for the hard drive to load a file, the hard drive is the bottleneck.
* **Disk I/O (Input/Output):** The process of reading from or writing to a physical hard drive. It is magnitudes slower than reading from RAM (Random Access Memory).
* **Overhead:** The hidden cost of using a tool. Pandas has high overhead because it comes with row indexes, column names, and data type checks. NumPy has low overhead.
* **Pandas DataFrame vs. NumPy Array:** A DataFrame is like a fully featured Excel spreadsheet. A NumPy array is just a grid of raw numbers. Machine learning models only need the numbers, so building the "Excel spreadsheet" every play is a waste of time.
* **`iterrows()`:** A Pandas function that loops over a DataFrame one row at a time. It is slow because it creates a new Pandas Series object for every single row.
* **Multiprocessing / CPU Cores:** Using multiple processors (cores) to do work at the same time. Think of it as opening 8 checkout lanes at a grocery store instead of forcing everyone through 1 lane.
* **Pickling (Serialization):** When Python sends data to a different CPU core, it has to pack the data up into a format that can be sent over the wire (pickling), and the other core unpacks it (unpickling). We have to be careful not to send things that can't be packed (like certain complex model objects) across processes.
* **Dynamic Positional Mapping:** Automatically sorting and ranking complex data structures (like real-world player names and shares) at startup into standard, standardized slots (like WR1, RB1) based on mathematical rules. This prevents hardcoding and ensures the simulation is flexible enough to take any dynamic team roster.
