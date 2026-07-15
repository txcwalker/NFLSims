# Phase 2: Vectorized State Machine (The "Big Tech" Solution)

## The Problem
Even after Phase 1, our simulation was fundamentally sequential: it took *one* game, simulated play 1, then play 2, then play 3, until the game ended. Then it moved to the next game. 
If we want to simulate 10,000 games, we are running 10,000 games × ~150 plays = 1,500,000 separate model predictions. 
In computer science, this is related to **Big O Notation**. Our runtime scales linearly—or **O(N)**—meaning if we want to run 10x more games, it takes exactly 10x longer. 

## How We Fixed It: Vectorization

Instead of simulating 10,000 games one by one, we simulate **Play 1 for all 10,000 games at the exact same time.** Then Play 2 for all 10,000 games. We rotate the problem 90 degrees.

### 1. The Vectorized State Matrix
**How:** Instead of tracking `self.yardline = 75` for one game, we track an array of yardlines: `self.yardlines = [75, 75, 75, ..., 75]` for all 10,000 games simultaneously. 
**Why:** A computer processor is designed to perform the exact same math operation on a large block of memory extremely fast. 

### 2. Batched Inference
**How:** Instead of asking the XGBoost model to predict the outcome of 1 play, 10,000 different times, we hand the model a matrix of 10,000 plays and ask for 10,000 predictions *in a single call*.
**Why:** Machine Learning models (especially XGBoost and neural networks) are heavily optimized for matrix math. Calling `.predict()` has overhead. Calling it once for 10,000 rows takes almost the exact same amount of time as calling it once for 1 row. This changes our scaling from O(N) to effectively O(1) for the model inference step.

### 3. Vectorized State Updates (Boolean Masking)
**How:** To figure out who got a first down, we don't use a `for` loop or `if/else` statements. We ask Python a single mathematical question: `is_first_down = (distance <= 0)`. Python returns a list of True/False values (a Boolean Mask) for all 10,000 games instantly. We then update the down and distance for only the `True` games in one swift motion.
**Why:** `if/else` statements slow computers down because the processor has to guess which path the code will take (called "branching"). Boolean masking avoids branching entirely.

---

## Vocabulary & Concepts Sheet

* **Big O Notation:** A mathematical way to describe how the runtime of an algorithm grows as the amount of data grows. **O(N)** means runtime grows directly in proportion to the data (10x games = 10x time). **O(1)** means the time stays roughly the same no matter how much data you add.
* **Vectorization:** The process of converting an algorithm that operates on a single piece of data at a time into one that operates on an entire block (or "vector") of data at once.
* **State Machine:** A system that stores the status of something at a given time (the "State"). In our engine, the "State" is the yardline, down, distance, and game clock.
* **Batched Inference:** Passing a large group (batch) of data into a machine learning model at one time, rather than feeding it one row at a time.
* **NumPy Broadcasting:** A feature of NumPy that allows you to do math on arrays of different sizes without writing loops. For example, `array - 5` subtracts 5 from *every single number* in the array instantly.
* **Boolean Masking:** Using an array of `True/False` values to filter or update another array. Example: `yardlines[is_touchdown] = 75` instantly resets the yardline to 75 for every game where a touchdown occurred.
* **SIMD (Single Instruction, Multiple Data):** A hardware concept where a CPU uses one command to perform the same operation on multiple pieces of data simultaneously. Vectorized Python code taps into the CPU's SIMD capabilities under the hood.
