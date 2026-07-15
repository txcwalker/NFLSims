# Python Optimization Libraries — Study Notes

## What Is Optimization?

Optimization is the process of finding the **best solution** from a set of possible solutions, subject to constraints.

**General form:**
- **Objective function**: The thing you're trying to maximize or minimize (e.g., maximize total projected DFS score)
- **Decision variables**: The choices you're making (e.g., which players to include)
- **Constraints**: Rules you must respect (e.g., salary cap ≤ $50,000, exactly 1 QB)

This class of problem appears everywhere:
- **Logistics**: How do you route delivery trucks to minimize total miles driven?
- **Finance**: How do you allocate a portfolio to maximize return for a given risk tolerance?
- **Manufacturing**: How do you schedule a factory to minimize cost while meeting demand?
- **DFS**: How do you pick 9 players to maximize projected score under salary and positional constraints?

---

## The Core Problem Type: Integer Linear Programming (ILP)

Our DFS problem is a specific subtype called **Integer Linear Programming (ILP)**:

- **Linear**: The objective and constraints are all linear relationships (no exponents, no multiplications of variables)
- **Integer**: Each decision variable must be an integer — specifically 0 or 1 (a player is either IN the lineup or OUT)

### Why ILP?

A naive approach would be to check every possible combination of 9 players from a pool of, say, 150. That's C(150, 9) = ~**8.5 billion combinations**. Even at 1 million checks per second, that takes 2+ hours per lineup.

ILP solvers use mathematical shortcuts (branch-and-bound, cutting planes) to find the provably optimal answer without checking every combination. Typically under **1 millisecond** per lineup for a DFS-sized problem.

---

## Library Comparisons

### 1. PuLP ⭐ (Our Primary Choice)

**What it is**: A Python library for formulating and solving Linear and Integer Linear Programs. It acts as a wrapper around several solver backends (CBC, GLPK, Gurobi, CPLEX).

**Best for**: Problems with tens to thousands of variables and constraints — exactly our DFS problem.

**Industries**: Operations research, logistics, supply chain, sports analytics, scheduling.

**Why we chose it**:
- Simple, readable Python API
- Ships with the free **CBC solver** (open source, fast enough for DFS)
- Well-documented, widely used in open-source DFS tools
- Can drop in a commercial solver (Gurobi) later if needed for speed

**DFS Example**:
```python
import pulp

prob = pulp.LpProblem("DFS_Lineup", pulp.LpMaximize)

# Decision variables: 1 if player selected, 0 if not
x = [pulp.LpVariable(f"player_{i}", cat="Binary") for i in range(n_players)]

# Objective: maximize projected score
prob += pulp.lpSum(projections[i] * x[i] for i in range(n_players))

# Constraint: salary cap
prob += pulp.lpSum(salaries[i] * x[i] for i in range(n_players)) <= 50000

# Constraint: exactly 1 QB
prob += pulp.lpSum(x[i] for i in qb_indices) == 1

# Solve
prob.solve(pulp.PULP_CBC_CMD(msg=0))
```

**Docs**: https://coin-or.github.io/pulp/

---

### 2. OR-Tools (Google)

**What it is**: Google's open-source optimization suite, covering constraint programming, linear programming, routing, and more.

**Best for**: Larger, more complex problems — routing problems with thousands of nodes, constraint satisfaction, scheduling at scale.

**Industries**: Google Maps routing, supply chain at scale, airline scheduling, chip design.

**Why we didn't pick it (yet)**: More complex API than PuLP for our use case. The power is there if we ever need it — for example, if we run portfolio optimization at scale across hundreds of thousands of player combinations simultaneously.

**Docs**: https://developers.google.com/optimization

---

### 3. scipy.optimize

**What it is**: The optimization module within SciPy, Python's scientific computing library.

**Best for**: Continuous optimization problems — finding the minimum of a smooth mathematical function. Not ideal for binary (0/1) problems.

**Industries**: Physics simulation, machine learning hyperparameter tuning, curve fitting, engineering design.

**Where we use it in our project**: Useful for **portfolio-level** optimization — e.g., finding the optimal exposure weights for each lineup cluster. Not for the core lineup selection problem (which needs ILP).

**Docs**: https://docs.scipy.org/doc/scipy/reference/optimize.html

---

### 4. Gurobi / CPLEX (Commercial)

**What they are**: The industry-leading commercial ILP solvers. Orders of magnitude faster than free solvers on large problems.

**Best for**: Enterprise-scale optimization — airline crew scheduling, Wall Street portfolio optimization, logistics at Amazon scale.

**Industries**: Finance (portfolio optimization, risk management), logistics, energy (grid optimization), pharmaceutical supply chain.

**Cost**: Gurobi has a free academic license; commercial licenses start at ~$10K/year. CPLEX similar.

**When we'd switch**: If generating 1000 lineups took more than a few seconds. For now, PuLP + CBC will be well within performance budget.

---

## Key Concepts Glossary

| Term | Meaning |
|---|---|
| **Objective Function** | The mathematical expression you're maximizing or minimizing |
| **Decision Variable** | A value the solver is choosing (e.g., include player X: yes/no) |
| **Binary Variable** | A decision variable constrained to be 0 or 1 |
| **Constraint** | A rule that must be satisfied (salary cap, position requirements) |
| **Feasible Solution** | Any solution that satisfies all constraints |
| **Optimal Solution** | The feasible solution that maximizes/minimizes the objective |
| **Branch and Bound** | Core algorithm ILP solvers use: systematically explore the solution tree, pruning branches that can't beat the current best |
| **Relaxation** | Temporarily treating integer variables as continuous to get an upper bound |
| **Infeasible** | No valid solution exists given the constraints (e.g., impossible to build a lineup under salary cap) |

---

## How This Connects to Our DFS Optimizer

```
Player Pool (150 players)
        ↓
ILP Formulation (PuLP)
  - Objective: max Σ(projected_score[i] × selected[i])
  - Constraint: Σ(salary[i] × selected[i]) ≤ 50000
  - Constraint: exactly 1 QB, 2 RBs, 3 WRs, 1 TE, 1 FLEX, 1 DST
  - Constraint: min_unique_players vs previous lineups
  - Constraint: max exposure per player
        ↓
CBC Solver (runs in < 1ms per lineup)
        ↓
Optimal Lineup (9 players)
        ↓
Repeat N times with stochastic score draws
        ↓
N Diverse Lineups → Portfolio Analysis
```

The stochastic scoring layer (drawing from sim distributions) is what happens *before* the ILP solver runs for each lineup. The ILP finds the optimal 9 players given *that draw's* scores. Each draw produces slightly different optimal combinations → natural lineup diversity.
