from src.nfl_sim.engine import NFLGameSimulator
import pandas as pd
import numpy as np

def check_play_distribution(samples=500):
    away, home = "KC", "DET"
    print(f"Simulating {samples} games to check play distribution...")
    
    plays = []
    for _ in range(samples):
        sim = NFLGameSimulator(away, home)
        while not sim.game_over:
            sim.simulate_play()
        plays.append(sim.total_plays)
    
    df = pd.DataFrame(plays, columns=['total_plays'])
    
    print("\n--- SIMULATED PLAY DISTRIBUTION ---")
    print(f"Mean:   {df['total_plays'].mean():.1f} (Target: 118)")
    print(f"Median: {df['total_plays'].median():.1f}")
    print(f"Min:    {df['total_plays'].min()}")
    print(f"Max:    {df['total_plays'].max()}")
    print(f"StdDev: {df['total_plays'].std():.2f}")
    
    # Simple ASCII Histogram
    counts, bins = np.histogram(plays, bins=10)
    print("\n--- HISTOGRAM ---")
    for i in range(len(counts)):
        bar = "#" * int(counts[i] / (samples / 50))
        print(f"{int(bins[i]):>3}-{int(bins[i+1]):<3} | {bar} ({counts[i]})")

if __name__ == "__main__":
    check_play_distribution()
