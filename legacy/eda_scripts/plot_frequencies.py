import pandas as pd
import matplotlib.pyplot as plt

print("Loading refreshed scenarios...")
df = pd.read_csv('data/scenarios/scenarios_database.csv')
counts = df['player_slot'].value_counts()

# Create dynamic, aesthetic bar chart
plt.figure(figsize=(14, 8))
colors = ['#1f77b4', '#aec7e8', '#4287f5', '#60a5fa', '#3b82f6',
          '#ff7f0e', '#ffbb78', '#fca5a5',
          '#2ca02c', '#d62728', '#9467bd',
          '#8b5cf6', '#a78bfa']

# Order exactly as specified by the user
ordered_positions = [
    'WR1', 'WR2', 'WR3', 'WR4', 'WR5',
    'TE1', 'TE2', 'TE3',
    'RB1', 'RB2', 'RB3',
    'QB1', 'QB2'
]

ordered_counts = counts.reindex(ordered_positions).fillna(0)

bars = plt.bar(ordered_counts.index, ordered_counts.values, color=colors[:len(ordered_positions)], edgecolor='black', alpha=0.85)

# Add grid lines
plt.grid(axis='y', linestyle='--', alpha=0.7)

# Title and labels
plt.title('All Position Role Frequencies in NFL Scenarios Database', fontsize=16, fontweight='bold', pad=15)
plt.xlabel('Player Slot / Position Role', fontsize=12, labelpad=10)
plt.ylabel('Total Plays in Database', fontsize=12, labelpad=10)

# Add text labels on top of bars
for bar in bars:
    yval = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2, yval + 5000, f"{int(yval):,}", ha='center', va='bottom', fontsize=10, fontweight='bold')

plt.tight_layout()
plt.savefig('Notebooks/position_frequencies.png', dpi=150)
print("Plot successfully updated to Notebooks/position_frequencies.png")
