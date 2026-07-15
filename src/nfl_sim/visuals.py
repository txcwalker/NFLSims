import os

class Visualizer:
    def __init__(self, output_dir='reports/charts'):
        self.output_dir = output_dir
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
        # NOTE: matplotlib/seaborn are intentionally NOT imported at module level.
        # Importing them here would cause PIL DLL load failures in multiprocessing
        # worker processes on Windows. All plot methods lazy-import them locally.

    def plot_fantasy_distribution(self, player_df, team, slot, scoring_type="DK"):
        """Generates a bell curve (KDE) for a specific player's fantasy points."""
        import matplotlib.pyplot as plt
        import seaborn as sns
        sns.set_theme(style="whitegrid")

        subset = player_df[(player_df['Team'] == team) & (player_df['Slot'] == slot)]
        if subset.empty: return
        
        col = 'dk_score' if scoring_type == "DK" else 'fd_score'
        
        plt.figure(figsize=(10, 6))
        sns.histplot(subset[col], kde=True, color='dodgerblue', bins=15)
        
        # Add Floor/Ceiling lines
        floor = subset[col].quantile(0.05)
        ceiling = subset[col].quantile(0.95)
        median = subset[col].median()
        
        plt.axvline(floor, color='red', linestyle='--', label=f'Floor (5%): {floor:.1f}')
        plt.axvline(median, color='black', linestyle='-', label=f'Median: {median:.1f}')
        plt.axvline(ceiling, color='green', linestyle='--', label=f'Ceiling (95%): {ceiling:.1f}')
        
        plt.title(f'{team} {slot} Fantasy Distribution ({scoring_type})')
        plt.xlabel('Fantasy Points')
        plt.ylabel('Frequency')
        plt.legend()
        
        filename = f"{team}_{slot}_{scoring_type}_dist.png"
        plt.savefig(os.path.join(self.output_dir, filename))
        plt.close()
        return filename

    def plot_game_totals(self, game_df):
        """Generates a histogram of total scores across all simulations."""
        import matplotlib.pyplot as plt
        import seaborn as sns
        sns.set_theme(style="whitegrid")

        plt.figure(figsize=(10, 6))
        sns.histplot(game_df['total'], kde=True, color='purple', bins=15)
        
        avg = game_df['total'].mean()
        plt.axvline(avg, color='orange', linestyle='-', label=f'Avg Total: {avg:.1f}')
        
        plt.title('Game Total Score Distribution')
        plt.xlabel('Combined Score')
        plt.legend()
        
        filename = "game_totals_dist.png"
        plt.savefig(os.path.join(self.output_dir, filename))
        plt.close()
        return filename

    def plot_spread_distribution(self, game_df, team_a):
        """Generates a histogram of the point spread."""
        import matplotlib.pyplot as plt
        import seaborn as sns
        sns.set_theme(style="whitegrid")

        plt.figure(figsize=(10, 6))
        sns.histplot(game_df['spread'], kde=True, color='teal', bins=15)
        
        avg_spread = game_df['spread'].mean()
        plt.axvline(avg_spread, color='red', linestyle='-', label=f'Avg Spread: {avg_spread:.1f}')
        plt.axvline(0, color='black', linewidth=1)
        
        plt.title(f'Point Spread Distribution ({team_a} relative)')
        plt.xlabel(f'Spread ({team_a} Score - Opponent Score)')
        plt.legend()
        
        filename = "spread_dist.png"
        plt.savefig(os.path.join(self.output_dir, filename))
        plt.close()
        return filename
