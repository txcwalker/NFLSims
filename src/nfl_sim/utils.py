import re


def resolve_iteration_range(loaded_model):
    """Return the (0, best_iteration + 1) tree slice for an XGBoost model that
    was trained with early stopping, or None (meaning "use every tree") for one
    that wasn't.

    Inputs
    ------
    loaded_model : an sklearn wrapper (XGBRegressor / XGBClassifier, freshly
        `joblib.load`-ed) OR a raw `xgboost.Booster`. For the wrapper the cutoff
        is read from `.best_iteration`; for a booster (e.g. one loaded from a
        native `.json`) it is read from `booster.attributes()['best_iteration']`.

    Output
    ------
    tuple[int, int] -- pass straight to `booster.inplace_predict(
        X, iteration_range=...)`. `(0, best_iteration + 1)` for an early-stopped
        model; `(0, 0)` (xgboost's "use every tree" sentinel) otherwise, so the
        return value can be handed over unconditionally. Note: `inplace_predict`
        does NOT accept `None` here -- it subscripts the tuple directly.

    Why this exists
    ---------------
    `early_stopping_rounds` records `best_iteration` but leaves the extra
    (post-checkpoint, noise-fitting) trees in the saved artifact. The sklearn
    `.predict()` path auto-truncates to `best_iteration`; the faster
    `booster.inplace_predict()` path -- which the game engine uses everywhere --
    does NOT unless `iteration_range` is passed. Without this, a model silently
    serves more trees than the version that was validated (measured: up to
    0.15 on a play-selection bucket, +/-0.9 yd on deep air yards). See
    docs/audit/2026_09_audit/phase_1_sim_core.md and the identical handling
    already in models/chaos_v_0_1_0/inference.py for Gate 2b / Gate 4.
    """
    best_it = getattr(loaded_model, "best_iteration", None)
    if best_it is None:
        booster = loaded_model.get_booster() if hasattr(loaded_model, "get_booster") else loaded_model
        raw = booster.attributes().get("best_iteration")
        best_it = int(raw) if raw is not None else None
    return (0, best_it + 1) if best_it is not None else (0, 0)


class NameCentralizer:
    """
    Standardizes NFL personnel names.
    - Players: 'F.LastName'
    - Coaches: 'Full Name'
    """
    @staticmethod
    def standardize_player(name):
        if not name or name == "Unknown": return "Unknown"
        
        # Remove suffixes
        name = re.sub(r'\s+(Jr\.|Sr\.|III|II|IV|V)$', '', name, flags=re.IGNORECASE)
        
        # Handle F.LastName (A.Rodgers, A.St. Brown)
        # If it already looks like F.LastName, just clean whitespace and normalize case
        if re.match(r'^[A-Z]\..+', name):
            parts = name.split('.', 1) # Split on first dot only
            initial = parts[0].strip().upper()
            last = parts[1].strip()
            # If last name has dots (St. Brown), keep them but capitalize parts
            last = " ".join([p.capitalize() for p in last.split()])
            return f"{initial}.{last}"
            
        # Handle Full Names (Patrick Mahomes)
        name = name.replace('.', '').strip()
        parts = name.split()
        if len(parts) >= 2:
            return f"{parts[0][0].upper()}.{parts[-1].capitalize()}"
        
        return name.capitalize()

    @staticmethod
    def standardize_coach(name):
        if not name or name == "Unknown": return "Unknown"
        name = re.sub(r'\s+(Jr\.|Sr\.|III|II|IV|V)$', '', name, flags=re.IGNORECASE)
        name = name.replace('.', '').strip()
        parts = name.split()
        return " ".join([p.capitalize() for p in parts])
