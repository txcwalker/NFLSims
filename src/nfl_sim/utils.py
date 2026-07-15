import re

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
