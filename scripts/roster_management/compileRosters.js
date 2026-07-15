const fs = require('fs');
const path = require('path');

const rostersDir = path.join(__dirname, '..', 'data', 'current_rosters');
const outputFilePath = path.join(__dirname, '..', 'frontend', 'src', 'allRosters.js');

function calculateDfsSalary(pos, targetShare, carryShare) {
  const base = 3000;
  if (pos === 'QB') {
    return 6500;
  } else if (pos === 'RB') {
    return Math.max(3000, Math.min(9500, Math.round(base + (carryShare * 9000))));
  } else if (pos === 'WR' || pos === 'TE' || pos === 'WR/TE') {
    return Math.max(3000, Math.min(9500, Math.round(base + (targetShare * 18000))));
  }
  return base;
}

const compiled = {};

const files = fs.readdirSync(rostersDir);
files.forEach(file => {
  if (file.endsWith('.json') && file.includes('_traits_2025')) {
    const team = file.split('_')[0]; // e.g. "ARI"
    const filePath = path.join(rostersDir, file);
    const content = JSON.parse(fs.readFileSync(filePath, 'utf8'));
    
    const rosterList = [];
    const traits = content.traits || {};
    
    Object.keys(traits).forEach(name => {
      const p = traits[name];
      const pos = p.pos || 'WR/TE';
      const targetShare = p.target_share || 0.0;
      const carryShare = p.carry_share || 0.0;
      const salary = calculateDfsSalary(pos, targetShare, carryShare);
      
      rosterList.push({
        name: name,
        pos: pos,
        target_share: Math.round(targetShare * 1000) / 10,
        carry_share: Math.round(carryShare * 1000) / 10,
        catch_rate: 70.0, // Default baseline
        td_share: Math.round((targetShare + carryShare) * 1000) / 10,
        salary: salary
      });
    });
    
    // Sort descending by salary
    rosterList.sort((a, b) => b.salary - a.salary);
    
    compiled[team] = {
      team_settings: {
        coach: 'Head Coach',
        plays_per_game: 64.0,
        def_pressure_rate: 0.30,
        proe: 0.0,
        deep_shot_rate: 0.12,
        conservative_score_bias: 0.0
      },
      roster: rosterList
    };
  }
});

// Also duplicate LAR to LA and vice versa
if (compiled.LA && !compiled.LAR) compiled.LAR = compiled.LA;
if (compiled.LAR && !compiled.LA) compiled.LA = compiled.LAR;

const fileContent = `// Generated automatically from data/current_rosters json files.
export const ALL_ROSTERS = ${JSON.stringify(compiled, null, 2)};
`;

fs.writeFileSync(outputFilePath, fileContent, 'utf8');
console.log('Successfully compiled all rosters into allRosters.js!');
