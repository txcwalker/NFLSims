import { useState } from 'react';

/**
 * 3-tab save-slot switcher UI, paired with the useWorkspaceSlots hook.
 * Presentational only -- all persistence lives in the hook.
 *
 * Props: slots (from useWorkspaceSlots), active, saveStatus,
 * onSwitch(slot), onRename(slot,label), onClear(slot).
 */
export default function SlotSwitcher({ slots, active, saveStatus, onSwitch, onRename, onClear }) {
  const [editing, setEditing] = useState(null); // slot number being renamed, or null
  const [draft, setDraft] = useState('');
  const [confirmClear, setConfirmClear] = useState(null); // slot number pending clear confirm, or null

  const startEdit = (s) => { setEditing(s.slot); setDraft(s.label); setConfirmClear(null); };
  const commitEdit = () => {
    if (editing != null) onRename(editing, draft.trim() || `Slot ${editing}`);
    setEditing(null);
  };

  const statusText = { saving: 'saving…', saved: 'saved', error: 'save failed', idle: '' }[saveStatus] || '';
  const statusColor = { saving: 'var(--text-muted)', saved: 'var(--accent-green, #22c55e)', error: 'var(--accent-red, #ef4444)', idle: 'var(--text-muted)' }[saveStatus];

  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: '6px', flexWrap: 'wrap' }}>
      {slots.map(s => {
        const isActive = s.slot === active;
        return (
          <div key={s.slot} style={{ position: 'relative', display: 'flex', alignItems: 'center' }}>
            {editing === s.slot ? (
              <input
                autoFocus value={draft} onChange={e => setDraft(e.target.value)}
                onBlur={commitEdit} onKeyDown={e => { if (e.key === 'Enter') commitEdit(); if (e.key === 'Escape') setEditing(null); }}
                style={{ width: '92px', fontSize: '0.78rem', padding: '5px 8px', borderRadius: '7px', border: '1px solid rgba(0,242,254,0.4)', background: 'rgba(0,0,0,0.3)', color: 'var(--text-white)' }}
              />
            ) : (
              <button
                onClick={() => (isActive ? startEdit(s) : onSwitch(s.slot))}
                onDoubleClick={() => startEdit(s)}
                title={isActive ? 'Click to rename this slot' : `Switch to ${s.label}`}
                style={{
                  padding: '5px 12px', borderRadius: '7px', fontSize: '0.78rem', fontWeight: 600, cursor: 'pointer',
                  border: `1px solid ${isActive ? 'rgba(0,242,254,0.45)' : 'rgba(255,255,255,0.12)'}`,
                  background: isActive ? 'rgba(0,242,254,0.14)' : 'rgba(255,255,255,0.04)',
                  color: isActive ? 'var(--accent-primary)' : 'var(--text-muted)',
                  display: 'flex', alignItems: 'center', gap: '5px',
                }}
              >
                {s.has_data && <span style={{ width: '5px', height: '5px', borderRadius: '50%', background: isActive ? 'var(--accent-primary)' : 'var(--text-muted)', display: 'inline-block' }} />}
                {s.label}
              </button>
            )}
            {isActive && editing !== s.slot && (
              confirmClear === s.slot ? (
                <span style={{ display: 'flex', gap: '3px', marginLeft: '4px' }}>
                  <button onClick={() => { onClear(s.slot); setConfirmClear(null); }}
                    title="Wipe this slot's saved data"
                    style={{ fontSize: '0.68rem', padding: '3px 6px', borderRadius: '5px', border: '1px solid rgba(239,68,68,0.4)', background: 'rgba(239,68,68,0.12)', color: '#ef4444', cursor: 'pointer' }}>
                    confirm
                  </button>
                  <button onClick={() => setConfirmClear(null)}
                    style={{ fontSize: '0.68rem', padding: '3px 6px', borderRadius: '5px', border: '1px solid rgba(255,255,255,0.12)', background: 'none', color: 'var(--text-muted)', cursor: 'pointer' }}>
                    ✕
                  </button>
                </span>
              ) : s.has_data && (
                <button onClick={() => setConfirmClear(s.slot)} title="Start fresh — wipe this slot"
                  style={{ marginLeft: '3px', fontSize: '0.7rem', padding: '3px 6px', borderRadius: '5px', border: '1px solid rgba(255,255,255,0.1)', background: 'none', color: 'var(--text-muted)', cursor: 'pointer' }}>
                  ↺
                </button>
              )
            )}
          </div>
        );
      })}
      {statusText && <span style={{ fontSize: '0.7rem', color: statusColor, marginLeft: '4px' }}>{statusText}</span>}
    </div>
  );
}
