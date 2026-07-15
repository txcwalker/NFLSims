import React from 'react';

export function SandboxBadge({ reason }) {
  return (
    <div style={{
      display: 'inline-flex',
      alignItems: 'center',
      gap: '6px',
      fontSize: '0.75rem',
      fontWeight: 700,
      padding: '4px 10px',
      borderRadius: '6px',
      background: 'rgba(245, 158, 11, 0.12)',
      color: '#f59e0b',
      border: '1px solid rgba(245, 158, 11, 0.25)',
      width: 'fit-content'
    }}
    title={reason || "Using estimated mock sandbox data due to backend connectivity issues"}
    >
      <span style={{
        width: '6px',
        height: '6px',
        borderRadius: '50%',
        background: '#f59e0b',
        display: 'inline-block'
      }}></span>
      <span>Sandbox Fallback</span>
    </div>
  );
}
