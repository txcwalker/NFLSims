import { useCallback, useEffect, useRef, useState } from 'react';
import { ApiService } from '../api';

const AUTOSAVE_DEBOUNCE_MS = 1200;

/**
 * 3 switchable, autosaved workspace "save slots" for one slate (see
 * workspace_store.py) -- Cam can keep 2-3 different takes on the same slate
 * (chalk build / contrarian build / stack build) side by side, and whichever
 * is active survives a page or tab switch.
 *
 * State-shape agnostic: the caller hands over a `snapshot` (recompute it with
 * useMemo from whatever local state should persist) and an `onHydrate(data)`
 * callback that applies a loaded blob back onto that same local state -- this
 * hook only fetches, debounces, and autosaves; it never interprets the blob.
 *
 * Inputs
 *   slateKey  - opaque string identifying this slate/game within the week
 *               (e.g. `showdown_${game_id}`, `classic_${platform}_${draftGroupId}`).
 *               Pass null/'' to disable (e.g. no game picked yet).
 *   week, season - workspace context (mirrors optimizer_store's keying).
 *   snapshot  - the current data to autosave; a NEW reference on every render
 *               where something persist-worthy changed (useMemo it).
 *   onHydrate - (data) => void, called after mount and after every slot
 *               switch with that slot's saved `data` (or {} if never saved).
 *
 * Outputs
 *   { slots, active, ready, saveStatus, switchSlot(n), renameSlot(n,label),
 *     clearSlot(n) } -- `ready` guards the UI until the initial hydrate lands
 *   so a snapshot computed from still-default local state doesn't overwrite
 *   a real save before it's even been read.
 *
 * Purpose
 *   The "save file" half of the Explore/Build/Evaluate workspace: keeps a
 *   working session alive across navigation without a database, same
 *   file-based pattern as optimizer_store's per-week state/builds.
 */
export function useWorkspaceSlots(slateKey, week, season, snapshot, onHydrate) {
  const [slots, setSlots] = useState([1, 2, 3].map(slot => ({ slot, label: `Slot ${slot}`, updated_at: null, has_data: false })));
  const [active, setActive] = useState(1);
  const [ready, setReady] = useState(false);
  const [saveStatus, setSaveStatus] = useState('idle'); // 'idle' | 'saving' | 'saved' | 'error'

  // Refs mirroring the latest callback/value for use inside async callbacks
  // (effects/timeouts) without retriggering them -- assigned in their own
  // effect, never during render (React flags a direct render-time ref write).
  const onHydrateRef = useRef(onHydrate);
  useEffect(() => { onHydrateRef.current = onHydrate; }, [onHydrate]);
  const activeRef = useRef(1);
  useEffect(() => { activeRef.current = active; }, [active]);

  const suppressSaveRef = useRef(false); // true while a load is in flight/just landed
  const saveTimerRef = useRef(null);

  const loadSlot = useCallback(async (slotNum) => {
    suppressSaveRef.current = true;
    const res = await ApiService.getWorkspaceSlot(slateKey, slotNum, week, season);
    onHydrateRef.current(res?.data || {});
    // Let the hydrate's own state updates flush and re-render (and this
    // hook's snapshot-effect re-derive) before re-arming autosave, or the
    // just-loaded data would immediately "change" and re-save itself.
    setTimeout(() => { suppressSaveRef.current = false; }, 0);
  }, [slateKey, week, season]);

  // Initial load: slot metadata + whichever slot is active, for this slate.
  // Every setState here runs inside the microtask's `.then`, not the
  // synchronous effect body, so a slateKey change can't cascade a render
  // during the effect pass itself.
  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(async () => {
      if (cancelled) return;
      if (!slateKey) { setReady(false); return; }
      setReady(false);
      const meta = await ApiService.getWorkspaceSlots(slateKey, week, season);
      if (cancelled) return;
      setSlots(meta.slots || []);
      setActive(meta.active || 1);
      await loadSlot(meta.active || 1);
      if (!cancelled) setReady(true);
    });
    return () => { cancelled = true; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [slateKey, week, season]);

  // Debounced autosave into the active slot whenever `snapshot` changes.
  useEffect(() => {
    if (!slateKey || !ready || suppressSaveRef.current) return;
    if (saveTimerRef.current) clearTimeout(saveTimerRef.current);
    saveTimerRef.current = setTimeout(async () => {
      setSaveStatus('saving');
      const ok = await ApiService.putWorkspaceSlot(slateKey, activeRef.current, snapshot, week, season);
      setSaveStatus(ok ? 'saved' : 'error');
      if (ok) {
        setSlots(rows => rows.map(r => r.slot === activeRef.current ? { ...r, updated_at: new Date().toISOString(), has_data: true } : r));
      }
    }, AUTOSAVE_DEBOUNCE_MS);
    return () => { if (saveTimerRef.current) clearTimeout(saveTimerRef.current); };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [snapshot, slateKey, ready]);

  const switchSlot = useCallback(async (slotNum) => {
    if (slotNum === activeRef.current || !slateKey) return;
    if (saveTimerRef.current) clearTimeout(saveTimerRef.current);
    setActive(slotNum);
    await ApiService.setWorkspaceActive(slateKey, slotNum, week, season);
    await loadSlot(slotNum);
  }, [slateKey, week, season, loadSlot]);

  const renameSlot = useCallback(async (slotNum, label) => {
    setSlots(rows => rows.map(r => r.slot === slotNum ? { ...r, label } : r));
    if (slateKey) await ApiService.renameWorkspaceSlot(slateKey, slotNum, label, week, season);
  }, [slateKey, week, season]);

  const clearSlot = useCallback(async (slotNum) => {
    if (!slateKey) return;
    await ApiService.clearWorkspaceSlot(slateKey, slotNum, week, season);
    setSlots(rows => rows.map(r => r.slot === slotNum ? { ...r, has_data: false, updated_at: new Date().toISOString() } : r));
    if (slotNum === activeRef.current) await loadSlot(slotNum);
  }, [slateKey, week, season, loadSlot]);

  return { slots, active, ready, saveStatus, switchSlot, renameSlot, clearSlot };
}
