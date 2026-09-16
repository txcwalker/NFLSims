"""File-backed store for bankroll-tracking accounts (paper and real).

A build (see optimizer_store.py) or a paper entry (see paper_store.py) tags
itself with one `account_id` from here so the Bankroll page can roll up
cost/winnings per account, across weeks. Single user, no auth, no database --
same atomic-write pattern as optimizer_store.py / paper_store.py.

Layout::

    data/bankroll/accounts.json
        {"accounts": [
            {"account_id": "paper_baseline", "label": "Baseline (untouched)",
             "kind": "paper_baseline", "starting_bankroll": 0.0,
             "created_at": "..."},
            ...
        ]}

`kind` drives behavior elsewhere (e.g. whether a build tagged to it is
eligible for the real-money "submitted" flag): one of "paper_baseline",
"paper_catered", "real". `account_id` is the stable key everything else
references; multiple accounts CAN share a `kind` (e.g. a second real-money
account on a different DK login) -- `kind` is a category, not a uniqueness
constraint.
"""
import datetime
import json
import os
import re
import tempfile
from typing import Any, Dict, List, Optional

_REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_PATH = os.path.join(_REPO_ROOT, "data", "bankroll", "accounts.json")

_ID_RE = re.compile(r"[^a-z0-9_]")

VALID_KINDS = ("paper_baseline", "paper_catered", "real")

_DEFAULT_ACCOUNTS = [
    {"label": "Baseline (untouched optimizer)", "kind": "paper_baseline", "starting_bankroll": 0.0},
    {"label": "Catered pool + adjustments", "kind": "paper_catered", "starting_bankroll": 0.0},
    {"label": "Real (actually entered)", "kind": "real", "starting_bankroll": 0.0},
]


def _atomic_write_json(path: str, data: Any) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(path), suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
        os.replace(tmp, path)
    except Exception:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def _slugify(label: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "_", label.strip().lower()).strip("_")
    return slug or "account"


def _read_all() -> Dict[str, Any]:
    if not os.path.exists(_PATH):
        now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
        doc = {"accounts": [
            {"account_id": a["kind"], "label": a["label"], "kind": a["kind"],
             "starting_bankroll": a["starting_bankroll"], "created_at": now}
            for a in _DEFAULT_ACCOUNTS
        ]}
        _atomic_write_json(_PATH, doc)
        return doc
    try:
        with open(_PATH, "r", encoding="utf-8") as f:
            doc = json.load(f)
        if not isinstance(doc, dict) or not isinstance(doc.get("accounts"), list):
            return {"accounts": []}
        return doc
    except (json.JSONDecodeError, OSError):
        return {"accounts": []}


def list_accounts() -> List[Dict[str, Any]]:
    return _read_all()["accounts"]


def get_account(account_id: str) -> Optional[Dict[str, Any]]:
    for a in list_accounts():
        if a.get("account_id") == account_id:
            return a
    return None


def create_account(label: str, kind: str, starting_bankroll: float = 0.0) -> Dict[str, Any]:
    if kind not in VALID_KINDS:
        raise ValueError(f"kind must be one of {VALID_KINDS}, got {kind!r}")
    doc = _read_all()
    existing_ids = {a["account_id"] for a in doc["accounts"]}
    base_id = _ID_RE.sub("", _slugify(label))
    account_id = base_id
    n = 2
    while account_id in existing_ids:
        account_id = f"{base_id}_{n}"
        n += 1
    record = {
        "account_id": account_id,
        "label": label,
        "kind": kind,
        "starting_bankroll": float(starting_bankroll),
        "created_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    }
    doc["accounts"].append(record)
    _atomic_write_json(_PATH, doc)
    return record


def patch_account(account_id: str, patch: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Update label / starting_bankroll only -- kind and account_id are fixed
    once created (retagging kind would silently reinterpret every build/entry
    already pointed at this account)."""
    doc = _read_all()
    for a in doc["accounts"]:
        if a.get("account_id") == account_id:
            for k in ("label", "starting_bankroll"):
                if k in patch:
                    a[k] = patch[k]
            _atomic_write_json(_PATH, doc)
            return a
    return None


def delete_account(account_id: str) -> bool:
    doc = _read_all()
    before = len(doc["accounts"])
    doc["accounts"] = [a for a in doc["accounts"] if a.get("account_id") != account_id]
    if len(doc["accounts"]) == before:
        return False
    _atomic_write_json(_PATH, doc)
    return True
