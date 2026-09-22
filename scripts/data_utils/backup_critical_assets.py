"""Backs up the repo's un-regeneratable, gitignored training assets to Backblaze B2.

Inputs:
    - Local files: `data/processed/hardened_pass_training_master_v2_5.csv`
      (the pass-model training master, 308 MB, no committed builder script)
      and the R model train/val/test splits in `data/win_probability/`,
      `data/field_goal/`, `data/fourth_down/` (feature_names/train/test/valid
      .rds files). All are gitignored -- see .gitignore -- because of size,
      not because they're regeneratable; nothing else in the repo can rebuild
      them (audit S2-6, 2026-09).
    - Env vars: B2_APPLICATION_KEY_ID, B2_APPLICATION_KEY, B2_BUCKET_NAME
      (a Backblaze B2 account + bucket Cam creates and owns -- this script
      never creates the account or bucket itself).

Outputs:
    - Uploads each file to the B2 bucket under the same relative path it has
      in the repo (e.g. `data/processed/hardened_pass_training_master_v2_5.csv`).
      Skips a file whose remote SHA1 already matches the local one, so a
      no-op run after the first is cheap even for the 308 MB file.
    - Prints a per-file uploaded/skipped/missing summary; exits non-zero if
      any required file is missing locally or any upload fails.

Purpose:
    These files are the last remaining un-backed-up single point of failure
    the audit found -- a disk failure or a bad OneDrive-conflict overwrite
    (see A1/S2-5) would make 4 model families + the pass pipeline
    unrebuildable with no recovery path. Run this after any material change
    to one of the tracked paths (e.g. a `hardened_pass_training_master`
    rebuild, or a retrain of the win_probability/field_goal/fourth_down
    R-era models). Not scheduled/automated -- run it by hand.

Usage:
    B2_APPLICATION_KEY_ID=... B2_APPLICATION_KEY=... B2_BUCKET_NAME=... \\
        venv/Scripts/python.exe scripts/data_utils/backup_critical_assets.py
    (add --dry-run to preview without uploading)
"""
import argparse
import hashlib
import os
import sys

CRITICAL_PATHS = [
    "data/processed/hardened_pass_training_master_v2_5.csv",
    "data/win_probability/feature_names.rds",
    "data/win_probability/train.rds",
    "data/win_probability/valid.rds",
    "data/win_probability/test.rds",
    "data/field_goal/feature_names.rds",
    "data/field_goal/train.rds",
    "data/field_goal/valid.rds",
    "data/field_goal/test.rds",
    "data/fourth_down/feature_names.rds",
    "data/fourth_down/train.rds",
    "data/fourth_down/valid.rds",
    "data/fourth_down/test.rds",
]

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def local_sha1(path, chunk_size=8 * 1024 * 1024):
    h = hashlib.sha1()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(chunk_size), b""):
            h.update(chunk)
    return h.hexdigest()


def get_bucket():
    from b2sdk.v2 import B2Api, InMemoryAccountInfo

    key_id = os.environ.get("B2_APPLICATION_KEY_ID")
    key = os.environ.get("B2_APPLICATION_KEY")
    bucket_name = os.environ.get("B2_BUCKET_NAME")
    missing = [n for n, v in [
        ("B2_APPLICATION_KEY_ID", key_id),
        ("B2_APPLICATION_KEY", key),
        ("B2_BUCKET_NAME", bucket_name),
    ] if not v]
    if missing:
        print(f"Missing required env var(s): {', '.join(missing)}", file=sys.stderr)
        print("Set these to a Backblaze B2 application key + bucket you control.", file=sys.stderr)
        sys.exit(1)

    api = B2Api(InMemoryAccountInfo())
    api.authorize_account("production", key_id, key)
    return api.get_bucket_by_name(bucket_name)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dry-run", action="store_true", help="Preview without uploading")
    args = parser.parse_args()

    missing_local = [p for p in CRITICAL_PATHS if not os.path.isfile(os.path.join(REPO_ROOT, p))]
    if missing_local:
        print("Missing locally (not backed up, needs investigation):")
        for p in missing_local:
            print(f"  - {p}")

    present = [p for p in CRITICAL_PATHS if p not in missing_local]
    if not present:
        print("Nothing to back up -- no critical files found locally.")
        sys.exit(1 if missing_local else 0)

    bucket = None if args.dry_run else get_bucket()

    uploaded, skipped, failed = [], [], []
    for rel_path in present:
        local_path = os.path.join(REPO_ROOT, rel_path)
        remote_name = rel_path.replace("\\", "/")
        size_mb = os.path.getsize(local_path) / (1024 * 1024)

        if args.dry_run:
            print(f"[dry-run] would check/upload {remote_name} ({size_mb:.1f} MB)")
            continue

        sha1 = local_sha1(local_path)
        try:
            existing = bucket.get_file_info_by_name(remote_name)
            if existing.content_sha1 == sha1:
                print(f"SKIP  (unchanged, {size_mb:.1f} MB): {remote_name}")
                skipped.append(remote_name)
                continue
        except Exception:
            pass  # not present remotely yet -- fall through to upload

        try:
            bucket.upload_local_file(local_file=local_path, file_name=remote_name, sha1_sum=sha1)
            print(f"UPLOAD ({size_mb:.1f} MB): {remote_name}")
            uploaded.append(remote_name)
        except Exception as e:
            print(f"FAILED: {remote_name} -- {e}", file=sys.stderr)
            failed.append(remote_name)

    if args.dry_run:
        return

    print(f"\n{len(uploaded)} uploaded, {len(skipped)} unchanged, {len(failed)} failed, "
          f"{len(missing_local)} missing locally.")
    if failed or missing_local:
        sys.exit(1)


if __name__ == "__main__":
    main()
