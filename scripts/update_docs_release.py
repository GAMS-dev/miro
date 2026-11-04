"""Persist the current documentation release metadata before the Sphinx build."""

import argparse
from datetime import date
from pathlib import Path
import re


def update_release_sources(source_dir: Path, version: str, release_date: str) -> None:
    """Update only the latest release; leave historical dates and formatting intact."""
    if not re.fullmatch(r"\d+\.\d+\.\d+(?:[-+][A-Za-z0-9.-]+)?", version):
        raise ValueError(f"Invalid MIRO version: {version}")
    date.fromisoformat(release_date)
    short_version = ".".join(version.split(".")[:2])
    release_path = source_dir / "release.rst"
    index_path = source_dir / "index.rst"
    # Preserve the checked-in newline convention, including Windows CRLF files.
    release = release_path.read_bytes().decode("utf-8")
    index = index_path.read_bytes().decode("utf-8")

    heading = re.search(r"^GAMS MIRO (\S+)\r?\n-+\r?$", release, re.MULTILINE)
    if heading is None or heading[1] not in (version, short_version):
        raise ValueError(f"Latest release heading must match MIRO {version}")
    release_date_match = re.match(
        r"\s*\((xxxx-xx-xx|\d{4}-\d{2}-\d{2})\)", release[heading.end():]
    )
    if release_date_match is None:
        raise ValueError("Latest release must have a date or (xxxx-xx-xx) placeholder")
    existing_date = release_date_match[1]
    if existing_date not in ("xxxx-xx-xx", release_date):
        raise ValueError(
            f"Latest release is already dated {existing_date}; refusing to overwrite it "
            f"with {release_date}. Add a new release entry instead."
        )
    start = heading.end() + release_date_match.start(1)
    end = heading.end() + release_date_match.end(1)
    updated_release = release[:start] + release_date + release[end:]
    if "(xxxx-xx-xx)" in updated_release:
        raise ValueError("Older release has an unresolved date placeholder; set its historical date explicitly")

    updated_index, count = re.subn(
        r"^([ \t]+)Get GAMS MIRO \d[^\s]*([ \t]*\r?)$",
        lambda match: f"{match[1]}Get GAMS MIRO {version}{match[2]}",
        index,
        flags=re.MULTILINE,
    )
    if count != 1:
        raise ValueError("Expected exactly one Get GAMS MIRO version label in index.rst")

    # Validate both sources before writing either one. Repeated builds are a no-op.
    if updated_release != release:
        release_path.write_bytes(updated_release.encode("utf-8"))
    if updated_index != index:
        index_path.write_bytes(updated_index.encode("utf-8"))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, default=Path("doc/source"))
    parser.add_argument("--version", required=True)
    parser.add_argument("--date", required=True)
    args = parser.parse_args()
    try:
        update_release_sources(args.source_dir, args.version, args.date)
    except (ValueError, OSError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
