"""Persist the current documentation release metadata before the Sphinx build."""

import argparse
from datetime import date
from html.parser import HTMLParser
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "doc" / "source"))
from r_packages_ext import write_snapshot  # noqa: E402


def select_latest_release(headings, version):
    """Select the current entry, including a patch below its minor release."""
    short_version = ".".join(version.split(".")[:2])
    if not headings or headings[0][1] != 2 or headings[0][0] not in (version, short_version):
        raise ValueError(f"Latest release heading must match MIRO {version}")
    # Patch releases are listed newest first within the current minor release.
    if len(headings) > 1 and headings[1][1] == 3:
        if headings[1][0] != version:
            raise ValueError(f"Latest release heading must match MIRO {version}")
        return 1
    if headings[0][0] != version and not re.fullmatch(
        re.escape(short_version) + r"\.0(?:[-+][A-Za-z0-9.-]+)?", version
    ):
        raise ValueError(f"Latest release heading must match MIRO {version}; add a patch entry")
    return 0


class ReleaseHTMLParser(HTMLParser):
    """Read release headings and their first paragraph, excluding TOC links."""

    def __init__(self):
        super().__init__()
        self.entries = []
        self.capture = None
        self.text = []
        self.awaiting_date = False
        self.in_headerlink = False

    def handle_starttag(self, tag, attrs):
        if tag == "a" and "headerlink" in dict(attrs).get("class", "").split():
            self.in_headerlink = True
        if tag in ("h2", "h3"):
            self.capture = tag
            self.text = []
            self.awaiting_date = False
        elif tag == "p" and self.awaiting_date:
            self.capture = tag
            self.text = []

    def handle_data(self, data):
        if self.capture and not self.in_headerlink:
            self.text.append(data)

    def handle_endtag(self, tag):
        if tag == "a":
            self.in_headerlink = False
        if tag != self.capture:
            return
        text = "".join(self.text).strip()
        self.capture = None
        if tag in ("h2", "h3"):
            match = re.fullmatch(r"GAMS MIRO (\S+)", text)
            if match:
                self.entries.append([match[1], int(tag[1]), None])
                self.awaiting_date = True
        elif tag == "p":
            self.entries[-1][2] = text
            self.awaiting_date = False


def validate_release_html(html_path: Path, version: str, release_date: str) -> None:
    """Ensure the rendered current release advertises the expected date."""
    parser = ReleaseHTMLParser()
    parser.feed(html_path.read_text(encoding="utf-8"))
    selected = select_latest_release(parser.entries, version)
    if parser.entries[selected][2] != f"({release_date})":
        raise ValueError(f"Current release date {release_date} is not advertised in {html_path}")


def update_release_sources(source_dir: Path, version: str, release_date: str) -> None:
    """Update only the latest release; leave historical dates and formatting intact."""
    if not re.fullmatch(r"\d+\.\d+\.\d+(?:[-+][A-Za-z0-9.-]+)?", version):
        raise ValueError(f"Invalid MIRO version: {version}")
    date.fromisoformat(release_date)
    release_path = source_dir / "release.rst"
    index_path = source_dir / "index.rst"
    # Preserve the checked-in newline convention, including Windows CRLF files.
    release = release_path.read_bytes().decode("utf-8")
    index = index_path.read_bytes().decode("utf-8")

    headings = list(re.finditer(
        r"^GAMS MIRO (\S+)\r?\n(-+|~+)\r?$", release, re.MULTILINE
    ))
    selected = select_latest_release(
        [(match[1], 2 if match[2].startswith("-") else 3) for match in headings], version
    )
    heading = headings[selected]
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
    write_snapshot(heading[1], source_dir / "r_packages")
    if updated_release != release:
        release_path.write_bytes(updated_release.encode("utf-8"))
    if updated_index != index:
        index_path.write_bytes(updated_index.encode("utf-8"))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, default=Path("doc/source"))
    parser.add_argument("--html-file", type=Path, help="Validate rendered release metadata instead of updating sources")
    parser.add_argument("--version", required=True)
    parser.add_argument("--date", required=True)
    args = parser.parse_args()
    try:
        if args.html_file:
            validate_release_html(args.html_file, args.version, args.date)
        else:
            update_release_sources(args.source_dir, args.version, args.date)
    except (ValueError, OSError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
