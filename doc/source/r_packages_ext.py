"""Generates the "R packages" page: the R version and R packages shipped with each release.

Each release freezes its list as r_packages/<version>.csv, named after its heading in
release.rst (see scripts/update_docs_release.py).
While the newest release in release.rst is still undated, the current list from the sources
is shown for it as well.
"""

import csv
import json
from pathlib import Path
import re

SOURCE_DIR = Path(__file__).resolve().parent
REPO_DIR = SOURCE_DIR.parents[1]
SNAPSHOT_DIR = SOURCE_DIR / "r_packages"


def current_packages():
    """R and the R packages the current sources ship, as (name, version) pairs."""
    r_version = json.loads((REPO_DIR / "build-config.json").read_text(encoding="utf-8"))["rVersion"]
    packages = []
    with open(REPO_DIR / "src" / "miro-pkg-lock.csv", newline="", encoding="utf-8") as f:
        for row in csv.reader(f):
            if len(row) > 1:
                packages.append((row[0].strip(), row[1].strip()))
            elif row:  # built from source in r-src/
                name = row[0].strip()
                description = (REPO_DIR / "r-src" / name / "DESCRIPTION").read_text(encoding="utf-8")
                packages.append((name, re.search(r"^Version:\s*(\S+)", description, re.MULTILINE)[1]))
    return [("R", r_version)] + sorted(packages, key=lambda p: p[0].lower())


def write_snapshot(version, snapshot_dir=SNAPSHOT_DIR):
    """Freeze the current list for a release."""
    snapshot_dir.mkdir(exist_ok=True)
    with open(snapshot_dir / f"{version}.csv", "w", newline="", encoding="utf-8") as f:
        csv.writer(f, lineterminator="\n").writerows(current_packages())


def render():
    def version_key(version):
        return tuple(int(x) for x in re.findall(r"\d+", version))

    releases = []
    for path in SNAPSHOT_DIR.glob("*.csv"):
        with open(path, newline="", encoding="utf-8") as f:
            releases.append((path.stem, list(csv.reader(f))))
    upcoming = re.search(r"^GAMS MIRO (\S+)\r?\n[-~]+\r?\n\s*\(xxxx-xx-xx\)",
                         (SOURCE_DIR / "release.rst").read_text(encoding="utf-8"), re.MULTILINE)
    if upcoming:
        releases.append((f"{upcoming[1]} (upcoming)", current_packages()))

    lines = []
    for version, packages in sorted(releases, key=lambda r: version_key(r[0]), reverse=True):
        title = f"GAMS MIRO {version}"
        lines += [title, "-" * len(title), "", ".. list-table::", "   :header-rows: 1", "",
                  "   * - Package", "     - Version"]
        for name, package_version in packages:
            lines += [f"   * - {name}", f"     - {package_version}"]
        lines.append("")
    return "\n".join(lines)


def add_package_lists(app, docname, source):
    if docname == "r_packages":
        source[0] += "\n" + render()
        # rebuild the page when the lists change, not only when r_packages.rst does
        for path in [REPO_DIR / "build-config.json", REPO_DIR / "src" / "miro-pkg-lock.csv",
                     *SNAPSHOT_DIR.glob("*.csv")]:
            app.env.note_dependency(str(path))


def setup(app):
    app.connect("source-read", add_package_lists)
    return {"parallel_read_safe": True}
