"""Regression checks for persistent release dates in the documentation sources."""

import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "update_docs_release", Path(__file__).resolve().parents[1] / "update_docs_release.py"
)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class ReleaseSourcesTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.source = Path(self.directory.name)
        self.release = self.source / "release.rst"
        self.index = self.source / "index.rst"
        self.history = "GAMS MIRO 2.13\n--------------\n\n(2026-01-15)\n\nHistorical notes.\n"
        self.release.write_text(
            "Release notes\n=============\n\n.. _v214:\n\n"
            "GAMS MIRO 2.14\n--------------\n\n(xxxx-xx-xx)\n\nNew notes.\n\n" + self.history
        )
        self.index.write_text(
            ".. button-link:: download.html\n   :color: primary\n\n   Get GAMS MIRO 2.13.5\n"
        )

    def update(self, version="2.14.0", release_date="2026-11-04"):
        module.update_release_sources(self.source, version, release_date)

    def assert_rejected_unchanged(self, message, **kwargs):
        before = (self.release.read_bytes(), self.index.read_bytes())
        with self.assertRaisesRegex(ValueError, message):
            self.update(**kwargs)
        self.assertEqual(before, (self.release.read_bytes(), self.index.read_bytes()))

    def test_freezes_only_latest_date_and_updates_version(self):
        self.update()
        self.assertIn("(2026-11-04)", self.release.read_text())
        self.assertTrue(self.release.read_text().endswith(self.history))
        self.assertIn("Get GAMS MIRO 2.14.0", self.index.read_text())
        before = (self.release.read_bytes(), self.index.read_bytes())
        self.update()
        self.assertEqual(before, (self.release.read_bytes(), self.index.read_bytes()))

    def test_next_release_preserves_previously_published_date(self):
        self.update()
        previous = self.release.read_bytes()
        self.release.write_bytes(b"Release notes\n=============\n\nGAMS MIRO 2.15\n--------------\n\n(xxxx-xx-xx)\n\n" + previous)
        self.update("2.15.0", "2027-01-10")
        self.assertIn(b"(2027-01-10)", self.release.read_bytes())
        self.assertTrue(self.release.read_bytes().endswith(previous))

    def test_preserves_crlf(self):
        for path in (self.release, self.index):
            path.write_bytes(path.read_bytes().replace(b"\n", b"\r\n"))
        self.update()
        for path in (self.release, self.index):
            self.assertNotIn(b"\n", path.read_bytes().replace(b"\r\n", b""))

    def test_accepts_exact_patch_release_heading(self):
        self.release.write_text(self.release.read_text().replace("GAMS MIRO 2.14\n--------------", "GAMS MIRO 2.14.1\n----------------"))
        self.update("2.14.1")
        self.assertIn("Get GAMS MIRO 2.14.1", self.index.read_text())

    def test_rejects_wrong_latest_version_even_if_target_exists_later(self):
        self.release.write_text("GAMS MIRO 2.15\n--------------\n\n(xxxx-xx-xx)\n\n" + self.release.read_text())
        self.assert_rejected_unchanged("Latest release heading")

    def test_rejects_historical_placeholder(self):
        self.release.write_text(self.release.read_text().replace("(2026-01-15)", "(xxxx-xx-xx)"))
        self.assert_rejected_unchanged("Older release")

    def test_rejects_changing_existing_release_date(self):
        self.update()
        self.assert_rejected_unchanged("already dated", release_date="2026-11-05")

    def test_rejects_missing_latest_date(self):
        self.release.write_text(self.release.read_text().replace("(xxxx-xx-xx)", "Missing date"))
        self.assert_rejected_unchanged("Latest release must have a date")

    def test_index_validation_does_not_partially_update_release(self):
        self.index.write_text("Get GAMS MIRO\n")
        self.assert_rejected_unchanged("exactly one")
        self.index.write_text("   Get GAMS MIRO 2.13.5\n   Get GAMS MIRO 2.12.0\n")
        self.assert_rejected_unchanged("exactly one")

    def test_rejects_invalid_release_metadata(self):
        self.assert_rejected_unchanged("Invalid MIRO version", version="bad")
        self.assert_rejected_unchanged("day is out of range", release_date="2026-02-30")


if __name__ == "__main__":
    unittest.main()
