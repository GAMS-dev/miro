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

    def nested_patch_release(self):
        return (
            "Release notes\n=============\n\n"
            "GAMS MIRO 2.14\n--------------\n\n(2026-11-01)\n\n"
            "GAMS MIRO 2.14.1\n~~~~~~~~~~~~~~~~\n\n(xxxx-xx-xx)\n\nPatch notes.\n\n"
            "GAMS MIRO 2.14.0\n~~~~~~~~~~~~~~~~\n\n(2026-11-01)\n\nOriginal notes.\n\n"
            + self.history
        )

    def test_nested_patch_updates_only_its_date_and_is_idempotent(self):
        for newline in ("\n", "\r\n"):
            with self.subTest(newline=repr(newline)):
                original = self.nested_patch_release().replace("\n", newline).encode()
                self.release.write_bytes(original)
                self.update("2.14.1")
                expected = original.replace(b"(xxxx-xx-xx)", b"(2026-11-04)")
                self.assertEqual(self.release.read_bytes(), expected)
                self.assertIn("Get GAMS MIRO 2.14.1", self.index.read_text())
                before = (self.release.read_bytes(), self.index.read_bytes())
                self.update("2.14.1")
                self.assertEqual(before, (self.release.read_bytes(), self.index.read_bytes()))

    def test_accepts_initial_prerelease_under_minor_heading(self):
        self.update("2.14.0-rc.1")
        self.assertIn("Get GAMS MIRO 2.14.0-rc.1", self.index.read_text())

    def test_nested_patch_rejects_redating(self):
        self.release.write_text(self.nested_patch_release())
        self.update("2.14.1")
        self.assert_rejected_unchanged("already dated", version="2.14.1", release_date="2026-11-05")

    def test_rejects_patch_without_its_own_entry(self):
        self.assert_rejected_unchanged("add a patch entry", version="2.14.1")

    def test_rejects_wrong_patch_even_when_requested_patch_exists_later(self):
        original = self.nested_patch_release()
        self.release.write_text(original.replace(
            "GAMS MIRO 2.14.1", "GAMS MIRO 2.14.2\n~~~~~~~~~~~~~~~~\n\n(2026-11-05)\n\nGAMS MIRO 2.14.1"
        ))
        self.assert_rejected_unchanged("Latest release heading", version="2.14.1")

    def test_nested_patch_rejects_unresolved_parent_date(self):
        self.release.write_text(self.nested_patch_release().replace("(2026-11-01)", "(xxxx-xx-xx)", 1))
        self.assert_rejected_unchanged("Older release", version="2.14.1")

    def test_nested_patch_requires_its_own_date(self):
        self.release.write_text(self.nested_patch_release().replace("(xxxx-xx-xx)", "No date"))
        self.assert_rejected_unchanged("Latest release must have a date", version="2.14.1")

    def test_html_validation_handles_main_and_nested_patch_releases(self):
        html = self.source / "release.html"
        for heading in ("GAMS MIRO 2.14", "GAMS MIRO 2.14.0"):
            html.write_text(f'<h2>{heading}<a class="headerlink" href="#main">¶</a></h2><p>(2026-11-01)</p>')
            module.validate_release_html(html, "2.14.0", "2026-11-01")
        html.write_text(
            '<nav><a href="#old">GAMS MIRO 2.13.5</a></nav>'
            '<h2>GAMS MIRO 2.14<a class="headerlink" href="#main">¶</a></h2><p>(2026-11-01)</p>'
            '<h3>GAMS MIRO 2.14.1<a class="headerlink" href="#patch">#</a></h3>\n<p>(2026-11-04)</p>'
            '<h3>Bug fixes</h3><p>Details.</p>'
            '<h3>GAMS MIRO 2.14.0</h3><p>(2026-11-01)</p>'
            '<h2>GAMS MIRO 2.13</h2><p>(2025-09-25)</p>'
        )
        module.validate_release_html(html, "2.14.1", "2026-11-04")
        for version, release_date, message in (
            ("2.14.1", "2026-11-01", "Current release date"),
            ("2.14.2", "2026-11-04", "Latest release heading"),
            ("2.14.0", "2026-11-01", "Latest release heading"),
            ("2.13.5", "2025-09-25", "Latest release heading"),
        ):
            with self.subTest(version=version, release_date=release_date):
                with self.assertRaisesRegex(ValueError, message):
                    module.validate_release_html(html, version, release_date)

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
