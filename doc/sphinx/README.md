# MIRO Sphinx preview

This is an isolated reStructuredText foundation. All 34 pages are placeholders;
the existing HTML remains authoritative. No release or publishing scripts use
this build yet. Do not publish `source/` or this entire directory as a site.

## Build locally

Use Python 3.11 or newer. From the repository root:

```sh
python3 -m venv doc/sphinx/.venv
doc/sphinx/.venv/bin/python -m pip install -r doc/sphinx/requirements.txt
doc/sphinx/.venv/bin/python -m sphinx -b html -n -W --keep-going doc/sphinx/source doc/sphinx/_build/html
```

For subsequent builds, run just the last command. On Windows, use `python`
to create the environment and `doc/sphinx/.venv/Scripts/python.exe` for its
interpreter. Debian/Ubuntu may require the OS package `python3-venv`.

## Validate a migrated page

After migrating a page, run the migration validator with its basename:

```sh
doc/sphinx/.venv/bin/python doc/sphinx/tools/validate_migration.py charts
```

The validator runs the strict Sphinx build and compares the legacy and generated
pages for content anchors, duplicate IDs, local links and fragments, assets,
ordered image occurrences, alt text, and code examples. Missing fragments on
pages that are still explicit Sphinx placeholders are reported as future
migration information rather than failures.

Open `_build/html/index.html`, or serve it from the repository root:

```sh
doc/sphinx/.venv/bin/python -m http.server 8000 --directory doc/sphinx/_build/html
```

Then visit <http://localhost:8000/>. Build output and virtual environments are
ignored by Git. Add `-E -a` to the build command for a complete Sphinx rebuild.

## Structure and authoring

- Keep source filenames flat: `model.rst` builds to `model.html` with the HTML
  builder. Do not use `dirhtml` for the compatibility site.
- `index.rst` groups the current MIRO pages. `customize.rst` and `server.rst`
  hold the configuration and server subtrees, retaining their existing URLs.
- Published generated-page names are used: `download`, `license`,
  `license-server`, and `schema`, rather than the legacy template filenames.
- Use reStructuredText headings, explicit targets, `code-block`, admonitions,
  and `toctree`, following GAMSPy's authoring conventions. Content and legacy
  anchor migration have not started.
- `_templates/` is intentionally empty: the PyData theme supplies the layout.

## Shared assets

### Image sizing migration rule

CSS `max-height`/`max-width` constraints in the legacy documentation must not
be converted into fixed RST `:height:`/`:width:` values when that changes the
original sizing semantics. Use reusable classes in `_static/custom.css` and
apply them with the image directive's `:class:` option. Keep intrinsic aspect
ratios with automatic dimensions and constrain images to the content width.
For legacy `max-height: 400pt` screenshots, use
`:class: miro-image-max-height-400pt`; do not add a fixed `:height:` or `:width:`.

The `build-finished` hook copies `doc/assets/` to `_build/html/assets/` for HTML
builds, preserving `assets/...` URLs and relative CSS/font paths. There is no
second source asset tree or symlink, and no runtime dependency on `gamspy_docs`.
Only build output duplicates the assets. The legacy CSS and JavaScript are
copied as assets but are not loaded by the new pages.

For future native RST image directives, source images can be referenced with
`../../assets/images/example.png` (relative to a flat source page); Sphinx will
also copy referenced images to `_images`. Existing `assets/...` links remain
available through the compatibility copy. Raw HTML media can use those URLs.
Delete obsolete files from the build output before a final clean publication:
incremental asset copying does not remove files deleted from the source tree.

The MIRO logo is read directly from `doc/assets/images/logo.svg`. Only the small
shared GAMS favicon and icon were copied from GAMSPy into `_static/`.

## GAMSPy configuration assessment

Inspected `gamspy_docs/conf.py`, `README.md`, `_static/`, `_templates/`, root and
section toctrees, installation and introduction pages, and the Container API
reference. The supplied snapshot has no requirements/dependency manifest,
although its README references `requirements.txt`. The versions here are a
tested MIRO dependency set, not inferred GAMSPy version pins.

Reused:

- `pydata_sphinx_theme`, navigation/TOC depth 2, `navbar-nav`, light/dark syntax
  styles `tango`/`lightbulb`, source-link hiding, English language, build-date
  metadata, and GAMS authorship.
- Theme icon-link conventions, adapting GitHub to MIRO; shared X/GAMS links.
- `sphinx_design` for cards and tabs, `sphinx_copybutton`, `sphinx_tabs.tabs`,
  and `sphinx.ext.mathjax`. Extensions are listed once each.
- Generic CSS for icon sizing, information callouts, card images, dark-mode
  SVG card images, and transparent tab panels.
- The GAMS favicon through native `html_favicon`, avoiding an extra extension.

Deliberately omitted or adapted:

- `import gamspy`, package-derived version, and Python source-path injection.
- Autodoc, Napoleon, numpydoc, viewcode, coverage, and the API module template.
- Notebook/IPython/matplotlib integration and doctest execution. MIRO examples
  must not execute merely to build documentation.
- Intersphinx, graphviz, todo, ifconfig, and duration extensions until needed.
- Notebook table CSS and `#module-gamspy .class` styling.
- GAMSPy logos, switcher JSON, version matching, Read the Docs configuration,
  canonical URLs, Open Graph/sitemap metadata, and Plausible analytics.
- Python-default highlighting and stripping `>>> ` prompts: MIRO has multiple
  example languages, so explicit code languages will be used.
- GAMSPy link-check exceptions, content images, model data, and landing cards.

GAMS website header/footer integration, release metadata generation, API
rendering, content migration, and existing link/anchor repairs are deferred.
