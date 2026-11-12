# Theme and third-party assets

The Sphinx documentation uses the [PyData Sphinx Theme](https://github.com/pydata/pydata-sphinx-theme),
licensed under the BSD 3-Clause License. Bundled third-party assets retain their
respective license notices.

Legacy [PrettyDocs](https://github.com/xriley/PrettyDocs-Theme) assets remain in
`assets/` under their existing Creative Commons Attribution 3.0 license. They are
not used as the Sphinx theme or published as a complete asset tree.

# Documentation style guide
Follow these guidelines when you write or edit the GAMS MIRO documentation.

## Structure

1. Start each chapter with the problem it solves. Then show how GAMS MIRO solves it, using an example.
1. Put technical details in a separate subsection at the end. After reading it, the reader should understand how the feature works, not only how to reproduce the example.
1. Use screenshots and GIFs to support the examples. Keep them current: replace an image when the UI it shows changes.
1. Use the `transport` demo app whenever possible (it is based on the `trnsport` model from the GAMS Model Library). Otherwise, use another example app that ships with GAMS MIRO.

## Voice and tone

1. Write like a teacher in a classroom: use "**we**" when walking through an example and "**you**" when telling readers what they can do.
1. Be clear and concise. Prefer short sentences, active voice and everyday words. Cut filler such as "simply", "basically", "in order to" and "please note that".
1. Keep the subject of a sentence close to its verb.
1. Put the most important information in the *stress position*: at the end of the sentence, right before the period, colon or semicolon. See the articles by [George D. Gopen](https://www.georgegopen.com/the-litigation-articles.html).
1. Connect each sentence to the previous one. Start with information the reader already knows and end with what is new.
1. Use American English.

## Terminology

Use the same term for the same thing everywhere. In particular:

| Use | Do not use |
| --- | --- |
| GAMS MIRO on first mention in a chapter, MIRO afterwards | WebUI, web UI, Miro |
| MIRO Desktop, MIRO Server | MIRO desktop, MIRO server, MIRO Desktop app |
| MIRO app (or MIRO application) | MIRO model, dashboard |
| Base Mode, Configuration Mode, Hypercube Mode | configuration mode, config mode |
| Mira (the MIRO AI assistant) | MIRA, MIRO AI, chatbot |
| GAMS Engine, GAMS Studio, GAMSPy | Engine (alone), Gamspy |
| dataset | data set |
| dropdown | drop-down, drop down |
| input symbol, output symbol | input/output parameter (unless it is one) |
| click | click on |

## Formatting

1. Write UI labels exactly as they appear in the app and in italics (for example, *Solve model*).
1. Format file names, paths, options, symbol names and code with double backticks.
1. Use admonitions (`.. note::`, `.. tip::`, `.. warning::`, `.. admonition::`) sparingly, for information the reader must not miss.

# Sphinx documentation

The MIRO documentation is authored in reStructuredText under `source/` and
built with Sphinx.

## Build locally

From the repository root:

```sh
python3 -m venv doc/.venv
doc/.venv/bin/python -m pip install -r doc/requirements.txt
doc/.venv/bin/python -m sphinx -b html -n -W --keep-going doc/source doc/_build/html
```

On Windows, use `doc/.venv/Scripts/python.exe` for the interpreter. Build
output and the virtual environment are ignored by Git.

To preview the generated site locally:

```sh
doc/.venv/bin/python -m http.server 8000 --directory doc/_build/html
```

## Layout

- `source/` contains flat RST source files. For example, `model.rst` builds to
  `model.html`.
- `assets/` remains the shared source of images and runtime resources. Sphinx
  copies referenced images/figures to `_images/` and downloads to `_downloads/`.
  The complete legacy asset tree is not published.
- `html_static_path` lists the few runtime files still needed (jQuery, ReDoc,
  the configuration video and the dark logo), alongside `source/_static/`.
  These files and the primary `html_logo` are copied to `_static/`. Template
  URLs use `pathto()`; page-specific raw HTML refers to these static filenames.
- `html_extra_path` publishes `miro_server_api.json` at the output root for
  ReDoc. A targeted build hook also publishes `assets/images/logo.svg` at its
  existing public URL for Helm charts and image READMEs; no other legacy assets
  are copied by this hook.
- `_build/html/` is generated output and must not be committed.
- RST source files are not copied to the published site (`html_copy_source = False`).

## CI builds

The Linux builder image installs `doc/requirements.txt` into `/opt/miro-docs`
and exposes its interpreter through `SPHINX_PYTHON`. The requirements pin both
direct and transitive documentation dependencies, including the Windows-only
`colorama` dependency. Update the pins together and verify a strict Sphinx build.
Both staging and production jobs use `builder-linux:latest` without installing packages during publication.
Changes to the Dockerfile or requirements trigger a builder image rebuild in
push and merge request pipelines. The staging job waits for that build when it
is included in the pipeline; otherwise it uses the existing builder image. When building the image
manually, first copy `doc/requirements.txt` to `ci/linux/requirements-docs.txt`,
then use `ci/linux` as the Docker build context.

Before the production build, `scripts/update_docs_release.py` freezes the date
of the latest release and the download version label in `source/release.rst` and
`source/index.rst`. The publishing job commits both sources. Existing release
dates are never overwritten; unresolved placeholders in older entries fail the
build instead of being filled with the current date. Staging leaves these
sources unchanged.

Production post-processing checks the advertised download version and the latest
release heading/date, rejects unresolved placeholders, and makes rewritten HTML
files readable by the web server. Local release builds update the same sources
and can override `SPHINX_PYTHON` and `DOC_OUTPUT_DIR`.

Run the release metadata regression checks with:

```sh
python3 -m unittest discover -s scripts/tests -p 'test_update_docs_release.py'
```
