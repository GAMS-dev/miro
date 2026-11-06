# Theme and third-party assets

The Sphinx documentation uses the [PyData Sphinx Theme](https://github.com/pydata/pydata-sphinx-theme),
licensed under the BSD 3-Clause License. Bundled third-party assets retain their
respective license notices.

Legacy [PrettyDocs](https://github.com/xriley/PrettyDocs-Theme) assets remain in
`assets/` under their existing Creative Commons Attribution 3.0 license. They are
not used as the Sphinx theme or published as a complete asset tree.

# Documentation style guide
This style guide aims to standardize the structure and style of the documentation for GAMS MIRO. When you write a new chapter, please follow these guidelines:

1. Use lively language: write as if you are a teacher in a classroom and use "**we**" when you explain something.
1. At the beginning of each chapter, first describe the problem to be solved. Then, use an example and explain how this problem can be solved using GAMS MIRO. If needed, go into technical details at the end of each section. Use a new subsection for this. These technical subsections shall make sure that the reader can not only implement the example, but has an understanding of the inner workings of GAMS MIRO.
1. Use images and gifs to support your examples.
1. Try to use the trnsport example whenever you can and if not use any of the other example models that come with GAMS MIRO.
1. Try to keep the subject of your sentence close to the verb.
1. Make sure the **message** you want to convey in your sentence is in the *stress position*: it is located right before a colon, semicolon or period. (to learn more, read the articles by [George D. Gopen](https://www.georgegopen.com/the-litigation-articles.html))
1. Make sure, each sentence is connected to the previous one: your sentences must lead seamlessly forward. Use back references at the beginning of each sentence.
1. Use uniform terms. Especially when it comes to certain features of GAMS MIRO, specific expressions should remain.


#### Sources:
[https://www.georgegopen.com/the-litigation-articles.html](https://www.georgegopen.com/the-litigation-articles.html)

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
