# License
Note that the template used for this documentation is release under the Creative Commons Attribution 3.0 License. The template can be found here: https://github.com/xriley/PrettyDocs-Theme . The Montserrat font is available under the SIL Open Font License 1.1. A copy of this license can be found in `gallery/app/static/webfonts` subfolder.
# Documentation style guide
This style guide aims to standardize the structure and style of the documentation for the GAMS WebUI. When you write a new chapter, please follow these guidelines:

1. Use lively language: write as if you are a teacher in a classroom and use "**we**" when you explain something.
1. At the beginning of each chapter, first describe the problem to be solved. Then, use an example and explain how this problem can be solved using the WebUI. If needed, go into technical details at the end of each section. Use a new subsection for this. These technical subsections shall make sure that the reader can not only implement the example, but has an understanding of the inner workings of the WebUI.
1. Use images and gifs to support your examples.
1. Try to use the trnsport example whenever you can and if not use any of the other example models that come with the WebUI.
1. Try to keep the subject of your sentence close to the verb.
1. Make sure the **message** you want to convey in your sentence is in the *stress position*: it is located right before a colon, semicolon or period. (to learn more, read the articles by [George D. Gopen](https://www.georgegopen.com/the-litigation-articles.html))
1. Make sure, each sentence is connected to the previous one: your sentences must lead seamlessly forward. Use back references at the beginning of each sentence.
1. Use uniform terms. Especially when it comes to certain features of the WebUI, specific expressions should remain.


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
- `assets/` is the shared source of images and runtime resources. The Sphinx
  build copies it to `doc/_build/html/assets/` for compatibility URLs and
  runtime resources such as ReDoc.
- `_build/html/` is generated output and must not be committed.
