"""MIRO's standalone Sphinx preview, sharing GAMSPy's theme conventions."""

from datetime import datetime
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent))

from sphinx.highlighting import lexers
from gams_lexer import GamsLexer

lexers["gams"] = GamsLexer()
lexers["gms"] = GamsLexer()

DOC_DIR = Path(__file__).resolve().parents[1]

project = "GAMS MIRO"
author = "GAMS"
copyright = f"{datetime.now().year}, GAMS Development Corporation"
language = "en"
smartquotes = False

root_doc = "index"
source_suffix = {".rst": "restructuredtext"}
extensions = [
    "sphinx_design",
    "sphinx_copybutton",
    "sphinx_tabs.tabs",
]
templates_path = ["_templates"]
exclude_patterns = ["Thumbs.db", ".DS_Store"]

html_theme = "pydata_sphinx_theme"
# Keep the legacy source tree, but publish only the runtime files still used.
# image/figure directives and download roles track their own files separately.
html_static_path = [
    "_static",
    "../assets/plugins/jquery.min.js",
    "../assets/plugins/redoc/redoc.standalone.js",
    "../assets/images/configuration_mode/configuration.mp4",
    "../assets/images/miro-logo-wordmark-white.svg",
]
html_extra_path = ["../miro_server_api.json"]
html_css_files = ["custom.css"]
html_js_files = ["image-lightbox.js"]
html_show_sourcelink = False
html_show_search_summary = True
html_last_updated_fmt = None
html_title = "GAMS MIRO documentation"
html_logo = str(DOC_DIR / "assets" / "images" / "miro-logo-wordmark.svg")
html_favicon = "_static/gams.ico"
# Preserve public page names, including configuration_general.html.
html_file_suffix = ".html"
html_link_suffix = ".html"
html_theme_options = {
    "navbar_center": [],
    "navbar_start": [],
    "navbar_end": [],
    "navbar_persistent": ["search-button-field"],
    "logo": {
        "image_light": "miro-logo-wordmark.svg",
        "image_dark": "miro-logo-wordmark-white.svg",
    },
    "footer_start": [],
    "footer_center": [],
    "footer_end": [],
    "article_header_start": ["breadcrumbs"],
    "article_header_end": ["theme-switcher"],
    "show_nav_level": 1,
    "sidebar_includehidden": True,
    "navigation_depth": 4,
    "collapse_navigation": False,
    "show_toc_level": 2,
    "pygments_light_style": "tango",
    "pygments_dark_style": "lightbulb"
}

# MIRO examples span several languages; do not default to Python highlighting
# or strip Python prompts from literal code as GAMSPy's configuration does.
highlight_language = "none"
copybutton_prompt_text = ""


# The PyData theme suppresses its default TOC sidebar on the root document;
# use the same local sidebar template under a distinct name so index retains it.
html_sidebars = {
    "**": ["sidebar-brand-search.html", "sidebar-nav-bs.html"],
    "index": ["sidebar-brand-search.html", "sidebar-nav-index.html"],
}
