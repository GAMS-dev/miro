"""MIRO's standalone Sphinx preview, sharing GAMSPy's theme conventions."""

from datetime import datetime
from pathlib import Path
import shutil
import sys

sys.path.insert(0, str(Path(__file__).parent))

from sphinx.highlighting import lexers
from gams_lexer import GamsLexer

lexers["gams"] = GamsLexer()
lexers["gms"] = GamsLexer()

DOC_DIR = Path(__file__).resolve().parents[2]

project = "GAMS MIRO"
author = "GAMS"
copyright = f"{datetime.now().year}, GAMS Development Corporation"
language = "en"
smartquotes = False

root_doc = "index"
source_suffix = {".rst": "restructuredtext"}
extensions = [
    "sphinx.ext.mathjax",
    "sphinx_design",
    "sphinx_copybutton",
    "sphinx_tabs.tabs",
]
templates_path = ["_templates"]
exclude_patterns = ["Thumbs.db", ".DS_Store"]

html_theme = "pydata_sphinx_theme"
html_static_path = ["_static"]
html_css_files = ["custom.css"]
html_js_files = ["image-lightbox.js"]
html_show_sourcelink = False
html_show_search_summary = True
html_last_updated_fmt = ""
html_title = "GAMS MIRO documentation"
html_logo = str(DOC_DIR / "assets" / "images" / "MIRO_Logo_Schriftzug.svg")
html_favicon = "_static/gams.ico"
# Preserve public page names, including configuration_general.html.
html_file_suffix = ".html"
html_link_suffix = ".html"
html_theme_options = {
    "navbar_center": [],
    "navbar_start": [],
    "navbar_end": [],
    "navbar_persistent": ["search-button-field"],
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


def copy_legacy_assets(app, exception):
    """Keep legacy runtime files without a second checked-in asset tree.

    html_static_path would put these under _static, and html_extra_path pointed
    at doc/assets would flatten its contents into the output root. Copy only
    into the build output instead; do not load legacy CSS or JavaScript
    globally. The API specification is consumed by ReDoc on server_api.html.
    """
    if exception is None and app.builder.format == "html":
        output_dir = Path(app.outdir)
        shutil.copytree(DOC_DIR / "assets", output_dir / "assets", dirs_exist_ok=True)
        shutil.copy2(DOC_DIR / "miro_server_api.json", output_dir / "miro_server_api.json")


def setup(app):
    app.connect("build-finished", copy_legacy_assets)
    return {"parallel_read_safe": True, "parallel_write_safe": True}

# The PyData theme suppresses its default TOC sidebar on the root document;
# use the same local sidebar template under a distinct name so index retains it.
html_sidebars = {
    "**": ["sidebar-brand-search.html", "sidebar-nav-bs.html"],
    "index": ["sidebar-brand-search.html", "sidebar-nav-index.html"],
}
