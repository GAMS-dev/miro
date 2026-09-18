"""Pygments lexer for GAMS source code.

This lexer is based on the custom PrismJS GAMS grammar previously used by the
GAMS MIRO documentation.  It is intentionally self-contained so it can be
registered directly from a Sphinx ``conf.py`` without installing an additional
package.
"""

import re

from pygments.lexer import RegexLexer, bygroups, using, words
from pygments.lexers.python import PythonLexer
from pygments.token import Comment, Keyword, Name, Number, Operator, Punctuation, String, Text


class GamsLexer(RegexLexer):
    """Syntax highlighter for the GAMS modeling language."""

    name = "GAMS"
    aliases = ["gams", "gms"]
    filenames = ["*.gms"]
    mimetypes = ["text/x-gams"]
    flags = re.IGNORECASE | re.MULTILINE

    _declarations = (
        "acronym",
        "acronyms",
        "binary variable",
        "binary variables",
        "equation",
        "equations",
        "file",
        "files",
        "free variable",
        "free variables",
        "function",
        "functions",
        "integer variable",
        "integer variables",
        "logic equation",
        "logic equations",
        "model",
        "models",
        "negative variable",
        "negative variables",
        "nonnegative variable",
        "nonnegative variables",
        "parameter",
        "parameters",
        "positive variable",
        "positive variables",
        "scalar",
        "scalars",
        "semicont variable",
        "semicont variables",
        "semiint variable",
        "semiint variables",
        "set",
        "sets",
        "singleton set",
        "singleton sets",
        "sos1 variable",
        "sos1 variables",
        "sos2 variable",
        "sos2 variables",
        "table",
        "tables",
        "variable",
        "variables",
        "variable table",
        "variable tables",
        "equation table",
        "equation tables",
    )

    _keywords = (
        "abort",
        "break",
        "continue",
        "display",
        "else",
        "elseif",
        "endfor",
        "endif",
        "endloop",
        "endwhile",
        "eq",
        "eqv",
        "execute",
        "execute_load",
        "execute_loaddc",
        "execute_loadhandle",
        "execute_loadpoint",
        "execute_unload",
        "execute_unloaddi",
        "execute_unloadidx",
        "for",
        "ge",
        "gt",
        "if",
        "imp",
        "le",
        "loop",
        "lt",
        "ne",
        "put",
        "put_utility",
        "putclear",
        "putclose",
        "puthd",
        "putpage",
        "putil",
        "repeat",
        "then",
        "until",
        "while",
        "xor",
    )

    _functions = (
        "abs",
        "arccos",
        "acrcsin",  # kept for compatibility with the legacy Prism grammar
        "arctan",
        "beta",
        "betareg",
        "binomial",
        "card",
        "ciel",  # kept for compatibility with the legacy Prism grammar
        "centropy",
        "cos",
        "diag",
        "edist",
        "exp",
        "heaplimit",
        "ifthen",
        "log",
        "max",
        "min",
        "mod",
        "ord",
        "power",
        "prod",
        "round",
        "sameas",
        "sin",
        "smax",
        "smin",
        "sqr",
        "sqrt",
        "sum",
        "tan",
        "uniform",
    )

    _model_types = (
        "cns",
        "dnlp",
        "emp",
        "lp",
        "mcp",
        "minlp",
        "mip",
        "miqcp",
        "mpec",
        "mpsge",
        "nlp",
        "qcp",
        "rmip",
        "rminlp",
        "rmiqcp",
        "rmpec",
    )

    tokens = {
        "root": [
            # GAMS block and line comments.
            (r"\$ontext[\s\S]*?\$offtext", Comment.Multiline),
            (r"^\*.*$", Comment.Single),
            (r"(?<![:\\])//.*$", Comment.Single),
            (r"(?<![:\\])!!.*$", Comment.Single),

            # Embedded Python.  Highlight the body using Pygments' Python lexer.
            (
                r"((?:continue|\$on)?embeddedcode(?:s|v)?\s*python\s*:)([\s\S]*?)"
                r"((?:\$off|pause|end)embeddedcode[^\n]*)",
                bygroups(Keyword.Declaration, using(PythonLexer), Keyword.Declaration),
            ),
            # Other embedded-code blocks remain readable even if we do not know
            # the embedded language.
            (
                r"((?:continue|\$on)?embeddedcode(?:s|v)?(?:\s+\w+)?\s*:)([\s\S]*?)"
                r"((?:\$off|pause|end)embeddedcode[^\n]*)",
                bygroups(Keyword.Declaration, Text, Keyword.Declaration),
            ),

            # Compiler directives.  As in the legacy Prism grammar, only a '$'
            # command beginning a line is treated as a directive; '$' used for
            # domain conditions remains part of normal GAMS syntax.
            (
                r"^(\s*)(\$(?:title|stitle)\b)([^\n]*)",
                bygroups(Text, Comment.Preproc, Name.Label),
            ),
            (
                r"^(\s*)(\$[A-Za-z_]\w*)([^\n]*)",
                bygroups(Text, Comment.Preproc, Text),
            ),

            # Strings.
            (r"'(?:\\.|[^'\\\r\n])*'", String),
            (r'"(?:\\.|[^"\\\r\n])*"', String),

            # Multi-word declaration keywords have to be matched before the
            # generic identifier rule.  The declaration state lasts to ';'.
            (words(_declarations, prefix=r"\b", suffix=r"\b"), Keyword.Declaration, "declaration"),

            # Alias declarations.
            (r"\balias\b", Keyword.Declaration, "alias"),

            # Solve statements.
            (r"\bsolve\b", Keyword, "solve"),

            # Option statements.
            (r"\boptions?\b", Keyword.Declaration, "option"),

            # Equation definitions: eq(i).. x(i) =g= ... ;
            (
                r"\b([A-Za-z_]\w*(?:\s*\([^;\n]*?\))?)(\s*)(\.\.)",
                bygroups(Name.Label, Text, Punctuation),
                "equation",
            ),

            (words(_keywords, prefix=r"\b", suffix=r"\b"), Keyword),
            (words(_functions, prefix=r"\b", suffix=r"(?=\s*\()"), Name.Builtin),

            # Numbers, operators, punctuation and identifiers.
            (r"\b(?:0x[\da-f]+|\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?\b", Number),
            (r"=b=|=c=|=e=|=g=|=l=|=n=|=x=", Operator.Word),
            (r"\*\*|<=|>=|<>|:=|[+\-*\/=<>]", Operator),
            (r"\$", Operator),
            (r"[()\[\]{},.;:]", Punctuation),
            (r"\b[A-Za-z_]\w*\b", Name),
            (r"\s+", Text.Whitespace),
            (r".", Text),
        ],

        "declaration": [
            (r";", Punctuation, "#pop"),
            (r"\$ontext[\s\S]*?\$offtext", Comment.Multiline),
            (r"^\*.*$", Comment.Single),
            (r"(?<![:\\])//.*$", Comment.Single),
            (r"(?<![:\\])!!.*$", Comment.Single),
            (r"'(?:\\.|[^'\\\r\n])*'", String),
            (r'"(?:\\.|[^"\\\r\n])*"', String),
            (r"\b(?:0x[\da-f]+|\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?\b", Number),
            (r"\.\.", Operator),
            (r"/", Punctuation),
            (r"[()\[\]{},.:]", Punctuation),
            (r"\b[A-Za-z_]\w*\b", Name.Variable),
            (r"[-+*=<>$]", Operator),
            (r"\s+", Text.Whitespace),
            (r".", Text),
        ],

        "alias": [
            (r";", Punctuation, "#pop"),
            (r"[(),]", Punctuation),
            (r"\b[A-Za-z_]\w*\b", Name.Variable),
            (r"\s+", Text.Whitespace),
            (r".", Text),
        ],

        "solve": [
            (r";", Punctuation, "#pop"),
            (r"\b(?:min|minimise|minimize|minimising|minimizing|max|maximise|maximize|maximising|maximizing)\b", Keyword),
            (r"\b(?:us|use|using)\b", Keyword),
            (words(_model_types, prefix=r"\b", suffix=r"\b"), Keyword.Type),
            (r"\b[A-Za-z_]\w*\b", Name.Variable),
            (r"[(),.]", Punctuation),
            (r"\s+", Text.Whitespace),
            (r".", Text),
        ],

        "option": [
            (r";", Punctuation, "#pop"),
            (r"\b[A-Za-z_]\w*\b", Name.Attribute),
            (r"\b(?:0x[\da-f]+|\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?\b", Number),
            (r"[<>=]", Operator),
            (r"[,.:]", Punctuation),
            (r"\s+", Text.Whitespace),
            (r".", Text),
        ],

        "equation": [
            (r";", Punctuation, "#pop"),
            (r"\$ontext[\s\S]*?\$offtext", Comment.Multiline),
            (r"^\*.*$", Comment.Single),
            (r"(?<![:\\])//.*$", Comment.Single),
            (r"(?<![:\\])!!.*$", Comment.Single),
            (r"'(?:\\.|[^'\\\r\n])*'", String),
            (r'"(?:\\.|[^"\\\r\n])*"', String),
            (r"=b=|=c=|=e=|=g=|=l=|=n=|=x=", Operator.Word),
            (words(_functions, prefix=r"\b", suffix=r"(?=\s*\()"), Name.Builtin),
            (r"\b(?:0x[\da-f]+|\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?\b", Number),
            (r"\*\*|<=|>=|<>|:=|[+\-*\/=<>]", Operator),
            (r"\$", Operator),
            (r"[()\[\]{},.:/]", Punctuation),
            (r"\b[A-Za-z_]\w*\b", Name.Variable),
            (r"\s+", Text.Whitespace),
            (r".", Text),
        ],
    }
