"""
    pygments_reach
    ~~~~~~~~~~~~~~

    Lexer for Reach.

    https://reach.sh
    https://github.com/reachsh/reach-lang
"""

# Note: Heavily copied from JavascriptLexer
# https://github.com/pygments/pygments/blob/master/pygments/lexers/javascript.py

import re

from pygments.lexer import RegexLexer, include, bygroups, default, using, \
    this, words, combined
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Other
import pygments.unistring as uni

__all__ = ['ReachLexer']

JS_IDENT_START = ('(?:[$_' + uni.combine('Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl') +
                  ']|\\\\u[a-fA-F0-9]{4})')
JS_IDENT_PART = ('(?:[$' + uni.combine('Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl',
                                       'Mn', 'Mc', 'Nd', 'Pc') +
                 u'\u200c\u200d]|\\\\u[a-fA-F0-9]{4})')
JS_IDENT = JS_IDENT_START + '(?:' + JS_IDENT_PART + ')*'

class ReachLexer(RegexLexer):
    """
    For Reach source code.
    """

    name = 'Reach'
    aliases = ['rsh', 'reach']
    filenames = ['*.rsh']

    # TODO?
    mimetypes = ['application/javascript', 'application/x-javascript',
                 'text/x-javascript', 'text/javascript']

    flags = re.DOTALL | re.UNICODE | re.MULTILINE

    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Text),
            (r'<!--', Comment),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'([gimuy]+\b|\B)', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            default('#pop')
        ],
        'badregex': [
            (r'\n', Text, '#pop')
        ],
        'root': [
            (r'\A#! ?/.*?\n', Comment.Hashbang),  # recognized by node.js
            (r'^(?=\s|/|<!--)', Text, 'slashstartsregex'),
            include('commentsandwhitespace'),
            (r'(\.\d+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?', Number.Float),
            (r'0[bB][01]+', Number.Bin),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'\.\.\.', Punctuation),
            (r'=>', Operator),
            (r'\+\+|--|~|&&|\?|:|\|\||\\(?=\n)|'
             r'(<<|>>>?|==?|!=?|[-<>+*%&|^/])=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),
            (r'(for|in|while|do|break|return|continue|match|switch|case|default|if|else|'
             r'throw|try|catch|finally|new|delete|typeof|instanceof|void|yield|'
             # Reach ones
             r'interact|commit|exit|only|each|race|fork|paySpec|parallelReduce|when|timeout|timeRemaining|throwTimeout|publish|pay|declassify|transfer|'
             r'invariant|assert|require|assume|possible|unknowable|forall|'
             r'this|of)\b', Keyword, 'slashstartsregex'),
            (r'(var|let|with|function|'
             # reach ones
             r'export|const|import|from|as'
             r')\b', Keyword.Declaration, 'slashstartsregex'),
            (r'(abstract|boolean|byte|char|class|debugger|double|enum|'
             r'extends|final|float|goto|implements|int|interface|long|native|'
             r'package|private|protected|public|short|static|super|synchronized|throws|'
             r'transient|volatile)\b', Keyword.Reserved),
            (r'(true|false|null|NaN|Infinity|undefined)\b', Keyword.Constant),
            (r'(Array|Boolean|Date|Error|Function|Foldable|Math|netscape|'
             r'Number|Object|Packages|RegExp|String|Promise|Proxy|sun|decodeURI|'
             r'decodeURIComponent|encodeURI|encodeURIComponent|'
             r'Error|eval|isFinite|isNaN|isSafeInteger|parseFloat|parseInt|'
             # The reach ones
             r'UInt|Int|FixedPoint|Interval|IntervalType|Reach|App|Fun|Null|Bool|Bytes|Address|Token|Tuple|Struct|Participant|ParticipantClass|Data|Digest|Map|Set|Refine|Anybody|deployMode|verifyArithmetic|verifyPerConnector|connectors|ETH|ALGO|'
             r'balance|digest|implies|ensure|hasRandom|makeCommitment|checkCommitment|closeTo|lastConsensusTime|remote|'
             r'and|or|add|sub|mul|div|mod|lt|le|gt|ge|lsh|rsh|band|bior|bxor|eq|neq|'
             r'polyEq|polyNeq|typeEq|intEq|ite|typeOf|isType|is|'
             r'array|makeEnum|'
             r'document|this|window)\b', Name.Builtin),
            (JS_IDENT, Name.Other),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
            (r'`', String.Backtick, 'interp'),
        ],
        'interp': [
            (r'`', String.Backtick, '#pop'),
            (r'\\\\', String.Backtick),
            (r'\\`', String.Backtick),
            (r'\$\{', String.Interpol, 'interp-inside'),
            (r'\$', String.Backtick),
            (r'[^`\\$]+', String.Backtick),
        ],
        'interp-inside': [
            # TODO: should this include single-line comments and allow nesting strings?
            (r'\}', String.Interpol, '#pop'),
            include('root'),
        ],
        # (\\\\|\\`|[^`])*`', String.Backtick),
    }
