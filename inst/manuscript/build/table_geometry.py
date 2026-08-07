"""Decide column widths and type size for the tables of a pandoc .docx.

HTML carries no column widths, so pandoc gives every column an equal share of
the page. That is wrong in both directions at once: in Table 8 the stub column
holds "Reads finished SfM products (ortho / DSM / DTM / LAS / PLY)" while nine
others hold "yes", and at equal widths the stub wraps mid-word while the rest
sit half empty.

Two quantities decide a readable table, and they trade against each other:

  width   Each column should get room in proportion to what is in it, but never
          less than its longest *unbreakable* run of characters. Below that
          floor the writer breaks inside a word -- "DroneBio / R", "Metashap /
          e" -- which is the one outcome a reader always notices.

  size    When the floors together exceed the text column, no distribution of
          width can satisfy them. The answer then is a smaller type size for
          that table, which shrinks every floor at once. Squeezing the columns
          instead just reintroduces the mid-word breaks.

Both are settled here, on the finished XML, because settling them apart means
neither can see the other's constraint.

The character widths are not assumed. They were recovered by least squares from
every bold text span in a rendered PDF of this manuscript -- 290 spans, 71
characters, mean residual 0.05 em -- because a width per character *class* is
not good enough here: in Palatino bold "M" is 0.98 em and "i" is 0.26 em, so
"WebODM" and "Capability" have almost the same width at very different lengths,
and a class-based estimate put "WebODM" 0.4 pt short of its column.

Bold is the right table to use even for the body: headers are bold and are what
usually binds, and using bold widths for roman text errs toward wider columns.
"""

import html
import re

# MDPI A4 with 720-twip margins: (11906 - 1440) twips.
TEXT_WIDTH_PT = 523.3
CELL_PAD_PT = 11.0          # the two default cell margins, 108 twips each
BASE_SZ_HALFPT = 20         # the template's 10 pt table type
MIN_SZ_HALFPT = 15          # do not shrink a table below 7.5 pt
SAFETY = 1.08               # measured widths are averages; leave a margin

_EM = {
    ' ': 0.336, '%': 0.894, '(': 0.323, ')': 0.323, '*': 0.602, '+': 0.678,
    ',': 0.054, '-': 0.419, '.': 0.290, '/': 0.295, '0': 0.469, '1': 0.530,
    '2': 0.520, '3': 0.449, '4': 0.501, '5': 0.428, '6': 0.486, '7': 0.423,
    '8': 0.480, '9': 0.520, ':': 0.223, '?': 0.478, 'A': 0.798, 'B': 0.592,
    'C': 0.822, 'D': 0.868, 'E': 0.780, 'F': 0.513, 'G': 0.810, 'H': 0.688,
    'I': 0.361, 'K': 0.604, 'L': 0.471, 'M': 0.978, 'N': 0.860, 'O': 0.802,
    'P': 0.577, 'R': 0.717, 'S': 0.502, 'T': 0.642, 'V': 0.740, 'W': 0.985,
    '_': 0.493, 'a': 0.539, 'b': 0.536, 'c': 0.451, 'd': 0.638, 'e': 0.485,
    'f': 0.203, 'g': 0.480, 'h': 0.554, 'i': 0.264, 'k': 0.648, 'l': 0.319,
    'm': 0.853, 'n': 0.647, 'o': 0.573, 'p': 0.600, 'q': 0.420, 'r': 0.412,
    's': 0.462, 't': 0.313, 'u': 0.691, 'v': 0.574, 'w': 0.744, 'x': 0.407,
    'y': 0.634, '²': 0.300, 'Δ': 0.714, 'β': 0.583, '−': 0.714,
}
_EM_DEFAULT = 0.60           # anything not measured

def text_pt(s: str) -> float:
    """Width of a string in points, at the template's 10 pt table type."""
    return sum(_EM.get(ch, _EM_DEFAULT) for ch in s) * BASE_SZ_HALFPT / 2


def longest_unbreakable(s: str) -> float:
    """Width of the longest run the writer cannot break.

    Word breaks at whitespace, and after a hyphen, slash or closing
    punctuation. Not at an underscore: "cloth_resolution" in a narrow column
    comes out as "cloth_resol / ution", so an R identifier has to be treated as
    one indivisible run however long it is. "FIELD-imageR" can break into "FIELD-" and "imageR";
    "georeferenced" cannot break at all and sets the floor for its column on
    its own.

    The punctuation matters more than it looks. Table 2 lists an argument
    default as c("dtm","min_z",...,"perimeter_tin"), -- 73 characters without a
    single space. Treating the commas as unbreakable demanded 357 pt for one
    column and squeezed the next one down to a ribbon a word wide.
    """
    widest = 0.0
    for word in s.split():
        for piece in re.split(r"(?<=[-/,;:)\]}])", word):
            widest = max(widest, text_pt(piece))
    return widest


def _cells(row_xml: str):
    """(gridSpan, text) for each cell of a row."""
    out = []
    for cell in re.findall(r"<w:tc>.*?</w:tc>", row_xml, re.S):
        span = re.search(r'<w:gridSpan w:val="(\d+)"', cell)
        # <w:t> only -- not <w:tc>, <w:tcPr>, <w:tbl>. "w:t" is a prefix of all
        # of them, so the tag name has to end here, at a space or the bracket.
        text = "".join(re.findall(r"<w:t(?:\s[^>]*)?>(.*?)</w:t>", cell, re.S))
        # &quot; is one character on the page but six in the XML, and it has no
        # break opportunity in it, so leaving it encoded turned
        # 'pc_quality = "medium",' into a single 26-character unbreakable token
        # and demanded 722 pt for one column of a 523 pt page.
        text = html.unescape(text)
        out.append((int(span.group(1)) if span else 1, text))
    return out


def _columns(tbl: str, ncol: int):
    """Per-column (demand, floor) in points, ignoring spanning cells."""
    demand = [0.0] * ncol
    floor = [0.0] * ncol
    for row in re.findall(r"<w:tr>.*?</w:tr>", tbl, re.S):
        col = 0
        for span, text in _cells(row):
            if span == 1 and col < ncol:
                # A cell far longer than a column should ever be gets to ask
                # for a wide column, not an entire page.
                demand[col] = max(demand[col], min(text_pt(text), 250.0))
                floor[col] = max(floor[col], longest_unbreakable(text) * SAFETY)
            col += span
    return demand, floor


def _distribute(demand, floor, width_pt):
    """Share width by demand, pinning any column that falls under its floor.

    Pinning one column takes room from the others, which can push a second
    under its own floor, so this repeats until a pass changes nothing.
    """
    n = len(demand)
    width = [0.0] * n
    pinned = [False] * n
    free_pt = width_pt
    free_demand = sum(demand) or 1.0

    changed = True
    while changed:
        changed = False
        for i in range(n):
            if pinned[i]:
                continue
            width[i] = free_pt * demand[i] / free_demand if free_demand else 0.0
            if width[i] < floor[i]:
                pinned[i] = True
                width[i] = floor[i]
                free_pt -= floor[i]
                free_demand -= demand[i]
                changed = True
    return width


def _shrink_runs(tbl: str, half_pt: int) -> str:
    """Force every run in the table to a type size."""
    sz = f'<w:sz w:val="{half_pt}"/><w:szCs w:val="{half_pt}"/>'

    def fix(m):
        run = m.group(0)
        run = re.sub(r"<w:sz w:val=\"\d+\"\s*/>|<w:szCs w:val=\"\d+\"\s*/>", "", run)
        if "<w:rPr>" in run:
            return run.replace("<w:rPr>", f"<w:rPr>{sz}", 1)
        return run.replace("<w:r>", f"<w:r><w:rPr>{sz}</w:rPr>", 1)

    return re.sub(r"<w:r>.*?</w:r>", fix, tbl, flags=re.S)


def fit(tbl: str, label: str = "") -> str:
    """Set one table's column widths, shrinking its type only if it must."""
    grid = re.search(r"<w:tblGrid>.*?</w:tblGrid>", tbl, re.S)
    if not grid:
        return tbl
    ncol = len(re.findall(r"<w:gridCol", grid.group(0)))
    if ncol == 0:
        return tbl

    demand, floor = _columns(tbl, ncol)

    # Cell margins are a fixed 108 twips a side whatever the type size, so they
    # come off the top and only the text has to fit in what is left. Scaling
    # them with the font was what left "WebODM" one point short of its column.
    padding = CELL_PAD_PT * ncol
    for_text = TEXT_WIDTH_PT - padding

    # If the floors still cannot all be met, the columns are not the problem:
    # the type is too big for this many columns. Shrink it just enough to fit.
    scale = 1.0
    need = sum(floor)
    if need > for_text:
        scale = max(for_text / need, MIN_SZ_HALFPT / BASE_SZ_HALFPT)
        floor = [f * scale for f in floor]
        demand = [d * scale for d in demand]

    width = _distribute(demand, floor, for_text)

    # _distribute honours the floors even when they cannot all be honoured --
    # after the type size has hit its own limit, some tables simply contain a
    # token too long for any column. Overrunning the page is the worse failure
    # of the two, so give up the floors proportionally rather than the margin.
    total = sum(width)
    if total > for_text + 0.5:
        print(f"    ! {label or 'table'}: content needs {total + padding:.0f} pt "
              f"in {TEXT_WIDTH_PT:.0f} pt even at minimum type; scaling to fit")
        width = [w * for_text / total for w in width]

    width = [w + CELL_PAD_PT for w in width]

    cols = "".join(f'<w:gridCol w:w="{max(1, round(w * 20))}"/>' for w in width)
    tbl = tbl.replace(grid.group(0), f"<w:tblGrid>{cols}</w:tblGrid>", 1)

    # Pin each cell to its column too; a fixed-layout table that disagrees with
    # its own grid is resolved differently by Word and by LibreOffice.
    def fix_row(m):
        row = m.group(0)
        col = 0
        out = []
        last = 0
        for cell in re.finditer(r"<w:tc>.*?</w:tc>", row, re.S):
            span_m = re.search(r'<w:gridSpan w:val="(\d+)"', cell.group(0))
            span = int(span_m.group(1)) if span_m else 1
            w = sum(width[col:col + span]) if col < ncol else 0.0
            body = cell.group(0)
            tcw = f'<w:tcW w:w="{max(1, round(w * 20))}" w:type="dxa"/>'
            if "<w:tcPr>" in body:
                body = body.replace("<w:tcPr>", f"<w:tcPr>{tcw}", 1)
            else:
                body = body.replace("<w:tcPr />", f"<w:tcPr>{tcw}</w:tcPr>", 1)
            out.append(row[last:cell.start()])
            out.append(body)
            last = cell.end()
            col += span
        out.append(row[last:])
        return "".join(out)

    tbl = re.sub(r"<w:tr>.*?</w:tr>", fix_row, tbl, flags=re.S)

    if scale < 1.0:
        half_pt = max(MIN_SZ_HALFPT, int(round(BASE_SZ_HALFPT * scale)))
        tbl = _shrink_runs(tbl, half_pt)
        print(f"    - {label or 'table'}: {ncol} columns need {need:.0f} pt of "
              f"unbreakable text in {for_text:.0f} pt; set to {half_pt / 2:g} pt")
    return tbl
