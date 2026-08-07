#!/usr/bin/env python3
"""Post-process a pandoc-generated .docx into the MDPI template's conventions.

    python3 docx_postprocess.py <unzipped-docx-dir>

Pandoc writes correct OOXML, but it names things after its own defaults rather
than after the styles the journal's template defines. Four fixes, each of which
is a rename or a width, never a change of content:

  1. Declare the png content type. The MDPI .dot does not declare it, so the
     figures are technically undeclared and strict readers flag the file as
     needing repair.

  2. Drop <w:pStyle w:val="Compact"/> where it precedes <w:numPr>. "Compact" is
     undefined in the template; LibreOffice discards the list properties of a
     paragraph whose style it cannot resolve, so numbered references and the
     Highlights bullets render flat. Word resolves it either way, so this only
     makes the LibreOffice preview match what Word shows.

  3. Put the tables onto the template's own styles: the three-line table style
     the journal expects, its table-caption style, and its table-body style for
     the cell paragraphs. Pandoc emits "Table", "TableCaption" and "Compact",
     none of which the template defines -- and an undefined *table* style is
     worse than an undefined paragraph style, because LibreOffice drops the
     table structure entirely and flattens the rows into paragraphs.

  4. Make each table span the text column and lay itself out from its contents.
     Pandoc gives every column an equal fixed width, which is wrong in both
     directions at once: a column holding one word gets as much room as one
     holding a sentence, and a wide table overruns the margin instead of
     fitting. Width becomes 100% of the text column and layout becomes autofit,
     so the column widths follow what is actually in them.

The cell-paragraph rename is deliberately scoped to the inside of each table.
"Compact" also appears outside tables, where it means something else, and a
global rename would restyle text that is not in any table.
"""

import re
import sys
import pathlib

import table_geometry

BODY_STYLE    = "MDPI42tablebody"
CAPTION_STYLE = "MDPI41tablecaption"
TABLE_STYLE   = "MDPI41threelinetable"


def declare_png(root: pathlib.Path) -> None:
    ct = root / "[Content_Types].xml"
    x = ct.read_text(encoding="utf-8")
    if 'Extension="png"' in x:
        print("  = png content type already declared")
        return
    x = x.replace('<Default Extension="xml"',
                  '<Default Extension="png" ContentType="image/png"/><Default Extension="xml"', 1)
    ct.write_text(x, encoding="utf-8")
    print("  + declared png content type")


def define_code_style(root: pathlib.Path) -> None:
    """Give pandoc's code runs a monospace face.

    Pandoc marks every `code span` with <w:rStyle w:val="VerbatimChar"/>, but
    the MDPI template defines no such style, so Word resolves it to nothing and
    every function name in Tables 2, 3 and in the captions came out in the body
    serif -- indistinguishable from prose. In Table 2 that is a real misreading:
    a column of R arguments stops looking like code at all.

    Define the style rather than hard-coding fonts on the runs, so a copy editor
    can restyle all of it from one place.
    """
    path = root / "word" / "styles.xml"
    x = path.read_text(encoding="utf-8")
    if 'w:styleId="VerbatimChar"' in x:
        print("  = code character style already defined")
        return
    style = (
        '<w:style w:type="character" w:customStyle="1" w:styleId="VerbatimChar">'
        '<w:name w:val="Verbatim Char"/><w:qFormat/><w:rPr>'
        '<w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/>'
        '<w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr></w:style>'
    )
    x = x.replace("</w:styles>", style + "</w:styles>", 1)
    path.write_text(x, encoding="utf-8")
    print("  + defined the code character style the template lacks")


def unblock_numbering(doc: str) -> str:
    doc, n = re.subn(r'<w:pStyle w:val="Compact"\s*/>(?=\s*<w:numPr>)', "", doc)
    print(f"  + unblocked list numbering on {n} paragraphs")
    return doc


def rule_under_header(tbl: str) -> str:
    """Draw the middle rule of the three-line table.

    The template's table style supplies the rule above the header and the one
    under the last row, but the third -- between the header and the body -- has
    to be set on the header row itself, and pandoc has no reason to know that.
    """
    rows = list(re.finditer(r"<w:tr>", tbl))
    if not rows:
        return tbl
    head_end = tbl.find("</w:tr>")
    if head_end == -1:
        return tbl
    header = tbl[: head_end + len("</w:tr>")]
    if "<w:tcBorders>" in header:
        return tbl
    border = ('<w:tcBorders><w:bottom w:val="single" w:sz="8" w:space="0" '
              'w:color="auto"/></w:tcBorders>')
    header = header.replace("<w:tcPr />", f"<w:tcPr>{border}</w:tcPr>")
    return header + tbl[head_end + len("</w:tr>"):]


def left_align_stub(tbl: str) -> str:
    """Left-align the first column; the template centres everything.

    Centring is right for a column of short answers and wrong for the stub,
    whose entries are phrases that wrap onto two or three lines. MDPI's own
    typesetting left-aligns it, and a centred ragged block is harder to scan.
    """
    def fix_row(m: "re.Match[str]") -> str:
        row = m.group(0)
        first = row.find("<w:tc>")
        if first == -1:
            return row
        end = row.find("</w:tc>", first)
        if end == -1:
            return row
        cell = row[first:end]
        cell = cell.replace(f'<w:pStyle w:val="{BODY_STYLE}"/>',
                            f'<w:pStyle w:val="{BODY_STYLE}"/>'
                            '<w:jc w:val="left"/>')
        return row[:first] + cell + row[end:]

    return re.sub(r"<w:tr>.*?</w:tr>", fix_row, tbl, flags=re.S)


def uncramp_caption(doc: str) -> str:
    """Remove the template caption style's 2608-twip left indent.

    MDPI_4.1_table_caption carries w:ind left=2608, which sets the caption in a
    narrow column about 4.6 cm from the margin. That suits a caption sitting
    beside a narrow centred table; over a full-width table it reads as a
    mistake. Keep the style -- its font, size and spacing are what the journal
    wants -- and override only the indent.
    """
    doc, n = re.subn(
        r'(<w:pStyle w:val="%s"\s*/>)' % CAPTION_STYLE,
        r'\1<w:ind w:left="0" w:right="0" w:firstLine="0"/>',
        doc)
    print(f"  + un-indented {n} table caption(s)")
    return doc


def restyle_tables(doc: str) -> str:
    """Rewrite each <w:tbl> block: template style, full width, autofit, body cells."""
    tables = 0
    cells = 0

    def fix(match: "re.Match[str]") -> str:
        nonlocal tables, cells
        tbl = match.group(0)
        tables += 1

        tbl = tbl.replace('<w:tblStyle w:val="Table" />',
                          f'<w:tblStyle w:val="{TABLE_STYLE}"/>')
        # Span the text column, and lay out from the grid rather than from
        # the writer's guess: table_geometry has put real widths in the grid.
        tbl = tbl.replace('<w:tblW w:type="auto" w:w="0" />',
                          '<w:tblW w:type="pct" w:w="5000"/>'
                          '<w:tblLayout w:type="fixed"/>')
        tbl, k = re.subn(r'<w:pStyle w:val="Compact"\s*/>',
                         f'<w:pStyle w:val="{BODY_STYLE}"/>', tbl)
        cells += k
        tbl = rule_under_header(tbl)
        tbl = left_align_stub(tbl)
        tbl = table_geometry.fit(tbl, label=f'table {tables}')
        return tbl

    doc = re.sub(r"<w:tbl>.*?</w:tbl>", fix, doc, flags=re.S)
    print(f"  + restyled {tables} table(s), {cells} cell paragraphs -> {BODY_STYLE}")

    doc, n = re.subn(r'<w:pStyle w:val="TableCaption"\s*/>',
                     f'<w:pStyle w:val="{CAPTION_STYLE}"/>', doc)
    print(f"  + {n} table caption(s) -> {CAPTION_STYLE}")
    return uncramp_caption(doc)


def main() -> int:
    root = pathlib.Path(sys.argv[1])
    declare_png(root)
    define_code_style(root)

    path = root / "word" / "document.xml"
    doc = path.read_text(encoding="utf-8")
    doc = unblock_numbering(doc)
    doc = restyle_tables(doc)
    path.write_text(doc, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
