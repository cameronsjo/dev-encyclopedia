---
title: Document Formats
aliases:
  - PDF Format
  - Office Formats
  - EPUB Format
  - Document File Formats
tags:
  - cs
  - fundamentals
  - file-formats
type: concept
status: complete
difficulty: fundamentals
created: "2026-02-19"
---

# Document Formats

How documents are stored digitally — from PDF's page description model to Office XML's ZIP-based structure and EPUB's web-standards approach.

## Overview

| Format | Structure | Editable | Layout | Use Case |
|--------|-----------|----------|--------|----------|
| PDF | Binary object graph | Difficult | Fixed (pixel-perfect) | Print, contracts, archival |
| DOCX | ZIP of XML | Yes (Word) | Reflowable | Business documents |
| ODT | ZIP of XML | Yes (LibreOffice) | Reflowable | Open-source documents |
| EPUB | ZIP of XHTML + CSS | Yes | Reflowable | E-books |
| RTF | Text markup | Yes | Basic | Legacy interchange |
| Plain text | Raw bytes | Yes | None | Code, logs, notes |
| LaTeX | Text markup | Yes (source) | Fixed (compiled) | Academic papers, math |

---

## PDF (Portable Document Format)

Created by Adobe in 1993, now ISO standard 32000. Designed for **fixed-layout** documents that look identical everywhere.

### How PDF Works

A PDF is not a sequence of pages like you'd expect. It's an **object graph** — a collection of numbered objects that reference each other:

```
┌─────────────────────────────────────────────┐
│ Header:  %PDF-1.7                            │
├─────────────────────────────────────────────┤
│ Body: Numbered objects                       │
│                                              │
│  1 0 obj  (Catalog - root of document)       │
│    → points to Pages object                  │
│  2 0 obj  (Pages - page tree)                │
│    → points to individual Page objects        │
│  3 0 obj  (Page 1)                           │
│    → points to Content stream + Resources     │
│  4 0 obj  (Content stream - drawing commands) │
│    → moveto, lineto, show text, draw image    │
│  5 0 obj  (Font - embedded or referenced)     │
│    → TrueType/Type1/CID font data            │
│  6 0 obj  (Image - embedded raster)           │
│    → JPEG/CCITT/Flate compressed pixels       │
│                                              │
├─────────────────────────────────────────────┤
│ Cross-Reference Table (xref)                 │
│   Maps object numbers → byte offsets         │
├─────────────────────────────────────────────┤
│ Trailer                                      │
│   Points to: Catalog, Info dict, xref offset │
│   startxref [byte offset to xref]            │
│   %%EOF                                      │
└─────────────────────────────────────────────┘
```

### PDF Hex Walkthrough

```
Offset    Content                          Meaning
00000000  25 50 44 46 2D 31 2E 37         "%PDF-1.7" header
00000008  0A 25 E2 E3 CF D3 0A            Binary comment (signals binary content)

          ...objects...

          1 0 obj                          Object 1, generation 0
          << /Type /Catalog               Catalog dictionary
             /Pages 2 0 R >>              Reference to object 2
          endobj

          4 0 obj                          Content stream
          << /Length 44 >>
          stream
          BT                               Begin Text
          /F1 12 Tf                        Font F1, 12pt
          100 700 Td                       Move to (100, 700)
          (Hello, World!) Tj               Draw text
          ET                               End Text
          endstream
          endobj
```

### PDF Content Streams

Page content uses a PostScript-like drawing language:

| Operator | Meaning | Example |
|----------|---------|---------|
| `BT` / `ET` | Begin/end text block | `BT ... ET` |
| `Tf` | Set font and size | `/F1 12 Tf` |
| `Td` | Move text position | `100 700 Td` |
| `Tj` | Show text string | `(Hello) Tj` |
| `m` | Move to point | `100 200 m` |
| `l` | Line to point | `300 400 l` |
| `S` | Stroke path | `S` |
| `f` | Fill path | `f` |
| `re` | Rectangle | `50 50 200 100 re` |
| `Do` | Draw XObject (image) | `/Img1 Do` |
| `cm` | Transform matrix | `1 0 0 1 100 200 cm` |

### PDF Incremental Updates

PDFs can be updated **without rewriting** — new objects and a new xref table are appended to the end:

```
[Original PDF content]
[Original xref]
[Original trailer + %%EOF]

[New/modified objects]        ← Appended
[New xref (references new objects)]
[New trailer + %%EOF]
```

This is how form filling and digital signatures work — the original content is preserved and new data is layered on. It also means "deleted" content may still exist in the file.

### PDF Versions and Features

| Feature | PDF Version |
|---------|-------------|
| Basic text and images | 1.0 (1993) |
| Interactive forms | 1.2 |
| JavaScript | 1.3 |
| Transparency | 1.4 |
| Embedded multimedia | 1.5 |
| AES encryption | 1.6 |
| 3D content | 1.6 |
| XFA forms | 1.5 |
| PDF/A (archival) | Based on 1.4-1.7 |
| PDF 2.0 (ISO 32000-2) | 2.0 (2017) |

---

## Office Open XML (DOCX, XLSX, PPTX)

Microsoft Office's format since 2007. A **ZIP archive** containing XML files, media, and relationships.

### DOCX Structure

```bash
$ unzip -l document.docx
  [Content_Types].xml          ← MIME type registry
  _rels/.rels                  ← Root relationships
  word/document.xml            ← Main document content
  word/styles.xml              ← Style definitions
  word/settings.xml            ← Document settings
  word/fontTable.xml           ← Font declarations
  word/theme/theme1.xml        ← Theme (colors, fonts)
  word/media/image1.png        ← Embedded images
  word/_rels/document.xml.rels ← Document relationships
  docProps/core.xml            ← Dublin Core metadata
  docProps/app.xml             ← Application metadata
```

### Document.xml Content

```xml
<w:document xmlns:w="http://schemas.openxmlformats.org/.../wordprocessingml">
  <w:body>
    <w:p>                              <!-- Paragraph -->
      <w:pPr>                          <!-- Paragraph properties -->
        <w:pStyle w:val="Heading1"/>
      </w:pPr>
      <w:r>                            <!-- Run (text span) -->
        <w:rPr>                        <!-- Run properties -->
          <w:b/>                       <!-- Bold -->
        </w:rPr>
        <w:t>Introduction</w:t>        <!-- Text content -->
      </w:r>
    </w:p>
  </w:body>
</w:document>
```

### XLSX Structure

Spreadsheets split data across multiple XML files:

```
xl/worksheets/sheet1.xml     ← Cell data (row/col/value)
xl/sharedStrings.xml         ← String table (cells reference by index)
xl/styles.xml                ← Number formats, fonts, fills
xl/workbook.xml              ← Sheet names, defined names
```

Cell values reference the shared strings table by index, so repeated strings are stored once:

```xml
<!-- xl/sharedStrings.xml -->
<sst count="3" uniqueCount="2">
  <si><t>Name</t></si>       <!-- index 0 -->
  <si><t>Revenue</t></si>    <!-- index 1 -->
</sst>

<!-- xl/worksheets/sheet1.xml -->
<row r="1">
  <c r="A1" t="s"><v>0</v></c>   <!-- "Name" (string index 0) -->
  <c r="B1" t="s"><v>1</v></c>   <!-- "Revenue" (string index 1) -->
  <c r="B2"><v>50000</v></c>      <!-- Numeric value (no type = number) -->
</row>
```

---

## OpenDocument Format (ODF)

ISO standard, used by LibreOffice and other open-source suites. Also ZIP-based with XML content, but uses different schemas.

| Office XML | ODF Equivalent |
|-----------|----------------|
| `.docx` | `.odt` (text) |
| `.xlsx` | `.ods` (spreadsheet) |
| `.pptx` | `.odp` (presentation) |

Structure is similar: ZIP containing `content.xml`, `styles.xml`, `meta.xml`, `META-INF/manifest.xml`.

---

## EPUB

The standard e-book format. Essentially a **website in a ZIP file** — XHTML pages + CSS + images + metadata.

### EPUB Structure

```bash
$ unzip -l book.epub
  mimetype                     ← Must be first, uncompressed: "application/epub+zip"
  META-INF/container.xml       ← Points to the OPF file
  OEBPS/content.opf            ← Package document (manifest + spine)
  OEBPS/toc.ncx                ← Table of contents (EPUB 2)
  OEBPS/nav.xhtml              ← Navigation document (EPUB 3)
  OEBPS/chapter1.xhtml         ← Content pages
  OEBPS/chapter2.xhtml
  OEBPS/styles/main.css        ← Stylesheets
  OEBPS/images/cover.jpg       ← Images
```

### Key EPUB Files

**content.opf** — The manifest and reading order:

```xml
<package xmlns="http://www.idpf.org/2007/opf" version="3.0">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:title>The Great Novel</dc:title>
    <dc:creator>Author Name</dc:creator>
    <dc:language>en</dc:language>
    <dc:identifier id="uid">isbn:978-0-123456-78-9</dc:identifier>
  </metadata>

  <manifest>
    <item id="ch1" href="chapter1.xhtml" media-type="application/xhtml+xml"/>
    <item id="ch2" href="chapter2.xhtml" media-type="application/xhtml+xml"/>
    <item id="css" href="styles/main.css" media-type="text/css"/>
    <item id="cover" href="images/cover.jpg" media-type="image/jpeg"/>
  </manifest>

  <spine>
    <itemref idref="ch1"/>
    <itemref idref="ch2"/>
  </spine>
</package>
```

**mimetype file** — The `mimetype` entry MUST be the first file in the ZIP, stored without compression, at byte offset 38. This allows identification without full ZIP parsing:

```
Offset    Content
00000000  50 4B 03 04                   ZIP local file header
...
00000026  6D 69 6D 65 74 79 70 65       "mimetype"
00000030  61 70 70 6C 69 63 61 74       "application/epub+zip"
          69 6F 6E 2F 65 70 75 62
          2B 7A 69 70
```

### EPUB 2 vs EPUB 3

| Feature | EPUB 2 | EPUB 3 |
|---------|--------|--------|
| Content | XHTML 1.1 | HTML5 |
| Styling | CSS 2.1 | CSS3 |
| Navigation | NCX (XML) | nav.xhtml (HTML5) |
| Scripting | No | JavaScript (limited) |
| Audio/Video | No | HTML5 `<audio>` / `<video>` |
| MathML | Limited | Full support |
| Accessibility | Basic | WCAG integration |

---

## RTF (Rich Text Format)

Microsoft's legacy document interchange format. Plain-text markup (not binary, not XML):

```
{\rtf1\ansi\deff0
{\fonttbl{\f0 Times New Roman;}}
{\colortbl;\red255\green0\blue0;}
\f0\fs24 Normal text. \b Bold text.\b0  \cf1 Red text.\cf0
\par New paragraph.
}
```

RTF is rarely used for new documents but remains relevant as an interchange format — nearly every word processor can read and write it.

---

## Format Comparison for Developers

| Need | Best Format | Why |
|------|-------------|-----|
| Pixel-perfect printing | PDF | Fixed layout, embeds fonts |
| Programmatic doc generation | PDF (via libraries) | wkhtmltopdf, Puppeteer, WeasyPrint, reportlab |
| Editable business docs | DOCX | Universal Office compatibility |
| Open standard docs | ODF | ISO standard, no vendor lock-in |
| E-books | EPUB | Reflowable, e-reader standard |
| Technical/academic papers | LaTeX → PDF | Best math/citation support |
| Web-first docs | HTML/Markdown | Already web-native |
| Data export (tabular) | CSV or XLSX | Depends on whether formatting matters |

### Programmatic PDF Generation

Common approaches for generating PDFs in code:

| Approach | Libraries | Notes |
|----------|-----------|-------|
| HTML → PDF | Puppeteer, Playwright, WeasyPrint, wkhtmltopdf | Write HTML/CSS, render to PDF |
| Direct PDF API | reportlab (Python), iText (Java), PDFKit (Node) | Full control, more complex |
| Template-based | Typst, LaTeX, Pandoc | Write in markup, compile to PDF |
| Fill forms | pdf-lib (JS), PyPDF, iText | Populate existing PDF templates |

---

## Related

- [[File Formats]] — Parent overview of file format concepts
- [[File Metadata]] — Metadata in documents (author, dates, tracked changes)
- [[Archive and Compression Formats]] — ZIP internals that underlie DOCX/EPUB
- [[Serialization]] — JSON, YAML, and structured data formats
