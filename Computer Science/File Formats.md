---
title: File Formats
aliases:
  - File Format
  - Binary Formats
  - Data Formats
tags:
  - cs
  - fundamentals
  - file-formats
type: concept
status: complete
difficulty: fundamentals
created: "2026-02-19"
---

# File Formats

How computers organize bytes into meaningful structures — from plain text to images, archives, and executables.

## Why It Matters

Every file is just bytes. The **format** defines what those bytes mean. Understanding file formats helps you:

- Debug corrupt files by reading raw hex
- Choose the right image/video/document format for a use case
- Build parsers, converters, and tooling
- Understand security implications (metadata leaks, polyglot attacks)

---

## Anatomy of a Binary File

Most binary files follow a common structural pattern:

```
┌──────────────────────────────────────────────────┐
│  Magic Bytes / Signature   (identifies format)   │
├──────────────────────────────────────────────────┤
│  Header                    (metadata, offsets)    │
├──────────────────────────────────────────────────┤
│  Body / Chunks / Segments  (actual data)          │
├──────────────────────────────────────────────────┤
│  Trailer / Footer          (checksums, EOF mark)  │
└──────────────────────────────────────────────────┘
```

### Magic Bytes

The first few bytes of a file that identify its format. Operating systems and tools use these to detect file types regardless of extension.

| Format | Magic Bytes (hex) | ASCII (if readable) |
|--------|-------------------|---------------------|
| PNG | `89 50 4E 47 0D 0A 1A 0A` | `.PNG....` |
| JPEG | `FF D8 FF` | n/a |
| GIF87a | `47 49 46 38 37 61` | `GIF87a` |
| GIF89a | `47 49 46 38 39 61` | `GIF89a` |
| PDF | `25 50 44 46 2D` | `%PDF-` |
| ZIP | `50 4B 03 04` | `PK..` |
| SQLite | `53 51 4C 69 74 65 20 66 6F 72 6D 61 74 20 33 00` | `SQLite format 3.` |
| ELF (Linux binary) | `7F 45 4C 46` | `.ELF` |
| Mach-O (macOS) | `FE ED FA CE` or `FE ED FA CF` | n/a |
| PE (Windows .exe) | `4D 5A` | `MZ` |
| WebAssembly | `00 61 73 6D` | `.asm` |
| FLAC | `66 4C 61 43` | `fLaC` |
| MP3 (ID3v2) | `49 44 33` | `ID3` |
| OGG | `4F 67 67 53` | `OggS` |
| RIFF (WAV/AVI) | `52 49 46 46` | `RIFF` |
| Gzip | `1F 8B` | n/a |
| Bzip2 | `42 5A 68` | `BZh` |
| 7z | `37 7A BC AF 27 1C` | `7z...` |
| TIFF (LE) | `49 49 2A 00` | `II*.` |
| TIFF (BE) | `4D 4D 00 2A` | `MM.*` |

The `file` command on Unix uses these signatures (via libmagic) to identify files:

```bash
$ file photo.jpg
photo.jpg: JPEG image data, JFIF standard 1.01, resolution (DPI)...
$ file mystery_file
mystery_file: ELF 64-bit LSB executable, x86-64
```

---

## Format Categories

### Text-Based Formats

Stored as human-readable character sequences. Can be opened in any text editor.

| Format | Structure | Use Case |
|--------|-----------|----------|
| CSV/TSV | Delimited rows | Tabular data exchange |
| JSON | Key-value tree | APIs, config |
| YAML | Indented key-value | Config, CI/CD |
| TOML | INI-like sections | Config (Cargo, pyproject) |
| XML | Nested tags | Legacy APIs, SOAP, SVG, HTML |
| Markdown | Lightweight markup | Docs, READMEs |
| Protocol Buffers (text) | Schema definition | `.proto` files |

**Plain text is not always simple.** Encoding matters — [[Character Encoding]] (UTF-8 vs UTF-16 vs ASCII) determines how characters map to bytes. A BOM (`EF BB BF` in UTF-8) can appear at the start of text files, which some tools mishandle.

### Binary Formats

Not human-readable. Require specialized parsers or hex editors.

| Category | Examples | See Also |
|----------|----------|----------|
| Images | JPEG, PNG, GIF, WebP, AVIF, TIFF, BMP | [[Image Formats]] |
| Audio | MP3, FLAC, WAV, AAC, OGG, Opus | [[Audio and Video Formats]] |
| Video | MP4, WebM, MKV, AVI, MOV | [[Audio and Video Formats]] |
| Archives | ZIP, tar, gzip, 7z, brotli, zstd | [[Archive and Compression Formats]] |
| Documents | PDF, DOCX, XLSX, ODF | [[Document Formats]] |
| Databases | SQLite, LevelDB | [[Database Engines]] |
| Executables | ELF, PE, Mach-O, WASM | — |
| Serialization | Protobuf, MessagePack, CBOR, FlatBuffers | [[Serialization]] |

### Container Formats

Some formats are actually **containers** that hold multiple formats inside:

| Container | Contains | Notes |
|-----------|----------|-------|
| ZIP | Arbitrary files | Also the basis for DOCX, XLSX, JAR, APK, EPUB |
| MP4 (MPEG-4 Part 14) | Video + audio + subtitles + metadata | ISO base media file format |
| MKV (Matroska) | Any codec combination | Extremely flexible |
| RIFF | Chunks of typed data | WAV (audio), AVI (video) |
| OGG | Vorbis, Opus, Theora streams | Open container format |
| TAR | Files + directory structure | No compression (pair with gzip/bzip2) |
| TIFF | Multiple images + metadata | Can embed JPEG-compressed frames |

**ZIP-based formats** are surprisingly common. You can rename and unzip them:

```bash
$ cp document.docx document.zip && unzip document.zip
Archive:  document.zip
  inflating: [Content_Types].xml
  inflating: _rels/.rels
  inflating: word/document.xml
  inflating: word/styles.xml
  ...
```

Same applies to `.jar`, `.apk`, `.epub`, `.odt`, `.xlsx`.

---

## Metadata

Files carry metadata beyond their primary content — creation dates, authorship, device info, and notably **GPS coordinates** in photos.

See [[File Metadata]] for deep coverage of EXIF, XMP, ID3, and other metadata systems.

### Quick Metadata Overview

| Domain | Standard | Found In | Notable Fields |
|--------|----------|----------|----------------|
| Photos | EXIF | JPEG, TIFF, HEIF | GPS coordinates, camera model, exposure settings |
| Photos | XMP | JPEG, PNG, PDF, TIFF | Extensible, XML-based, edit history |
| Photos | IPTC-IIM | JPEG, TIFF | Caption, credit, copyright (press/journalism) |
| Audio | ID3v2 | MP3 | Artist, album, track, cover art |
| Audio | Vorbis Comment | FLAC, OGG | Flexible key-value tags |
| Video | MP4 metadata | MP4, MOV | Duration, codec info, GPS, creation time |
| Documents | PDF metadata | PDF | Author, title, creation tool, XMP |
| Archives | Filesystem metadata | ZIP, TAR | File permissions, timestamps, paths |

---

## Endianness

Binary formats must decide the **byte order** for multi-byte values:

| Term | Order | Example (0x01020304) | Common In |
|------|-------|----------------------|-----------|
| Big-endian (BE) | MSB first | `01 02 03 04` | Network protocols, TIFF (Motorola), Java `.class` |
| Little-endian (LE) | LSB first | `04 03 02 01` | x86/x64, TIFF (Intel), WASM, most modern formats |

TIFF files declare their endianness in the first two bytes: `II` = Intel (LE), `MM` = Motorola (BE). Many modern formats standardize on little-endian to match dominant CPU architecture.

---

## Hex Dump Walkthrough

Reading a hex dump is the most direct way to understand file structure. Here is the start of a real PNG file:

```
Offset    00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F   ASCII
00000000  89 50 4E 47 0D 0A 1A 0A  00 00 00 0D 49 48 44 52   .PNG........IHDR
00000010  00 00 02 00 00 00 02 00  08 06 00 00 00 F4 78 D4   ..............x.
00000020  FA 00 00 00 04 73 42 49  54 08 08 08 08 7C 08 64   .....sBIT....|.d
```

Breaking this down:

```
89 50 4E 47 0D 0A 1A 0A   ← PNG signature (magic bytes)
00 00 00 0D                ← Chunk length: 13 bytes
49 48 44 52                ← Chunk type: "IHDR" (image header)
00 00 02 00                ← Width: 512px
00 00 02 00                ← Height: 512px
08                         ← Bit depth: 8
06                         ← Color type: 6 (RGBA)
00                         ← Compression: deflate
00                         ← Filter method: adaptive
00                         ← Interlace: none
```

See [[Image Formats]] for full format breakdowns of JPEG, PNG, GIF, WebP, and more.

---

## Common Patterns Across Formats

### Chunked / Tagged Structure

Many formats organize data as a sequence of labeled chunks, each with a type and length:

```
┌──────────┬──────────┬──────────────────────┐
│ Type/Tag │  Length   │   Data (Length bytes) │
│ (4 bytes)│ (4 bytes)│                       │
└──────────┴──────────┴──────────────────────┘
```

Used by: PNG, RIFF (WAV/AVI), IFF, TIFF (IFDs), MP4 (atoms/boxes)

### Offset Tables / Index

Rather than reading sequentially, some formats include an index pointing to data locations:

- **ZIP** — Central directory at end of file points to each file entry
- **PDF** — Cross-reference table (xref) maps objects to byte offsets
- **SQLite** — B-tree page index for row lookup
- **ELF** — Section header table and program header table

### Compression

Most modern binary formats compress their payload:

| Algorithm | Used By | Ratio | Speed |
|-----------|---------|-------|-------|
| DEFLATE | PNG, ZIP, gzip, HTTP | Good | Moderate |
| LZ77/LZ78 | GIF (LZW), many others | Moderate | Fast |
| Brotli | Web (WOFF2, HTTP) | Excellent | Slow compress, fast decompress |
| Zstandard | Newer archives, databases | Excellent | Fast |
| LZMA/LZMA2 | 7z, xz | Best ratio | Slowest |

---

## Tools for Inspecting Files

| Tool | Purpose |
|------|---------|
| `xxd` | Hex dump with ASCII sidebar |
| `hexdump -C` | Classic hex dump |
| `file` | Identify format via magic bytes |
| `exiftool` | Read/write image and media metadata |
| `ffprobe` | Inspect audio/video container structure |
| `pdfinfo` | PDF metadata |
| `zipinfo` | ZIP archive contents |
| `readelf` | ELF binary structure |
| `otool` | Mach-O binary structure (macOS) |
| `strings` | Extract ASCII strings from binary |
| `binwalk` | Scan for embedded files/signatures |

---

## Related

- [[Image Formats]] — JPEG, PNG, GIF, WebP, AVIF deep dive
- [[File Metadata]] — EXIF, GPS, XMP, ID3 metadata systems
- [[Audio and Video Formats]] — Codecs, containers, streaming
- [[Archive and Compression Formats]] — ZIP, tar, gzip, zstd, brotli
- [[Document Formats]] — PDF, Office XML, EPUB internals
- [[Serialization]] — Protobuf, MessagePack, structured binary data
- [[Character Encoding]] — UTF-8, ASCII, Unicode
- [[Database Engines]] — SQLite file format and others
