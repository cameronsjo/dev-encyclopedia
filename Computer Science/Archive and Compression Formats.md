---
title: Archive and Compression Formats
aliases:
  - Compression Formats
  - Archive Formats
  - ZIP Format
  - Compression Algorithms
tags:
  - cs
  - fundamentals
  - file-formats
type: concept
status: complete
difficulty: fundamentals
created: "2026-02-19"
---

# Archive and Compression Formats

How files are bundled together (archiving) and made smaller (compression) — from ZIP internals to modern algorithms like Zstandard and Brotli.

## Archive vs Compression

These are separate concepts, often combined:

| Concept | What It Does | Examples |
|---------|-------------|----------|
| **Archive** | Bundles multiple files into one | TAR, CPIO, AR |
| **Compression** | Reduces file size | gzip, bzip2, zstd, brotli, LZMA |
| **Both** | Bundles + compresses | ZIP, 7z, RAR |

TAR was designed for tape archives and handles **only** archiving. Compression is applied separately:

```bash
# Archive only (no compression)
tar cf archive.tar files/

# Archive + gzip
tar czf archive.tar.gz files/

# Archive + zstandard
tar --zstd -cf archive.tar.zst files/

# Archive + bzip2
tar cjf archive.tar.bz2 files/

# Archive + xz (LZMA2)
tar cJf archive.tar.xz files/
```

---

## Compression Algorithms

### Comparison

| Algorithm | Ratio | Compress Speed | Decompress Speed | Used In |
|-----------|-------|----------------|------------------|---------|
| DEFLATE | Good | Moderate | Fast | ZIP, gzip, PNG, HTTP |
| LZ4 | Low | Very fast | Very fast | Filesystem compression, real-time |
| Zstandard (zstd) | Excellent | Fast | Very fast | Kernel, packaging, databases |
| Brotli | Excellent | Slow | Fast | HTTP (WOFF2, web assets) |
| LZMA/LZMA2 | Best | Very slow | Moderate | 7z, xz |
| bzip2 | Good | Slow | Slow | Legacy, some distro packages |
| LZW | Moderate | Fast | Fast | GIF (legacy) |
| Snappy | Low | Very fast | Very fast | Google internal, Hadoop |

### How LZ77/DEFLATE Works

Most general-purpose compression builds on LZ77, which replaces repeated sequences with back-references:

```
Input:  "ABCABCABCXYZ"
                     ↓
Step 1: Output literal "ABC"
Step 2: See "ABC" repeats → output (distance=3, length=6)
Step 3: Output literal "XYZ"

Compressed: ABC <3,6> XYZ

The decoder reads forward:
  "ABC" → emit as-is
  <3,6> → go back 3 chars, copy 6 chars → "ABCABC"
  "XYZ" → emit as-is
  Result: "ABCABCABCXYZ"
```

DEFLATE combines LZ77 with Huffman coding — after finding repeated patterns, it Huffman-encodes the literals and back-references for additional compression.

### Zstandard (zstd)

Facebook/Meta's modern replacement for gzip. Uses finite state entropy (ANS) instead of Huffman coding and has a dictionary mode for compressing many small items.

```bash
# Compress (default level 3)
zstd file.dat

# Compress with level 19 (max practical)
zstd -19 file.dat

# Train a dictionary on similar files (e.g., JSON logs)
zstd --train samples/* -o dictionary

# Compress using dictionary
zstd --dict dictionary file.json
```

Key advantage: zstd decompression speed is nearly constant regardless of compression level. You can spend more time compressing (once) and decompress quickly (many times).

### Brotli

Google's algorithm optimized for web content. Includes a built-in dictionary of common web strings (HTML tags, CSS properties, JavaScript keywords).

```bash
# Compress for web serving (level 11 = max)
brotli -q 11 styles.css

# Content-Encoding header in HTTP
Content-Encoding: br
```

Typical web asset savings over gzip: 15-25% smaller.

---

## ZIP

The most widely used archive+compression format. Also the foundation for DOCX, XLSX, JAR, APK, EPUB, and many other formats.

### ZIP File Structure

ZIP is unusual — the authoritative file index is at the **end** of the file, not the beginning:

```
┌────────────────────────────────────────────┐
│ Local File Header 1                         │
│   50 4B 03 04 (PK..)  ← signature          │
│   version, flags, compression method        │
│   CRC-32, sizes, filename                   │
│ [File Data 1 - compressed]                  │
├────────────────────────────────────────────┤
│ Local File Header 2                         │
│ [File Data 2 - compressed]                  │
├────────────────────────────────────────────┤
│ ...more files...                            │
├────────────────────────────────────────────┤
│ Central Directory                           │  ← The actual index
│   50 4B 01 02 (PK..)  ← entry signature    │
│   Entry for File 1 (offset, size, name)     │
│   Entry for File 2 (offset, size, name)     │
│   ...                                       │
├────────────────────────────────────────────┤
│ End of Central Directory Record             │
│   50 4B 05 06 (PK..)  ← EOCD signature     │
│   Number of entries                         │
│   Central directory offset                  │
│   Comment                                   │
└────────────────────────────────────────────┘
```

**Why the index is at the end:** ZIP was designed for appending files. You can add files to a ZIP without rewriting the entire archive — just append new local entries and write a new central directory.

### ZIP Hex Walkthrough

```
Offset    Hex                                       Meaning
00000000  50 4B 03 04                               Local file header signature
00000004  14 00                                     Version needed: 2.0
00000006  00 00                                     Flags: none
00000008  08 00                                     Compression: DEFLATE
0000000A  4A 7D                                     Mod time (MS-DOS format)
0000000C  54 59                                     Mod date (MS-DOS format)
0000000E  XX XX XX XX                               CRC-32
00000012  XX XX XX XX                               Compressed size
00000016  XX XX XX XX                               Uncompressed size
0000001A  0A 00                                     Filename length: 10
0000001C  00 00                                     Extra field length: 0
0000001E  68 65 6C 6C 6F 2E 74 78 74 00             "hello.txt"
00000028  [compressed data...]                      DEFLATE'd file content
```

### ZIP Compression Methods

| Value | Method | Notes |
|-------|--------|-------|
| 0 | Stored | No compression (files already compressed) |
| 8 | DEFLATE | Standard, universal support |
| 9 | DEFLATE64 | Larger window, rare |
| 12 | bzip2 | Better ratio, less common |
| 14 | LZMA | 7-Zip format, uncommon in ZIP |
| 93 | Zstandard | Modern, gaining support |
| 95 | XZ (LZMA2) | Very high ratio |

### ZIP-Based Formats

| Format | Extension | Contents |
|--------|-----------|----------|
| Office Open XML | `.docx`, `.xlsx`, `.pptx` | XML + media files |
| Java Archive | `.jar` | `.class` files + manifest |
| Android Package | `.apk` | DEX + resources + manifest |
| EPUB | `.epub` | XHTML + CSS + images |
| OpenDocument | `.odt`, `.ods`, `.odp` | XML + media |
| XPI (Firefox ext) | `.xpi` | Web extension files |
| IPSW (iOS firmware) | `.ipsw` | Firmware images |

---

## TAR (Tape Archive)

Unix archiving format from 1979. No compression — purely bundles files with metadata.

### TAR Header Structure

Each file is preceded by a 512-byte header block:

```
Offset  Size  Field
0       100   Filename (null-terminated)
100     8     File mode (octal ASCII)
108     8     Owner UID (octal ASCII)
116     8     Group GID (octal ASCII)
124     12    File size (octal ASCII)
136     12    Modification time (Unix epoch, octal)
148     8     Header checksum
156     1     Type flag ('0'=file, '5'=directory, '2'=symlink)
157     100   Link target name
257     6     "ustar" magic
263     2     Version "00"
265     32    Owner username
297     32    Group name
329     8     Device major
337     8     Device minor
345     155   Filename prefix (for paths > 100 chars)
500     12    Padding to 512 bytes
```

File data follows immediately, padded to a 512-byte boundary. The archive ends with two consecutive 512-byte blocks of zeros.

**Note:** TAR headers are entirely ASCII-encoded octal numbers, making them partially human-readable in a hex editor.

---

## Gzip

The standard compression wrapper on Unix. Compresses a single stream using DEFLATE.

### Gzip Header

```
1F 8B                  ← Magic number
08                     ← Compression method: DEFLATE
XX                     ← Flags (FTEXT, FHCRC, FEXTRA, FNAME, FCOMMENT)
XX XX XX XX            ← Modification time (Unix epoch, LE)
XX                     ← Extra flags (compression level hint)
XX                     ← OS (0=FAT, 3=Unix, 7=macOS, 11=NTFS)
[optional: original filename, null-terminated]
[optional: comment, null-terminated]
[DEFLATE compressed data]
XX XX XX XX            ← CRC-32 of original data
XX XX XX XX            ← Original size mod 2^32
```

The trailing CRC-32 and size allow integrity verification after decompression.

---

## 7z

7-Zip's native format. Supports multiple compression methods and solid compression (compressing multiple files as a single stream for better ratio).

### 7z Signature

```
37 7A BC AF 27 1C      ← Magic bytes: "7z" + 4 signature bytes
00 04                  ← Format version
[header with offsets to compressed streams and metadata]
```

7z achieves the best compression ratios among common formats by using LZMA2 with large dictionaries and solid compression, at the cost of slower compression speed and higher memory usage.

---

## Choosing a Format

| Scenario | Recommended | Why |
|----------|-------------|-----|
| General file sharing | ZIP | Universal support, every OS handles it natively |
| Unix/Linux packages | `.tar.gz` or `.tar.zst` | Standard convention, preserves permissions/ownership |
| Maximum compression | `.tar.xz` or `.7z` | Best ratios, worth the slower compression |
| Fast compression | `.tar.zst` or `.tar.lz4` | Near-instant compress/decompress |
| Web assets | Brotli (`.br`) | Best ratio for HTTP, built-in web dictionary |
| Incremental backups | TAR (append mode) | Add files without rewriting |
| Cross-platform distribution | ZIP | Zero dependency on any platform |
| Container/Docker layers | gzip or zstd | OCI standard, broad registry support |

---

## Related

- [[File Formats]] — Parent overview of file format concepts
- [[File Metadata]] — Metadata preserved in archives (timestamps, permissions)
- [[Document Formats]] — PDF, DOCX, EPUB (ZIP-based formats)
- [[Build Systems]] — Build tools that produce archives
- [[Deployment]] — Container images and artifact packaging
