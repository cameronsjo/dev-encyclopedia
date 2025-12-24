---
title: Compression
aliases:
  - Data Compression
  - Compression Algorithms
  - gzip
  - zstd
tags:
  - cs
  - algorithms
  - performance
type: reference
status: complete
created: "2025-12-18"
---

# Compression

Reducing data size by encoding information more efficiently.

## Overview

| Type | Description | Use Cases |
|------|-------------|-----------|
| **Lossless** | Perfect reconstruction | Text, code, executables |
| **Lossy** | Approximation (smaller) | Images, audio, video |

## Lossless vs Lossy

```
Original: "AAAAAABBBBCCCCCCCC" (18 bytes)

Lossless (RLE):  "6A4B8C" (6 bytes)
                 → Decompresses to exact original

Lossy (example): "6A4B8C" but allows "AAAAAABBBBCCCCCCCD"
                 → Close enough for images/audio
```

## Core Techniques

### Run-Length Encoding (RLE)

**Replace repeated values with count + value.**

```
Input:  AAAAAABBBBCCCCCCCC
Output: 6A4B8C

Input:  WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWW
Output: 12W1B12W3B5W
```

```python
def rle_encode(data: str) -> str:
    result = []
    count = 1
    for i in range(1, len(data)):
        if data[i] == data[i-1]:
            count += 1
        else:
            result.append(f"{count}{data[i-1]}")
            count = 1
    result.append(f"{count}{data[-1]}")
    return ''.join(result)
```

**Best for:** Simple patterns, bitmap images (BMP), fax

### Dictionary Coding (LZ77/LZ78)

**Replace repeated sequences with references to earlier occurrences.**

```
Input:  "abracadabra"

LZ77 process:
Position 0-3: "abra" → output literal "abra"
Position 4:   "c"    → output literal "c"
Position 5-6: "ad"   → output literal "ad"
Position 7-10: "abra" → already seen at position 0!
              → output (offset=7, length=4)

Compressed: "abra" + "c" + "ad" + <7,4>
```

```
┌─────────────────────────────────────────────────────┐
│ Sliding Window (LZ77)                               │
│                                                     │
│  ←── Search Buffer ──→ ←── Look-ahead ──→          │
│  [...already seen...] [next to encode...]          │
│                                                     │
│  Find longest match in search buffer               │
│  Output: (distance back, length) or literal        │
└─────────────────────────────────────────────────────┘
```

**Used by:** gzip, DEFLATE, zstd, LZ4

### Huffman Coding

**Variable-length codes: frequent symbols get shorter codes.**

```
Text: "AAAAABBBCCDE"
Frequencies: A=5, B=3, C=2, D=1, E=1

Fixed-length (3 bits each): 12 × 3 = 36 bits

Huffman tree:
        (12)
       /    \
     A(5)   (7)
           /   \
        B(3)   (4)
              /   \
           C(2)   (2)
                 /   \
               D(1)  E(1)

Huffman codes:
A = 0        (1 bit)   - most frequent
B = 10       (2 bits)
C = 110      (3 bits)
D = 1110     (4 bits)
E = 1111     (4 bits)  - least frequent

Encoded: 5×1 + 3×2 + 2×3 + 1×4 + 1×4 = 25 bits
Savings: 36 - 25 = 11 bits (30% smaller)
```

**Used by:** DEFLATE (gzip), JPEG, MP3

### Arithmetic Coding

**Encode entire message as single number between 0 and 1.**

More efficient than Huffman for skewed distributions.

```
Message: "AABA" with P(A)=0.8, P(B)=0.2

Range starts as [0, 1)

A: [0, 0.8)           → narrow to A's range
A: [0, 0.64)          → narrow again
B: [0.512, 0.64)      → B's portion of current range
A: [0.512, 0.6144)    → final range

Output: any number in [0.512, 0.6144), e.g., 0.55
```

**Used by:** JPEG 2000, H.264/H.265, modern codecs

### Burrows-Wheeler Transform (BWT)

**Rearrange data to group similar characters together, then use RLE/Huffman.**

```
Input: "banana"

1. Create all rotations:
   banana
   ananab
   nanaba
   anaban
   nabana
   abanan

2. Sort alphabetically:
   abanan
   anaban
   ananab
   banana
   nabana
   nanaba

3. Take last column: "nnbaaa"
   (Notice: a's grouped together!)

4. Apply RLE: "2n1b3a" or Huffman
```

**Used by:** bzip2

## Common Algorithms

### Comparison Table

| Algorithm | Ratio | Speed | Use Case |
|-----------|-------|-------|----------|
| **gzip/DEFLATE** | Good | Medium | General, HTTP |
| **zstd** | Excellent | Fast | Modern default |
| **LZ4** | Fair | Very fast | Real-time, games |
| **Brotli** | Excellent | Slow | Web assets |
| **bzip2** | Excellent | Slow | Archival |
| **xz/LZMA** | Best | Very slow | Archival |
| **Snappy** | Fair | Very fast | Databases |

### DEFLATE (gzip, zlib, zip)

```
┌─────────────────────────────────────────────────────┐
│                    DEFLATE                           │
│                                                     │
│  Input → LZ77 → Huffman → Compressed Output         │
│         (find    (variable                          │
│         matches)  length codes)                     │
└─────────────────────────────────────────────────────┘
```

```bash
# gzip (DEFLATE with gzip header)
gzip file.txt              # → file.txt.gz
gzip -d file.txt.gz        # decompress
gzip -9 file.txt           # max compression
gzip -1 file.txt           # fastest

# zlib is DEFLATE with different header (used in PNG, HTTP)
```

```python
import gzip
import zlib

# gzip
with gzip.open('file.txt.gz', 'wt') as f:
    f.write('Hello, World!')

# zlib (raw DEFLATE)
compressed = zlib.compress(b'Hello, World!')
original = zlib.decompress(compressed)
```

### Zstandard (zstd)

**Modern algorithm: better ratio than gzip, faster than gzip.**

```bash
# CLI
zstd file.txt              # → file.txt.zst
zstd -d file.txt.zst       # decompress
zstd -19 file.txt          # max compression (1-19)
zstd -1 file.txt           # fastest
zstd --train *.json        # train dictionary for similar files
```

```python
import zstandard as zstd

# Compress
cctx = zstd.ZstdCompressor(level=3)
compressed = cctx.compress(b'Hello, World!')

# Decompress
dctx = zstd.ZstdDecompressor()
original = dctx.decompress(compressed)

# With dictionary (for small similar files)
dict_data = zstd.train_dictionary(100000, samples)
cctx = zstd.ZstdCompressor(dict_data=dict_data)
```

### LZ4

**Extremely fast, lower ratio. Good for real-time.**

```bash
lz4 file.txt               # → file.txt.lz4
lz4 -d file.txt.lz4        # decompress
lz4 -9 file.txt            # better compression
lz4 -1 file.txt            # fastest (default)
```

```python
import lz4.frame

compressed = lz4.frame.compress(b'Hello, World!')
original = lz4.frame.decompress(compressed)
```

### Brotli

**Designed for web. Best ratio for text/HTML/CSS/JS.**

```bash
brotli file.txt            # → file.txt.br
brotli -d file.txt.br      # decompress
brotli -q 11 file.txt      # max quality (0-11)
```

```python
import brotli

compressed = brotli.compress(b'Hello, World!', quality=4)
original = brotli.decompress(compressed)
```

```nginx
# nginx config
brotli on;
brotli_types text/html text/css application/javascript;
```

### Snappy

**Google's fast compressor. Used in databases.**

```python
import snappy

compressed = snappy.compress(b'Hello, World!')
original = snappy.uncompress(compressed)
```

**Used by:** LevelDB, RocksDB, Kafka, Cassandra

## Benchmarks (Typical)

### Compression Ratio (smaller = better)

| Algorithm | Text | Binary | JSON |
|-----------|------|--------|------|
| None | 1.00 | 1.00 | 1.00 |
| LZ4 | 0.45 | 0.65 | 0.35 |
| Snappy | 0.48 | 0.68 | 0.38 |
| gzip -6 | 0.32 | 0.55 | 0.25 |
| zstd -3 | 0.30 | 0.52 | 0.22 |
| Brotli -4 | 0.28 | 0.50 | 0.20 |
| xz -6 | 0.25 | 0.45 | 0.18 |

### Speed (MB/s, approximate)

| Algorithm | Compress | Decompress |
|-----------|----------|------------|
| LZ4 | 750 | 4000 |
| Snappy | 500 | 1500 |
| zstd -1 | 500 | 1500 |
| zstd -3 | 350 | 1200 |
| gzip -6 | 50 | 400 |
| Brotli -4 | 40 | 400 |
| xz -6 | 10 | 150 |

## HTTP Compression

### Content-Encoding

```http
# Request
GET /api/data HTTP/1.1
Accept-Encoding: gzip, deflate, br

# Response
HTTP/1.1 200 OK
Content-Encoding: gzip
Content-Length: 1234
```

### Priority

| Priority | Encoding | Support |
|----------|----------|---------|
| 1 | `br` (Brotli) | Modern browsers |
| 2 | `gzip` | Universal |
| 3 | `deflate` | Legacy |

### Express.js Example

```javascript
const compression = require('compression');
const express = require('express');

const app = express();
app.use(compression({
  level: 6,           // gzip level
  threshold: 1024,    // min size to compress
  filter: (req, res) => {
    if (req.headers['x-no-compression']) return false;
    return compression.filter(req, res);
  }
}));
```

## File Formats

| Format | Algorithm | Use |
|--------|-----------|-----|
| `.gz` | gzip (DEFLATE) | Single file |
| `.zip` | DEFLATE | Archive + compression |
| `.tar.gz` / `.tgz` | tar + gzip | Unix archives |
| `.zst` | Zstandard | Modern single file |
| `.br` | Brotli | Web assets |
| `.xz` | LZMA2 | High compression |
| `.bz2` | bzip2 | Legacy high compression |
| `.lz4` | LZ4 | Fast compression |
| `.7z` | LZMA/LZMA2 | Best ratio archive |

## When to Use What

| Scenario | Recommendation |
|----------|----------------|
| **HTTP responses** | Brotli (or gzip fallback) |
| **Log files** | zstd or gzip |
| **Database storage** | LZ4 or Snappy |
| **Real-time streaming** | LZ4 |
| **Archival** | xz or zstd -19 |
| **General purpose** | zstd |
| **Maximum compatibility** | gzip |
| **Container images** | zstd (OCI 1.1) |

## Dictionaries

**Pre-shared patterns for better small-file compression.**

```bash
# Train dictionary on sample files
zstd --train samples/*.json -o dict

# Compress with dictionary
zstd --dict dict file.json

# Decompress (needs same dictionary!)
zstd -d --dict dict file.json.zst
```

**Best for:**
- Small similar files (API responses, configs)
- Known structure (JSON schemas)
- Protocol messages

## Streaming Compression

```python
import zstandard as zstd

# Streaming compress
cctx = zstd.ZstdCompressor()
with open('input.txt', 'rb') as fin:
    with open('output.zst', 'wb') as fout:
        cctx.copy_stream(fin, fout)

# Streaming decompress
dctx = zstd.ZstdDecompressor()
with open('input.zst', 'rb') as fin:
    with open('output.txt', 'wb') as fout:
        dctx.copy_stream(fin, fout)
```

```go
// Go with gzip streaming
import (
    "compress/gzip"
    "io"
    "os"
)

func compressFile(src, dst string) error {
    in, _ := os.Open(src)
    defer in.Close()

    out, _ := os.Create(dst)
    defer out.Close()

    gz := gzip.NewWriter(out)
    defer gz.Close()

    _, err := io.Copy(gz, in)
    return err
}
```

## Lossy Compression

### Images

| Format | Algorithm | Use Case |
|--------|-----------|----------|
| **JPEG** | DCT + Huffman | Photos |
| **WebP** | VP8 | Web images |
| **AVIF** | AV1 | Modern web |
| **HEIC** | HEVC | Apple photos |

### Audio

| Format | Algorithm | Bitrate |
|--------|-----------|---------|
| **MP3** | MDCT + Huffman | 128-320 kbps |
| **AAC** | MDCT | 96-256 kbps |
| **Opus** | SILK + CELT | 6-510 kbps |
| **Vorbis** | MDCT | 64-500 kbps |

### Video

| Format | Codec | Efficiency |
|--------|-------|------------|
| **H.264/AVC** | Standard | Good |
| **H.265/HEVC** | 2x H.264 | Better |
| **AV1** | 3x H.264 | Best |
| **VP9** | 2x H.264 | Better |

## Tips

| Tip | Why |
|-----|-----|
| **Compress before encryption** | Encrypted data doesn't compress |
| **Don't double-compress** | JPEG in gzip = bigger |
| **Match algorithm to data** | Text vs binary vs images |
| **Consider CPU cost** | Fast decompress often matters more |
| **Use dictionaries for small files** | Big wins for JSON APIs |
| **Enable HTTP compression** | Easy 70%+ bandwidth savings |

## Related

- [[Big O Notation]] — Algorithm complexity
- [[Networking Fundamentals]] — HTTP compression
- [[Serialization]] — Data formats
- [[Computer Science MOC]] — CS topics
