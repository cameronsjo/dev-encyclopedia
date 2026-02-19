---
title: File Metadata
aliases:
  - EXIF
  - EXIF Data
  - Image Metadata
  - Photo Metadata
  - XMP
tags:
  - cs
  - fundamentals
  - file-formats
  - media
  - security
type: concept
status: complete
difficulty: fundamentals
created: "2026-02-19"
---

# File Metadata

Data about data — how files carry information beyond their primary content, including camera settings, GPS coordinates, authorship, and edit history.

## Why It Matters

- **Privacy** — Photos from smartphones embed GPS coordinates by default. Sharing a photo can reveal your home address, workplace, or travel patterns.
- **Forensics** — Metadata reveals what device created a file, when, and sometimes edit history.
- **Workflow** — Photo editors, DAMs, and media libraries rely on metadata for organization.
- **Security** — Metadata can leak sensitive information. Many organizations strip metadata before publishing.

---

## EXIF (Exchangeable Image File Format)

The dominant metadata standard for photos. Embedded directly in JPEG, TIFF, HEIF, and WebP files.

### Where EXIF Lives in a JPEG

EXIF data is stored in the APP1 marker segment, immediately after the SOI marker:

```
FF D8                    ← SOI (Start of Image)
FF E1 [length]           ← APP1 marker (EXIF container)
  45 78 69 66 00 00      ← "Exif\0\0" identifier
  ┌─────────────────────────────────────────────┐
  │ TIFF Header                                  │
  │   49 49 (II = little-endian)                 │
  │   2A 00 (TIFF magic number 42)               │
  │   08 00 00 00 (offset to first IFD)          │
  ├─────────────────────────────────────────────┤
  │ IFD0 (Image File Directory - main image)     │
  │   [count] [entry] [entry] [entry] ...        │
  │   [pointer to IFD1]                          │
  ├─────────────────────────────────────────────┤
  │ Sub-IFD (EXIF-specific tags)                 │
  │   [detailed camera settings]                 │
  ├─────────────────────────────────────────────┤
  │ GPS IFD (location data)                      │
  │   [latitude, longitude, altitude, timestamp] │
  ├─────────────────────────────────────────────┤
  │ IFD1 (thumbnail image)                       │
  │   [embedded JPEG thumbnail]                  │
  └─────────────────────────────────────────────┘
FF DB ...                ← Rest of JPEG follows
```

### IFD Entry Format

Each EXIF tag is stored as a 12-byte entry in an Image File Directory:

```
┌────────────┬────────────┬────────────┬──────────────────────┐
│ Tag ID     │ Data Type  │ Count      │ Value / Offset       │
│ (2 bytes)  │ (2 bytes)  │ (4 bytes)  │ (4 bytes)            │
└────────────┴────────────┴────────────┴──────────────────────┘
```

If the value fits in 4 bytes, it's stored inline. Otherwise, the field contains an offset pointing to the value elsewhere in the EXIF block.

### Common EXIF Tags

#### Camera & Capture

| Tag ID | Name | Example Value |
|--------|------|---------------|
| 0x010F | Make | `Apple` |
| 0x0110 | Model | `iPhone 15 Pro` |
| 0x829A | ExposureTime | `1/125` |
| 0x829D | FNumber | `f/1.8` |
| 0x8827 | ISOSpeedRatings | `100` |
| 0x9003 | DateTimeOriginal | `2026:01:15 14:30:22` |
| 0x920A | FocalLength | `6.86 mm` |
| 0xA405 | FocalLengthIn35mm | `24 mm` |
| 0xA433 | LensMake | `Apple` |
| 0xA434 | LensModel | `iPhone 15 Pro back triple camera 6.86mm f/1.78` |
| 0x0112 | Orientation | `6` (rotated 90 CW) |
| 0xA002 | PixelXDimension | `4032` |
| 0xA003 | PixelYDimension | `3024` |

#### Image Processing

| Tag ID | Name | Example Value |
|--------|------|---------------|
| 0x0131 | Software | `Adobe Photoshop 25.0` |
| 0x9286 | UserComment | Free-form text |
| 0xA001 | ColorSpace | `1` (sRGB) |
| 0xA300 | FileSource | `3` (digital camera) |
| 0xA301 | SceneType | `1` (directly photographed) |
| 0xA401 | CustomRendered | `0` (normal) or `1` (custom) |
| 0xA402 | ExposureMode | `0` (auto) |
| 0xA403 | WhiteBalance | `0` (auto) |

---

## GPS Metadata

The most privacy-sensitive metadata. Smartphones embed GPS coordinates by default unless the user explicitly disables it.

### GPS IFD Structure

GPS data uses its own set of tags in a dedicated IFD (Image File Directory):

| Tag ID | Name | Format | Example |
|--------|------|--------|---------|
| 0x0000 | GPSVersionID | 4 bytes | `2 3 0 0` |
| 0x0001 | GPSLatitudeRef | ASCII | `N` or `S` |
| 0x0002 | GPSLatitude | 3 rationals | `47/1, 36/1, 2174/100` |
| 0x0003 | GPSLongitudeRef | ASCII | `E` or `W` |
| 0x0004 | GPSLongitude | 3 rationals | `122/1, 19/1, 4522/100` |
| 0x0005 | GPSAltitudeRef | byte | `0` (above sea level) |
| 0x0006 | GPSAltitude | rational | `56/1` (meters) |
| 0x0007 | GPSTimeStamp | 3 rationals | `14/1, 30/1, 22/1` (UTC) |
| 0x001D | GPSDateStamp | ASCII | `2026:01:15` |
| 0x000C | GPSSpeedRef | ASCII | `K` (km/h) |
| 0x000D | GPSSpeed | rational | `0/1` |
| 0x000E | GPSTrackRef | ASCII | `T` (true north) |
| 0x000F | GPSTrack | rational | `275/1` (degrees) |

### Reading GPS Coordinates

GPS latitude and longitude are stored as three rational numbers: degrees, minutes, seconds.

```
GPSLatitude:     47/1, 36/1, 2174/100
GPSLatitudeRef:  N
GPSLongitude:    122/1, 19/1, 4522/100
GPSLongitudeRef: W

Conversion to decimal:
  Lat  = 47 + 36/60 + 21.74/3600 = 47.6060°N
  Long = 122 + 19/60 + 45.22/3600 = 122.3292°W
  → (47.6060, -122.3292) ≈ Seattle, WA
```

### Viewing and Stripping GPS Data

```bash
# View all EXIF data including GPS
exiftool photo.jpg

# View only GPS data
exiftool -GPS* photo.jpg

# Strip all GPS data
exiftool -GPS*= photo.jpg

# Strip ALL metadata
exiftool -all= photo.jpg

# Strip metadata from all JPEGs in a directory
exiftool -all= -overwrite_original *.jpg
```

### Privacy Implications

| Platform | Strips GPS on upload? |
|----------|----------------------|
| Twitter/X | Yes |
| Facebook | Yes (but may use internally) |
| Instagram | Yes |
| Imgur | Yes |
| Discord | No (as of 2024) |
| Email | No |
| Slack | No |
| iMessage | Depends on share settings |
| Google Photos shared links | Configurable |

**Best practice:** Disable GPS tagging in your camera app, or strip metadata before sharing files directly.

---

## EXIF Orientation Tag

One of the most commonly mishandled pieces of metadata. Cameras and phones store photos in sensor orientation and use the Orientation tag to indicate how to display them.

| Value | Transform | Common Cause |
|-------|-----------|-------------|
| 1 | None (normal) | Landscape, home button right |
| 3 | Rotate 180 | Upside down |
| 6 | Rotate 90 CW | Portrait, home button bottom |
| 8 | Rotate 90 CCW | Portrait, home button top |
| 2, 4, 5, 7 | Mirrored variants | Front-facing camera |

**Common bug:** Applications that ignore the Orientation tag display photos sideways or upside down. The pixel data itself is not rotated — only the metadata says how to display it.

---

## XMP (Extensible Metadata Platform)

Adobe's XML-based metadata standard. More flexible than EXIF — supports arbitrary namespaces, nested structures, and arrays.

### Where XMP Lives

| File Type | Location |
|-----------|----------|
| JPEG | APP1 marker (separate from EXIF APP1) |
| PNG | `iTXt` chunk with keyword "XML:com.adobe.xmp" |
| TIFF | Tag 700 in IFD |
| PDF | Metadata stream object |
| MP4 | `uuid` box with XMP UUID |
| Sidecar | `.xmp` file alongside the original |

### XMP Structure

```xml
<x:xmpmeta xmlns:x="adobe:ns:meta/">
  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <rdf:Description
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xmp="http://ns.adobe.com/xap/1.0/"
      xmlns:photoshop="http://ns.adobe.com/photoshop/1.0/"
      xmlns:Iptc4xmpCore="http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/">

      <dc:title>Sunset at Gas Works Park</dc:title>
      <dc:creator>Jane Smith</dc:creator>
      <dc:rights>Copyright 2026 Jane Smith</dc:rights>

      <xmp:CreatorTool>Adobe Lightroom Classic 14.0</xmp:CreatorTool>
      <xmp:CreateDate>2026-01-15T14:30:22-08:00</xmp:CreateDate>
      <xmp:ModifyDate>2026-01-16T09:15:00-08:00</xmp:ModifyDate>
      <xmp:Rating>4</xmp:Rating>

      <photoshop:City>Seattle</photoshop:City>
      <photoshop:State>Washington</photoshop:State>
      <photoshop:Country>United States</photoshop:Country>

      <dc:subject>
        <rdf:Bag>
          <rdf:li>sunset</rdf:li>
          <rdf:li>cityscape</rdf:li>
          <rdf:li>Seattle</rdf:li>
        </rdf:Bag>
      </dc:subject>

    </rdf:Description>
  </rdf:RDF>
</x:xmpmeta>
```

### EXIF vs XMP

| Aspect | EXIF | XMP |
|--------|------|-----|
| Format | Binary (TIFF-based) | XML text |
| Extensibility | Fixed tag set | Arbitrary namespaces |
| Size limit | 64KB in JPEG | Unlimited (practically) |
| Edit history | No | Yes (with sidecar) |
| Readability | Needs parser | Human-readable XML |
| Standardized by | JEITA/CIPA | Adobe (ISO 16684) |
| Typical use | Camera capture data | Post-processing, cataloging |

In practice, both coexist — EXIF for camera data, XMP for editorial metadata and tags.

---

## Audio Metadata: ID3

The metadata standard for MP3 files, though some fields have been adopted more broadly.

### ID3v1 (Legacy)

Fixed-size block at the **end** of the MP3 file:

```
Offset from EOF   Size   Field
-128               3     Tag identifier: "TAG"
-125              30     Title
-95               30     Artist
-65               30     Album
-35                4     Year
-31               30     Comment (or 28 + track number in v1.1)
-1                 1     Genre (index into predefined list)
```

Severely limited: 30 characters per field, 80 predefined genres, no album art, no Unicode.

### ID3v2

Variable-length block at the **start** of the MP3 file (before audio data):

```
49 44 33           ← "ID3" magic bytes
03 00              ← Version: 2.3.0
00                 ← Flags
XX XX XX XX        ← Tag size (syncsafe integer)
[Frame] [Frame] ... ← Variable number of tagged frames
```

Each frame:

```
┌────────────┬────────────┬───────┬──────────────────┐
│ Frame ID   │ Size       │ Flags │ Data             │
│ (4 bytes)  │ (4 bytes)  │ (2B)  │ (variable)       │
└────────────┴────────────┴───────┴──────────────────┘
```

Common frame IDs:

| Frame | Name | Contents |
|-------|------|----------|
| `TIT2` | Title | Song name |
| `TPE1` | Lead Artist | Primary performer |
| `TALB` | Album | Album name |
| `TRCK` | Track | Track number (e.g., "3/12") |
| `TDRC` | Recording Date | ISO 8601 date |
| `TCON` | Genre | Genre name or "(index)" |
| `COMM` | Comments | Description + text |
| `APIC` | Attached Picture | Cover art (embedded JPEG or PNG) |
| `USLT` | Unsynced Lyrics | Song lyrics |
| `TXXX` | User-defined text | Custom key-value pairs |

### Syncsafe Integers

ID3v2 uses "syncsafe" integers where the high bit of each byte is always 0. This prevents the metadata from being mistaken for an MP3 sync word (`FF FB`, `FF FA`, etc.):

```
Normal integer:     0x00021000 = 135168
As syncsafe bytes:  00 04 20 00
  Bit layout: 0AAAAAAA 0BBBBBBB 0CCCCCCC 0DDDDDDD
  Reassemble: AAAAAAABBBBBBBCCCCCCCDDDDDDD = 28-bit value
```

### Vorbis Comments (FLAC, OGG)

An alternative to ID3, used by open formats. Simple key=value pairs with no predefined tag set:

```
ARTIST=Pink Floyd
ALBUM=The Dark Side of the Moon
TITLE=Time
TRACKNUMBER=4
DATE=1973
GENRE=Progressive Rock
```

Stored in FLAC's `VORBIS_COMMENT` metadata block or OGG's header packets.

---

## Video Metadata

### MP4/MOV Metadata

MP4 files (based on ISO Base Media File Format) store metadata in "boxes" (also called "atoms"):

```
[moov box]
  └─[udta box]           ← User data
      └─[meta box]       ← Metadata container
          ├─[hdlr]       ← Handler (declares metadata type)
          ├─[ilst box]   ← iTunes-style metadata
          │   ├─©nam     ← Title
          │   ├─©ART     ← Artist
          │   ├─©day     ← Year
          │   ├─©gen     ← Genre
          │   └─covr     ← Cover art
          └─[XMP_ box]   ← XMP metadata (if present)

  └─[trak box]           ← Track metadata
      └─[mdia box]
          └─[minf box]   ← Media info (codec, dimensions, bitrate)
```

GPS data in MP4 can appear in multiple places:

- `©xyz` atom in `udta` — Apple's location format (`+47.6060-122.3292/`)
- XMP metadata box
- GPS track in a dedicated metadata track

### Inspecting Video Metadata

```bash
# Full metadata dump
ffprobe -v quiet -show_format -show_streams video.mp4

# Just format-level metadata
ffprobe -v quiet -show_entries format_tags video.mp4

# EXIF/XMP from video
exiftool video.mp4
```

---

## Document Metadata

### PDF Metadata

PDF files carry metadata in two places:

**Info Dictionary** (legacy):

```
<< /Title (Quarterly Report)
   /Author (Jane Smith)
   /Subject (Q4 2025 Financial Results)
   /Creator (Microsoft Word)
   /Producer (macOS Quartz PDFContext)
   /CreationDate (D:20260115143022-08'00')
   /ModDate (D:20260116091500-08'00')
>>
```

**XMP Metadata Stream** (modern):

Embedded as an XML stream object, following the same XMP structure described above. Most modern PDF tools write both for backward compatibility.

```bash
# View PDF metadata
pdfinfo document.pdf
exiftool document.pdf

# Remove metadata
exiftool -all= document.pdf
qpdf --linearize --replace-input document.pdf  # also removes hidden data
```

### Office Documents (DOCX, XLSX, PPTX)

These are ZIP archives containing XML files. Metadata lives in:

- `docProps/core.xml` — Dublin Core metadata (title, author, dates)
- `docProps/app.xml` — Application metadata (word count, company, app version)
- `docProps/custom.xml` — Custom properties

```xml
<!-- docProps/core.xml -->
<cp:coreProperties>
  <dc:title>Project Proposal</dc:title>
  <dc:creator>Jane Smith</dc:creator>
  <cp:lastModifiedBy>John Doe</cp:lastModifiedBy>
  <dcterms:created>2026-01-15T14:30:22Z</dcterms:created>
  <dcterms:modified>2026-01-16T09:15:00Z</dcterms:modified>
  <cp:revision>7</cp:revision>
</cp:coreProperties>
```

**Hidden data risks in Office docs:**

- Track changes / revision history
- Comments and annotations
- Hidden rows/columns in spreadsheets
- Embedded file paths (can reveal username and directory structure)
- Previous authors visible in revision metadata

---

## Metadata Security Checklist

| Risk | Mitigation |
|------|-----------|
| GPS coordinates in photos | Disable location in camera settings, strip before sharing |
| Author/username in documents | Use Document Inspector (Office) or `exiftool -all=` |
| Camera serial number in EXIF | Strip EXIF if publishing anonymously |
| Embedded thumbnails | EXIF thumbnails may show original un-cropped image |
| Edit history in XMP | Strip XMP sidecar data |
| File paths in Office XML | Run Document Inspector before distributing |
| Creation timestamps | Strip or normalize if anonymity is needed |

---

## Tools

| Tool | Purpose | Formats |
|------|---------|---------|
| `exiftool` | Swiss army knife for metadata | JPEG, PNG, TIFF, MP4, PDF, Office, 400+ formats |
| `identify -verbose` | ImageMagick metadata dump | All image formats |
| `ffprobe` | Media container metadata | MP4, MKV, WebM, audio |
| `pdfinfo` | PDF metadata | PDF |
| `mat2` | Metadata removal tool | Images, docs, audio, video |
| `jhead` | JPEG header manipulation | JPEG |

---

## Related

- [[File Formats]] — Parent overview of file format concepts
- [[Image Formats]] — JPEG, PNG, GIF, WebP format internals
- [[Audio and Video Formats]] — Codec and container formats
- [[Document Formats]] — PDF, Office XML, EPUB structure
- [[Web Security]] — Privacy and information leakage
