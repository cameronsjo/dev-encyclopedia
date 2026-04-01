---
title: Audio and Video Formats
aliases:
  - Video Formats
  - Audio Formats
  - Media Formats
  - Codecs
  - Video Codecs
  - Audio Codecs
tags:
  - cs
  - fundamentals
  - file-formats
  - media
type: concept
status: complete
difficulty: fundamentals
created: "2026-02-19"
---

# Audio and Video Formats

How audio and video are encoded, compressed, and packaged — the distinction between codecs (compression) and containers (packaging).

## Codec vs Container

The most important concept in media formats: **codecs** and **containers** are separate things.

| Concept | What It Does | Examples |
|---------|-------------|----------|
| **Codec** | Compresses/decompresses audio or video data | H.264, H.265, AV1, VP9, AAC, Opus |
| **Container** | Packages codec streams + metadata into a file | MP4, MKV, WebM, AVI, MOV, OGG |

A container holds one or more streams (video, audio, subtitles, metadata) that can each use a different codec:

```
┌─ MP4 Container ──────────────────────────────┐
│                                               │
│  Stream 0: Video  → H.264 codec, 1080p 24fps │
│  Stream 1: Audio  → AAC codec, 48kHz stereo   │
│  Stream 2: Audio  → AAC codec, 48kHz 5.1      │
│  Stream 3: Subtitle → SRT text                │
│  Metadata: title, duration, GPS, chapters     │
│                                               │
└───────────────────────────────────────────────┘
```

---

## Video Codecs

### Codec Comparison

| Codec | Standard | Compression | Licensing | Browser Support | Typical Use |
|-------|----------|-------------|-----------|-----------------|-------------|
| H.264 (AVC) | MPEG-4 Part 10 | Good | Patented (MPEG LA) | Universal | Web, streaming, Blu-ray |
| H.265 (HEVC) | MPEG-H Part 2 | ~50% better than H.264 | Patented (expensive) | Safari, some others | 4K broadcast, Apple ecosystem |
| AV1 | Alliance for Open Media | ~30% better than H.265 | Royalty-free | ~95% browsers | YouTube, Netflix, web |
| VP9 | Google | ~similar to H.265 | Royalty-free | ~97% browsers | YouTube (legacy) |
| VP8 | Google | ~similar to H.264 | Royalty-free | Wide | WebRTC (legacy) |
| AV1 | AOMedia | Best current ratio | Royalty-free | Growing | Next-gen streaming |
| ProRes | Apple | Visually lossless | Proprietary | N/A | Professional editing |

### How Video Compression Works

Video codecs exploit three types of redundancy:

**Spatial** — Within a single frame, nearby pixels are similar (same as image compression).

**Temporal** — Consecutive frames are mostly identical. Instead of storing every pixel for every frame, store the differences (motion vectors + residuals).

**Perceptual** — Human vision is less sensitive to certain details. Quantize aggressively in areas the eye won't notice.

### Frame Types

| Type | Name | Description | Size |
|------|------|-------------|------|
| I-frame | Intra | Complete image (like a JPEG). Seek point. | Largest |
| P-frame | Predicted | References previous frames. Stores only differences. | Medium |
| B-frame | Bidirectional | References both past and future frames. | Smallest |

```
I ← P ← P ← B ← B ← P ← P ← I ← P ← P ...
▲                              ▲
Keyframe (seekable)            Keyframe (seekable)
└──────── GOP (Group of Pictures) ────────┘
```

**GOP (Group of Pictures)** — The sequence between keyframes. Longer GOPs = better compression but slower seeking. Streaming services typically use 2-4 second GOPs.

---

## Audio Codecs

### Codec Comparison

| Codec | Type | Bitrate Range | Quality | Typical Use |
|-------|------|---------------|---------|-------------|
| MP3 (MPEG-1 Layer 3) | Lossy | 128-320 kbps | Good | Legacy music distribution |
| AAC (Advanced Audio) | Lossy | 96-256 kbps | Better than MP3 | Streaming, Apple, YouTube |
| Opus | Lossy | 6-510 kbps | Best lossy codec | VoIP, WebRTC, streaming |
| Vorbis | Lossy | 64-500 kbps | Good | OGG containers, games |
| FLAC | Lossless | 800-1400 kbps | Perfect | Archival, audiophile |
| ALAC | Lossless | 800-1400 kbps | Perfect | Apple ecosystem |
| WAV/PCM | Uncompressed | 1411 kbps (CD) | Perfect | Recording, editing |
| AC-3 (Dolby Digital) | Lossy | 192-640 kbps | Good | DVD, Blu-ray, streaming surround |

### How Audio Compression Works (MP3)

```mermaid
graph LR
    A[PCM Audio] --> B[Subband Filter]
    B --> C[Psychoacoustic Model]
    C --> D[Quantization]
    D --> E[Huffman Encoding]
    E --> F[MP3 Frames]

    style A fill:#E8E8E8
    style F fill:#90EE90
```

1. **Subband filtering** — Split audio into 32 frequency subbands using a polyphase filter bank.
2. **MDCT** — Modified Discrete Cosine Transform on each subband for finer frequency resolution.
3. **Psychoacoustic model** — Determine which frequencies are inaudible due to masking:
   - **Frequency masking:** A loud tone makes nearby quieter tones inaudible
   - **Temporal masking:** A loud sound masks softer sounds just before and after it
4. **Quantization** — Allocate bits based on the psychoacoustic model. Inaudible frequencies get fewer (or zero) bits.
5. **Huffman encoding** — Entropy-code the quantized values.

### MP3 Frame Structure

MP3 is a **frame-based** format. Each frame is independently decodable (enabling seeking and streaming):

```
FF FB                    ← Sync word (11 bits all 1s) + header bits
  Bits 12-13: MPEG version (11 = MPEG1)
  Bits 14-15: Layer (01 = Layer III)
  Bit  16:    CRC protection
  Bits 17-20: Bitrate index
  Bits 21-22: Sample rate (00 = 44100 Hz)
  Bit  23:    Padding
  Bit  24:    Private
  Bits 25-26: Channel mode (00 = stereo)
[Side information]       ← Huffman table selections, scalefactors
[Main data]              ← Huffman-coded frequency data
```

Each frame at 128 kbps/44.1 kHz contains 1152 audio samples (~26ms of audio).

---

## Container Formats

### Container Comparison

| Container | Extension | Video Codecs | Audio Codecs | Features | Common Use |
|-----------|-----------|-------------|-------------|----------|------------|
| MP4 | `.mp4`, `.m4a`, `.m4v` | H.264, H.265, AV1 | AAC, AC-3, Opus | Chapters, subtitles, metadata | Web, streaming |
| MKV | `.mkv`, `.mka` | Anything | Anything | Most flexible, multiple tracks | Desktop media |
| WebM | `.webm` | VP8, VP9, AV1 | Vorbis, Opus | Web-optimized subset of MKV | Web video |
| AVI | `.avi` | Legacy codecs | PCM, MP3 | Simple but limited | Legacy |
| MOV | `.mov` | H.264, H.265, ProRes | AAC, ALAC | Apple's MP4 variant | Apple ecosystem, editing |
| OGG | `.ogg`, `.ogv` | Theora | Vorbis, Opus | Open standard | Open source |
| FLAC | `.flac` | N/A | FLAC only | Lossless audio | Audiophile, archival |
| WAV | `.wav` | N/A | PCM (usually) | RIFF-based, uncompressed | Recording, editing |

### MP4 Box Structure

MP4 files are organized as nested "boxes" (atoms), each with a type and size:

```
Offset    Hex                          Meaning
00000000  00 00 00 20                  Box size: 32 bytes
00000004  66 74 79 70                  Box type: "ftyp" (file type)
00000008  69 73 6F 6D                  Major brand: "isom"
0000000C  00 00 02 00                  Minor version: 512
00000010  69 73 6F 6D 69 73 6F 32     Compatible: "isomiso2"
00000018  61 76 63 31 6D 70 34 31     Compatible: "avc1mp41"
```

Key boxes:

```
ftyp        ← File type / brand declaration
moov        ← Movie metadata (MUST exist)
├── mvhd    ← Movie header (duration, timescale)
├── trak    ← Track (one per stream)
│   ├── tkhd  ← Track header (dimensions, duration)
│   └── mdia  ← Media data
│       ├── mdhd  ← Media header (timescale, language)
│       ├── hdlr  ← Handler (video/audio/subtitle)
│       └── minf  ← Media information
│           └── stbl  ← Sample table (codec config, offsets, sizes)
└── udta    ← User data / metadata
mdat        ← Actual compressed media data (bulk of the file)
```

**Fast-start (web streaming):** The `moov` box must appear **before** `mdat` for progressive download to work. Videos encoded without this require the entire file to download before playback starts. Fix with:

```bash
ffmpeg -i input.mp4 -movflags +faststart output.mp4
```

### RIFF / WAV Structure

WAV files use the RIFF container — a simple chunk-based format:

```
52 49 46 46  [file size-8]   ← "RIFF" + remaining size
57 41 56 45                  ← "WAVE" format identifier

66 6D 74 20  [chunk size]    ← "fmt " chunk (audio format)
  01 00                      ← Format: 1 (PCM)
  02 00                      ← Channels: 2 (stereo)
  44 AC 00 00                ← Sample rate: 44100
  10 B1 02 00                ← Byte rate: 176400
  04 00                      ← Block align: 4
  10 00                      ← Bits per sample: 16

64 61 74 61  [chunk size]    ← "data" chunk
  [PCM audio samples...]     ← Raw audio data
```

At 16-bit stereo 44.1 kHz (CD quality), uncompressed audio is ~10 MB per minute.

---

## Streaming Formats

Streaming video uses segmented delivery rather than single-file download:

| Protocol | Format | Segments | Use Case |
|----------|--------|----------|----------|
| HLS | `.m3u8` playlist + `.ts` or `.mp4` segments | 2-10 second chunks | Apple, Safari, most CDNs |
| DASH | `.mpd` manifest + `.mp4` segments | 2-10 second chunks | Cross-platform, YouTube |
| WebRTC | Real-time packets | Per-frame | Video calls, live P2P |

### Adaptive Bitrate

Both HLS and DASH support **adaptive bitrate streaming** — multiple quality levels encoded, client switches based on bandwidth:

```
Master Playlist (HLS):
  → 1080p @ 5 Mbps  (strong connection)
  → 720p  @ 2.5 Mbps
  → 480p  @ 1 Mbps
  → 360p  @ 500 kbps (weak connection)

Client monitors download speed and switches quality
between segments to minimize buffering.
```

---

## Practical Reference

### Common Web Recommendations

| Content | Format | Codec | Why |
|---------|--------|-------|-----|
| Video (broad support) | MP4 | H.264 + AAC | Universal browser support |
| Video (modern) | MP4 or WebM | AV1 + Opus | Best compression, royalty-free |
| Audio (music) | MP4 | AAC | Small, good quality |
| Audio (speech) | WebM or OGG | Opus | Best at low bitrates |
| Audio (lossless) | FLAC | FLAC | Open, widely supported |
| Live/real-time | WebRTC | VP8/VP9/AV1 + Opus | Low latency |

### Useful Commands

```bash
# Inspect media file
ffprobe -v quiet -show_format -show_streams input.mp4

# Convert video codec
ffmpeg -i input.mov -c:v libx264 -c:a aac output.mp4

# Extract audio from video
ffmpeg -i video.mp4 -vn -c:a copy audio.m4a

# Re-mux without re-encoding (change container)
ffmpeg -i input.mkv -c copy output.mp4

# Add fast-start for web streaming
ffmpeg -i input.mp4 -c copy -movflags +faststart output.mp4
```

---

## Related

- [[File Formats]] — Parent overview of file format concepts
- [[File Metadata]] — EXIF, ID3, XMP metadata systems
- [[Image Formats]] — Still image format internals
- [[Archive and Compression Formats]] — Compression algorithms shared with media
