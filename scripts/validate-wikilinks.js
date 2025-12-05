#!/usr/bin/env node

/**
 * Validates wikilinks in all markdown files.
 * Ensures all [[wikilinks]] resolve to actual files.
 */

const fs = require('fs');
const path = require('path');
const { glob } = require('glob');

const IGNORED_PATTERNS = [
  'node_modules/**',
  '.obsidian/**',
  'templates/**',
  'docs/**',
  '.git/**',
  'README.md',
  'CLAUDE.md',
];

// Regex to match Obsidian wikilinks: [[link]] or [[link|display]]
const WIKILINK_REGEX = /\[\[([^\]|]+)(?:\|[^\]]+)?\]\]/g;

async function validateWikilinks() {
  // Get all markdown files
  const files = await glob('**/*.md', {
    ignore: IGNORED_PATTERNS,
    cwd: process.cwd(),
  });

  // Build a map of all valid targets (filename without extension, and full paths)
  const validTargets = new Set();
  const fileMap = new Map(); // Maps lowercase filename to actual path

  for (const file of files) {
    // Add the full path without extension
    const withoutExt = file.replace(/\.md$/, '');
    validTargets.add(withoutExt);
    validTargets.add(withoutExt.toLowerCase());

    // Add just the filename without extension
    const basename = path.basename(file, '.md');
    validTargets.add(basename);
    validTargets.add(basename.toLowerCase());

    // Store mapping for suggestions
    fileMap.set(basename.toLowerCase(), file);
  }

  let hasErrors = false;
  const brokenLinks = [];
  const mocWarnings = [];
  const allLinks = [];

  // MOC files are allowed to have links to planned content
  const isMocFile = (file) => file.includes('MOC') || file === 'Home.md';

  // Known intentional non-page links (code examples, etc.)
  const IGNORED_LINKS = ['...slug'];

  for (const file of files) {
    const filePath = path.join(process.cwd(), file);
    let content = fs.readFileSync(filePath, 'utf8');

    // Remove fenced code blocks before scanning for wikilinks
    content = content.replace(/```[\s\S]*?```/g, '');
    // Remove inline code
    content = content.replace(/`[^`]+`/g, '');

    let match;
    while ((match = WIKILINK_REGEX.exec(content)) !== null) {
      const link = match[1].trim();
      allLinks.push({ file, link });

      // Skip intentionally ignored links
      if (IGNORED_LINKS.includes(link)) {
        continue;
      }

      // Normalize the link for comparison
      const normalizedLink = link.toLowerCase();

      // Check various forms of the link
      const isValid =
        validTargets.has(link) ||
        validTargets.has(normalizedLink) ||
        validTargets.has(link.replace(/\.md$/, '')) ||
        validTargets.has(normalizedLink.replace(/\.md$/, ''));

      if (!isValid) {
        // Try to find similar files for suggestions
        const suggestions = findSimilar(link, fileMap);
        const entry = { file, link, suggestions };

        if (isMocFile(file)) {
          // MOC files get warnings, not errors (planned content)
          mocWarnings.push(entry);
        } else {
          brokenLinks.push(entry);
          hasErrors = true;
        }
      }
    }
  }

  // Print results
  console.log(`\nScanned ${files.length} files, found ${allLinks.length} wikilinks\n`);

  if (mocWarnings.length > 0) {
    console.log('Warnings (planned content in MOC files):');
    for (const { file, link } of mocWarnings) {
      console.log(`  ⚠️  ${file}: [[${link}]]`);
    }
    console.log('');
  }

  if (brokenLinks.length > 0) {
    console.log('Broken links:');
    for (const { file, link, suggestions } of brokenLinks) {
      console.log(`  ❌ ${file}: [[${link}]]`);
      if (suggestions.length > 0) {
        console.log(`     Did you mean: ${suggestions.join(', ')}?`);
      }
    }
    console.log('');
  }

  if (hasErrors) {
    console.log(`Found ${brokenLinks.length} broken link(s)\n`);
    process.exit(1);
  } else {
    if (mocWarnings.length > 0) {
      console.log(`All wikilinks valid (${mocWarnings.length} planned links in MOC files)\n`);
    } else {
      console.log('All wikilinks are valid!\n');
    }
    process.exit(0);
  }
}

/**
 * Find similar filenames using simple string matching
 */
function findSimilar(link, fileMap) {
  const normalizedLink = link.toLowerCase();
  const suggestions = [];

  for (const [name, filepath] of fileMap) {
    // Check if names are similar (contain each other or have small edit distance)
    if (
      name.includes(normalizedLink) ||
      normalizedLink.includes(name) ||
      levenshteinDistance(name, normalizedLink) <= 3
    ) {
      suggestions.push(path.basename(filepath, '.md'));
    }
  }

  return suggestions.slice(0, 3); // Return top 3 suggestions
}

/**
 * Simple Levenshtein distance implementation
 */
function levenshteinDistance(a, b) {
  if (a.length === 0) return b.length;
  if (b.length === 0) return a.length;

  const matrix = [];

  for (let i = 0; i <= b.length; i++) {
    matrix[i] = [i];
  }

  for (let j = 0; j <= a.length; j++) {
    matrix[0][j] = j;
  }

  for (let i = 1; i <= b.length; i++) {
    for (let j = 1; j <= a.length; j++) {
      if (b.charAt(i - 1) === a.charAt(j - 1)) {
        matrix[i][j] = matrix[i - 1][j - 1];
      } else {
        matrix[i][j] = Math.min(
          matrix[i - 1][j - 1] + 1,
          matrix[i][j - 1] + 1,
          matrix[i - 1][j] + 1
        );
      }
    }
  }

  return matrix[b.length][a.length];
}

validateWikilinks().catch((err) => {
  console.error('Validation failed:', err);
  process.exit(1);
});
