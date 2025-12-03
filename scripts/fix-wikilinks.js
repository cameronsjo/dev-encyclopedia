#!/usr/bin/env node

/**
 * Fixes common wikilink issues:
 * - Removes backslashes from links like [[languages/Java\]]
 * - Fixes path-prefixed links to use just the filename
 * - Fixes known renames (.NET MAUI -> dotNET MAUI)
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
];

// Known link replacements
const LINK_REPLACEMENTS = {
  '.NET MAUI': 'dotNET MAUI',
  'C Sharp': 'C Sharp',  // Keep as-is, we'll create a page for it
};

// Links that should strip the path prefix and just use the filename
const PATH_STRIP_PATTERNS = [
  /^languages\/(.+?)\\?$/,
  /^domains\/(.+?)\\?$/,
  /^frameworks\/(.+?)\\?$/,
  /^tools\/(.+?)\\?$/,
  /^cs\/(.+?)\\?$/,
  /^ml\/(.+?)\\?$/,
  /^math\/(.+?)\\?$/,
];

async function fixWikilinks() {
  const files = await glob('**/*.md', {
    ignore: IGNORED_PATTERNS,
    cwd: process.cwd(),
  });

  let totalFixes = 0;

  for (const file of files) {
    const filePath = path.join(process.cwd(), file);
    let content = fs.readFileSync(filePath, 'utf8');
    let modified = false;
    let fixes = [];

    // Fix path-prefixed links with backslashes
    for (const pattern of PATH_STRIP_PATTERNS) {
      const regex = new RegExp(`\\[\\[(${pattern.source})(?:\\|([^\\]]+))?\\]\\]`, 'g');
      content = content.replace(regex, (match, fullPath, displayText) => {
        const pathMatch = fullPath.match(pattern);
        if (pathMatch) {
          const filename = pathMatch[1].replace(/\\$/, '');
          const newLink = displayText ? `[[${filename}|${displayText}]]` : `[[${filename}]]`;
          if (match !== newLink) {
            fixes.push(`${match} -> ${newLink}`);
            modified = true;
            return newLink;
          }
        }
        return match;
      });
    }

    // Fix known replacements
    for (const [from, to] of Object.entries(LINK_REPLACEMENTS)) {
      const regex = new RegExp(`\\[\\[${from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(\\|[^\\]]+)?\\]\\]`, 'g');
      content = content.replace(regex, (match, displayText) => {
        const newLink = displayText ? `[[${to}${displayText}]]` : `[[${to}]]`;
        if (match !== newLink) {
          fixes.push(`${match} -> ${newLink}`);
          modified = true;
          return newLink;
        }
        return match;
      });
    }

    // Remove trailing backslashes from any remaining links
    content = content.replace(/\[\[([^\]|]+?)\\(\|[^\]]+)?\]\]/g, (match, link, display) => {
      const newLink = display ? `[[${link}${display}]]` : `[[${link}]]`;
      if (match !== newLink) {
        fixes.push(`${match} -> ${newLink}`);
        modified = true;
        return newLink;
      }
      return match;
    });

    if (modified) {
      fs.writeFileSync(filePath, content);
      console.log(`Fixed ${file}:`);
      fixes.forEach(f => console.log(`  ${f}`));
      totalFixes += fixes.length;
    }
  }

  console.log(`\nTotal fixes: ${totalFixes}\n`);
}

fixWikilinks().catch((err) => {
  console.error('Fix failed:', err);
  process.exit(1);
});
