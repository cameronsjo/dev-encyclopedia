#!/usr/bin/env node

/**
 * Fixes frontmatter issues in markdown files:
 * - Converts date strings to YYYY-MM-DD format
 * - Adds missing status field to MOC files
 * - Fixes Home.md type
 */

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');
const { glob } = require('glob');

const IGNORED_PATTERNS = [
  'node_modules/**',
  '.obsidian/**',
  'templates/**',
  '.git/**',
  'README.md',
  'CLAUDE.md',
];

async function fixFrontmatter() {
  const files = await glob('**/*.md', {
    ignore: IGNORED_PATTERNS,
    cwd: process.cwd(),
  });

  let fixedCount = 0;

  for (const file of files) {
    const filePath = path.join(process.cwd(), file);
    const content = fs.readFileSync(filePath, 'utf8');

    let parsed;
    try {
      parsed = matter(content);
    } catch (err) {
      console.log(`Skipping ${file}: ${err.message}`);
      continue;
    }

    const { data: frontmatter, content: body } = parsed;
    let modified = false;

    // Skip files without frontmatter
    if (!frontmatter || Object.keys(frontmatter).length === 0) {
      continue;
    }

    // Fix date format
    if (frontmatter.created) {
      const dateStr = String(frontmatter.created);
      // Check if it's a long date string
      if (dateStr.includes('GMT') || dateStr.length > 10) {
        const date = new Date(dateStr);
        if (!isNaN(date.getTime())) {
          const year = date.getFullYear();
          const month = String(date.getMonth() + 1).padStart(2, '0');
          const day = String(date.getDate()).padStart(2, '0');
          frontmatter.created = `${year}-${month}-${day}`;
          modified = true;
          console.log(`  Fixed date in ${file}: ${dateStr} -> ${frontmatter.created}`);
        }
      }
    }

    // Add missing status to MOC files
    if (file.includes('MOC') && !frontmatter.status) {
      frontmatter.status = 'complete';
      modified = true;
      console.log(`  Added status to ${file}`);
    }

    // Fix Home.md type
    if (file === 'Home.md') {
      if (frontmatter.type === 'home') {
        frontmatter.type = 'moc';
        modified = true;
        console.log(`  Fixed type in ${file}: home -> moc`);
      }
      if (!frontmatter.status) {
        frontmatter.status = 'complete';
        modified = true;
        console.log(`  Added status to ${file}`);
      }
    }

    // Write back if modified
    if (modified) {
      const newContent = matter.stringify(body, frontmatter);
      fs.writeFileSync(filePath, newContent);
      fixedCount++;
    }
  }

  console.log(`\nFixed ${fixedCount} file(s)\n`);
}

fixFrontmatter().catch((err) => {
  console.error('Fix failed:', err);
  process.exit(1);
});
