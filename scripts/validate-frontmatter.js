#!/usr/bin/env node

/**
 * Validates frontmatter in all markdown files.
 * Ensures required fields are present and have valid values.
 */

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');
const { glob } = require('glob');

const REQUIRED_FIELDS = ['title', 'tags', 'type', 'status', 'created'];

const VALID_TYPES = ['reference', 'comparison', 'concept', 'moc'];
const VALID_STATUSES = ['draft', 'in-progress', 'complete'];

const IGNORED_PATTERNS = [
  'node_modules/**',
  '.obsidian/**',
  'templates/**',
  'docs/**',
  '.git/**',
  'README.md',
  'CLAUDE.md',
];

async function validateFrontmatter() {
  const files = await glob('**/*.md', {
    ignore: IGNORED_PATTERNS,
    cwd: process.cwd(),
  });

  let hasErrors = false;
  const errors = [];
  const warnings = [];

  for (const file of files) {
    const filePath = path.join(process.cwd(), file);
    const content = fs.readFileSync(filePath, 'utf8');

    let parsed;
    try {
      parsed = matter(content);
    } catch (err) {
      errors.push(`${file}: Failed to parse frontmatter - ${err.message}`);
      hasErrors = true;
      continue;
    }

    const { data: frontmatter } = parsed;

    // Check if frontmatter exists
    if (!frontmatter || Object.keys(frontmatter).length === 0) {
      errors.push(`${file}: Missing frontmatter`);
      hasErrors = true;
      continue;
    }

    // Check required fields
    for (const field of REQUIRED_FIELDS) {
      if (!(field in frontmatter)) {
        errors.push(`${file}: Missing required field '${field}'`);
        hasErrors = true;
      }
    }

    // Validate field values
    if (frontmatter.type && !VALID_TYPES.includes(frontmatter.type)) {
      errors.push(
        `${file}: Invalid type '${frontmatter.type}'. Valid types: ${VALID_TYPES.join(', ')}`
      );
      hasErrors = true;
    }

    if (frontmatter.status && !VALID_STATUSES.includes(frontmatter.status)) {
      errors.push(
        `${file}: Invalid status '${frontmatter.status}'. Valid statuses: ${VALID_STATUSES.join(', ')}`
      );
      hasErrors = true;
    }

    // Check tags is an array
    if (frontmatter.tags && !Array.isArray(frontmatter.tags)) {
      errors.push(`${file}: 'tags' should be an array`);
      hasErrors = true;
    }

    // Check tags is not empty
    if (
      frontmatter.tags &&
      Array.isArray(frontmatter.tags) &&
      frontmatter.tags.length === 0
    ) {
      warnings.push(`${file}: 'tags' array is empty`);
    }

    // Check created date format (YYYY-MM-DD)
    if (frontmatter.created) {
      const dateStr = String(frontmatter.created);
      const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
      if (!dateRegex.test(dateStr)) {
        errors.push(
          `${file}: Invalid date format '${frontmatter.created}'. Expected YYYY-MM-DD`
        );
        hasErrors = true;
      }
    }

    // Check title matches filename (warning only)
    const expectedTitle = path.basename(file, '.md');
    if (
      frontmatter.title &&
      frontmatter.title !== expectedTitle &&
      !file.includes('/')
    ) {
      // Only warn for root files, subdirectory files might have different conventions
    }
  }

  // Print results
  console.log(`\nValidated ${files.length} markdown files\n`);

  if (warnings.length > 0) {
    console.log('Warnings:');
    warnings.forEach((w) => console.log(`  ⚠️  ${w}`));
    console.log('');
  }

  if (errors.length > 0) {
    console.log('Errors:');
    errors.forEach((e) => console.log(`  ❌ ${e}`));
    console.log('');
  }

  if (hasErrors) {
    console.log(`Found ${errors.length} error(s)\n`);
    process.exit(1);
  } else {
    console.log('All frontmatter is valid!\n');
    process.exit(0);
  }
}

validateFrontmatter().catch((err) => {
  console.error('Validation failed:', err);
  process.exit(1);
});
