#!/usr/bin/env node
// Fetches link metadata from jsonlink.io for [embed:URL] tags in entry markdown
// files and persists the results in each file's frontmatter under `embeds:`.
//
// Usage:
//   npm run fetch-embeds                          # process entries with no embeds yet
//   npm run fetch-embeds -- --refresh             # re-fetch all entries
//   npm run fetch-embeds -- --refresh path/to/file.md  # re-fetch a specific file

const fs = require('fs');
const path = require('path');
const https = require('https');
const yaml = require('js-yaml');

const ENTRIES_DIR = path.join(__dirname, '..', 'src', 'pages', 'entries');
const API_KEY = process.env.JSONLINK_API_KEY;
const RATE_LIMIT_DELAY_MS = 7000; // 10 req/min limit → ~8 req/min to be safe

const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
let requestCount = 0;

function findEmbedUrls(body) {
    const urls = [];
    const re = /\[embed:(.+?)\]/g;
    let m;
    while ((m = re.exec(body)) !== null) {
        urls.push(m[1].trim());
    }
    return [...new Set(urls)];
}

function fetchMetadata(url) {
    const keyParam = API_KEY ? '&api_key=' + encodeURIComponent(API_KEY) : '';
    const apiUrl = 'https://jsonlink.io/api/extract?url=' + encodeURIComponent(url) + keyParam;
    return new Promise((resolve, reject) => {
        https.get(apiUrl, { headers: { Accept: 'application/json' } }, (res) => {
            let data = '';
            res.on('data', (chunk) => { data += chunk; });
            res.on('end', () => {
                try {
                    resolve(JSON.parse(data));
                } catch (e) {
                    reject(new Error('bad JSON: ' + data.slice(0, 120)));
                }
            });
        }).on('error', reject);
    });
}

async function processFile(filePath, refresh) {
    const content = fs.readFileSync(filePath, 'utf8');
    const match = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
    if (!match) return;

    const [, rawFm, body] = match;
    const fm = yaml.load(rawFm, { schema: yaml.CORE_SCHEMA });

    const urls = findEmbedUrls(body);
    if (urls.length === 0) return;

    if (fm.embeds && !refresh) {
        console.log(`  ${path.basename(filePath)}: skipped (already has embeds, use --refresh to update)`);
        return;
    }

    const embeds = fm.embeds || {};

    console.log(`  ${path.basename(filePath)}: fetching ${urls.length}...`);

    for (const url of urls) {
        try {
            if (requestCount > 0) await delay(RATE_LIMIT_DELAY_MS);
            requestCount++;
            const meta = await fetchMetadata(url);
            if (meta.error || !meta.title) {
                console.warn(`    ✗ ${url}: ${meta.error || 'no title'}`);
                embeds[url] = { title: url };
            } else {
                embeds[url] = {
                    title: meta.title,
                    ...(meta.description && { description: meta.description }),
                    ...(meta.images && meta.images[0] && { image: meta.images[0] }),
                };
                console.log(`    ✓ ${url}`);
            }
        } catch (err) {
            console.warn(`    ✗ ${url}: ${err.message}`);
            embeds[url] = { title: url };
        }
    }

    fm.embeds = embeds;
    const newFm = yaml.dump(fm, { schema: yaml.CORE_SCHEMA, lineWidth: -1 });
    fs.writeFileSync(filePath, `---\n${newFm}---\n${body}`, 'utf8');
}

async function main() {
    const args = process.argv.slice(2);
    const refresh = args.includes('--refresh');
    const files = args.filter((a) => !a.startsWith('-'));

    const targets =
        files.length > 0
            ? files.map((f) => path.resolve(f))
            : fs
                  .readdirSync(ENTRIES_DIR)
                  .filter((f) => f.endsWith('.md'))
                  .map((f) => path.join(ENTRIES_DIR, f));

    if (!API_KEY) console.warn('Warning: JSONLINK_API_KEY not set — requests may be blocked by Cloudflare');
    console.log(`Processing ${targets.length} file(s)...`);
    for (const file of targets) {
        await processFile(file, refresh);
    }
    console.log('Done.');
}

main().catch((err) => {
    console.error(err);
    process.exit(1);
});
