# Japonica

[![Netlify Status](https://api.netlify.com/api/v1/badges/e3556637-32c5-438c-b5ff-2b361ac3ebdc/deploy-status)](https://app.netlify.com/sites/japonica/deploys)

A blog system with Elm, siteelm, and Netlify CMS.

## Setup

```
npm i
```

## Run dev server

```
npm run dev
```

## Production build

```
npm run build
```

Then, see `/dist`.

## Embed cards

Link cards in entries are written as `[embed:URL]` in markdown. Metadata (title, description, image) is fetched from [jsonlink.io](https://jsonlink.io) at authoring time and stored in each file's frontmatter under `embeds:`. The static Elm renderer reads this at build time — no client-side requests.

### Fetching metadata for new entries

Copy `.env.example` to `.env` and set your jsonlink.io API key, then run:

```
npm run fetch-embeds
```

This processes only entries that don't have `embeds:` yet. To re-fetch an existing entry:

```
npm run fetch-embeds -- --refresh src/pages/entries/YYYY-MM-DD.md
```
