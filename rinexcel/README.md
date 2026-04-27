# RinExcel — R in Excel via WebAssembly

Run R code directly in Excel using **WebR** (R compiled to WebAssembly).  
Your spreadsheet data **never leaves your machine** for pure computation.

---

## Project structure

```
rinexcel/
├── manifest.xml              ← Office Add-in manifest
└── taskpane/
    ├── index.html            ← Main task pane (WebR + Office.js)
    └── commands.html         ← Ribbon command stub
```

---

## Step 1 — Deploy the task pane

The task pane must be served over **HTTPS**. Choose any option:

| Option | How |
|---|---|
| **GitHub Pages** | Push to a repo, enable Pages — free, instant |
| **Netlify / Vercel** | Drag-and-drop the `rinexcel/` folder |
| **Your own server** | Any static file server with a valid TLS cert |
| **localhost (dev)** | See below |

Then **replace every `https://YOUR_HOST`** in `manifest.xml` with your URL.

### Local development with `office-addin-dev-certs`

```bash
npx office-addin-dev-certs install
npx http-server -S -C ~/.office-addin-dev-certs/localhost.crt \
                   -K ~/.office-addin-dev-certs/localhost.key \
                   -p 3000
```

Add-in will be available at `https://localhost:3000`.

---

## Step 2 — Sideload the add-in

**Excel Desktop (Windows / Mac)**
1. Excel → **Insert** → **Add-ins** → **Manage My Add-ins** → **Upload My Add-in**
2. Browse to `manifest.xml`

**Excel on the web**
1. **Insert** → **Add-ins** → **Upload My Add-in**

---

## Usage

1. **Select a range** in Excel (with or without a header row)
2. Click **⬆ Read → df** — data loads into R as `df`
3. Write R code in the editor (`Ctrl+Enter` to run)
4. Click **↓ Write to Sheet** to write the last result back

---

## Privacy model

| Action | Data leaves the device? | Destination |
|---|---|---|
| Pure R computation | ❌ No | WebAssembly, in-browser |
| `webr::install("pkg")` | ⚠️ Package name only | Posit CDN (binary download, no user data) |
| `httr::GET("https://your-api.com")` | ✅ Yes, intentionally | Your server only |
| Any R outbound call | ✅ Yes | That URL's server |
| Microsoft infrastructure | ❌ Never (for data) | Serves Office.js runtime only |

---

## Roadmap

- **Step 2**: Package installation UI (`webr::install()` with privacy warning)
- **Step 3**: Snippet library — extended pre-built R templates
- **Step 4**: Custom Excel functions (`=R.RUN("myFunc", A1:B10)`)
- **Step 5**: Optional secured Plumber API backend for heavy computation
