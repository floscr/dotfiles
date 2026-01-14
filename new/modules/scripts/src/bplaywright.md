# bplaywright - NixOS Playwright Wrapper

Wrapper script to make Playwright work with NixOS-provided browsers.

## Problem

Playwright has hardcoded expectations about browser locations and directory structures that don't match NixOS:

1. **Version mismatch**: Playwright's `browsers.json` expects specific browser revisions that don't match the NixOS package
2. **Path mismatch**: Playwright on `linux-x64` expects `chrome-linux64/` but NixOS provides `chrome-linux/`
3. **Headless shell path**: Expects `chrome-headless-shell-linux64/chrome-headless-shell` but NixOS provides `chrome-linux/headless_shell`

## What the script does

1. **Patches `browsers.json`**: Updates chromium revision/version to match the NixOS browser
2. **Patches `registry/index.js`**: Fixes `EXECUTABLE_PATHS` to use NixOS directory structure
3. **Sets environment variables**: `PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS`, `PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD`, etc.

## Debugging "No chromium-based browser found"

If you see this error:
```
No chromium-based browser found on the system.
Please run the following command to download one:
    npx playwright install chromium
```

### Step 1: Check the NixOS browser structure

```bash
# Find the browsers path
echo $PLAYWRIGHT_BROWSERS_PATH

# List browser directories
ls -la $PLAYWRIGHT_BROWSERS_PATH/

# Check chromium structure (should be chrome-linux/chrome)
ls -la $PLAYWRIGHT_BROWSERS_PATH/chromium-*/
```

### Step 2: Check what Playwright expects

Look at `node_modules/playwright-core/lib/server/registry/index.js`:

```javascript
// Around line 78-87, EXECUTABLE_PATHS defines expected paths
"chromium": {
  "linux-x64": ["chrome-linux64", "chrome"],  // Playwright expects this
  "linux-arm64": ["chrome-linux", "chrome"],  // NixOS provides this structure
}
```

### Step 3: Verify the patches were applied

```bash
# Check if browsers.json was patched (revision should match NixOS)
cat node_modules/playwright-core/browsers.json | grep -A2 chromium

# Check if index.js was patched (should show chrome-linux, not chrome-linux64)
grep "chrome-linux64" node_modules/playwright-core/lib/server/registry/index.js
# If this returns results, the patch wasn't applied
```

### Step 4: Common fixes

1. **Directory name mismatch**: Patch `EXECUTABLE_PATHS` in `registry/index.js`:
   - Replace `"chrome-linux64", "chrome"` with `"chrome-linux", "chrome"`
   - Replace `"chrome-headless-shell-linux64", "chrome-headless-shell"` with `"chrome-linux", "headless_shell"`

2. **Version mismatch**: Patch `browsers.json` to match NixOS browser revision

3. **Environment variables**: Ensure these are set:
   - `PLAYWRIGHT_BROWSERS_PATH` - path to NixOS browser store
   - `PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1`
   - `PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=1`

## UI Mode specific issues

The `--ui` mode uses `findChromiumChannelBestEffort()` which tries browsers in order:
1. `chromium` (bundled) - needs correct path in `EXECUTABLE_PATHS`
2. `chrome` (system) - looks at `/opt/google/chrome/chrome`
3. `msedge` (system) - looks at `/opt/microsoft/msedge/msedge`

If bundled chromium path is wrong, it falls through and fails to find any browser.
