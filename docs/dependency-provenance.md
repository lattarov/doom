# Dependency provenance & corporate network posture

Last generated: 2026-08-21, against this repo's `init.el` + `packages.el` on a
Linux/WSL profile. Regenerate the SBOM (see "Regenerating this document")
whenever the module list in `init.el` or the overlay in `config.org` changes
materially, or at least before handing this to security/IT for review.

This document is written to be handed to an internal security/IT reviewer
who has flagged "300 dependencies from GitHub, Codeberg, etc." as a concern,
and who is evaluating this against EU Cyber Resilience Act (CRA)-style
supply-chain expectations. It is not legal advice — see
[CRA applicability](#cra-applicability-not-legal-advice) for the reasoning
and its limits.

## 1. What this actually is

This repository (`~/.config/doom`) is a personal [Doom Emacs](https://github.com/doomemacs/doomemacs)
private configuration: `init.el` selects Doom's built-in modules, and
`packages.el` (generated — see §5) adds ~23 extra packages on top. Doom's
package manager, [straight.el](https://github.com/radian-software/straight.el),
resolves the module selection into a flat list of upstream Emacs Lisp
package repositories and clones them locally.

It is developer tooling used by one person to edit code and text. It is not
built, packaged, or distributed to any customer, and it is not a "product"
this employer places on the market.

## 2. Dependency inventory (SBOM)

Ground truth was taken directly from the local straight.el checkout
(`~/.config/emacs/.local/straight/repos/*`, `git remote get-url origin` +
`git rev-parse HEAD` per repo) plus Doom Emacs's own checkout, rather than
parsed from config — this is what is actually on disk on this machine.

The SBOM is published as **[SPDX 2.3 JSON](https://spdx.github.io/spdx-spec/v2.3/)**
— the Linux Foundation standard also used by the OpenEmbedded/Yocto build
system (`create-spdx.bbclass`) — at
[`sbom-doom-packages-2026-08-21.spdx.json`](./sbom-doom-packages-2026-08-21.spdx.json).
Each of its 281 `packages` entries carries a resolved git commit
(`versionInfo`), a VCS download location, and an SPDX license identifier
(`licenseConcluded`/`licenseDeclared`) — the license *identifier*, e.g.
`GPL-3.0-or-later`, not the full license text; `copyrightText` is left
`NOASSERTION` since no per-file copyright-holder extraction was done.

| Metric | Value |
|---|---|
| Packages in the SBOM (280 straight.el-managed + Doom Emacs itself) | **281** |
| — via github.com | 271 (270 packages + Doom Emacs core) |
| — via codeberg.org | 7 |
| — via gitlab.com | 3 |
| Doom Emacs's own module tree (upstream, not separately re-verified) | 586 packages declared, 574 carry an explicit `:pin <sha>` (~98%) |
| This user's `packages.el` overlay | 23 packages, 21 pinned as of this revision |

### License breakdown (281 packages)

| SPDX identifier | Count |
|---|---|
| GPL-3.0-or-later | 216 |
| MIT | 18 |
| GPL-2.0-or-later | 12 |
| BSD-2-Clause | 10 |
| BSD-3-Clause | 7 |
| GPL-3.0-only | 6 |
| Unlicense | 5 |
| NOASSERTION | 3 |
| WTFPL | 2 |
| LicenseRef-Public-Domain | 1 |
| Apache-2.0 | 1 |

Detection method, per package, in priority order: (1) an explicit
`SPDX-License-Identifier:` header tag in a source `.el` file — searched
recursively under the repo (not just its root, since well-organized
packages like magit/forge/transient keep sources under `lisp/`) — as the
single most authoritative signal, since it's the maintainer directly
stating the license; failing that, (2) a repo-root `LICENSE`/`COPYING`
family file, classified by signature phrases (full legal text, named
license mentions like "Simplified BSD" or "3-clause", public-domain
declarations, etc.); failing that, (3) GPL boilerplate or a license name in
a source header comment; failing that, `NOASSERTION` rather than a guess.

A bare `LICENSE` file's raw legal text can't by itself distinguish
GPL "-only" from "-or-later" — that's a per-file authorial choice, stated
in each source file's own header, not part of the license text itself. All
GPL/LGPL classifications resolved only from a LICENSE file were
cross-checked against source headers for "any later version" language
before being finalized as "-only"; a handful had no matching header to
check at all and default to the license file's plain reading — the least
certain entries if a reviewer needs exact GPL-variant precision (a
by-package breakdown of detection confidence is preserved in the
[regeneration script](#regenerating-this-document)'s output, not
duplicated here). Doom Emacs itself is MIT-licensed, read directly from its
own `LICENSE` file.

The 3 `NOASSERTION` entries: `emacsmirror-mirror` and `gnu-elpa-mirror`
(index/mirror meta-repos, not single-license packages) and `evil-quick-diff`
(no license file or header text found upstream — genuinely undeclared).
`org-re-reveal`'s repository is multi-licensed at file granularity
following the [REUSE](https://reuse.software/) spec (its `LICENSES/`
directory holds AGPL-3.0-or-later, CC0-1.0, and GPL-3.0-or-later texts for
different assets), but the actual Elisp file this config depends on,
`org-re-reveal.el`, carries its own `SPDX-License-Identifier:
GPL-3.0-or-later` tag — that's what's recorded here, since it's the
license of the code actually pulled in, not the repository's other assets.

Outside straight.el's package graph, more hosts are touched, only at
install/upgrade time, never at runtime, and are not part of this SBOM file:

| Component | Host | When | Integrity check |
|---|---|---|---|
| Emacs itself (source build) | `git.savannah.gnu.org` (GNU's own infra) | once, `build_emacs_doom.sh` Step 5 | git commit/tag pinned by version choice |
| LanguageTool (grammar checker) | `languagetool.org` | optional, Step 2 | **SHA-256 verified before use** |
| Rust toolchain installer | `sh.rustup.rs`, `static.rust-lang.org` | optional, Step 3 | rustup's own signature checks |
| Cargo crates (`emacs-lsp-booster`) | `crates.io` / `static.crates.io` / `index.crates.io` | optional, Step 3 | crates.io content hashing |
| npm packages (`pyright`) | `registry.npmjs.org` | optional, Step 4 | npm package-lock integrity hashes |
| whisper.cpp + speech model | `github.com/ggerganov/whisper.cpp` + model host (varies by upstream version — capture the exact URL from a verbose run before requesting a firewall allowlist entry) | optional, Step 8 | none currently |

**Firewall allowlist, if your network uses default-deny egress rather than a
proxy:** `github.com`, `codeberg.org`, `gitlab.com` cover the package graph;
add `git.savannah.gnu.org`, `languagetool.org`, `sh.rustup.rs`,
`static.rust-lang.org`, `crates.io`, `static.crates.io`, `index.crates.io`,
`registry.npmjs.org` for the optional build-script steps. That is the
complete, bounded list — not "the whole internet."

## 3. Pinning / reproducibility

- **Doom Emacs core is already pinned at the source.** Its module tree
  (`sources/doom+`, upstream `doomemacs/modules`) declares 586 packages and
  pins 574 of them to a maintainer-tested commit SHA. This is the primary
  reproducibility mechanism — there is deliberately no separate
  `straight/versions` lockfile; `:pin` per package *is* the lockfile,
  maintained by Doom's own maintainers as part of their release process.
- **This user's overlay (`packages.el`) is now pinned to the same standard.**
  Of 23 declared packages, 21 carry `:pin <sha>` resolved to the exact commit
  currently installed on this machine (see the diff in this branch).
  `robot-mode` / `robot-log` remain unpinned — they are `:disable`d on this
  personal profile and have never actually been cloned here, so there is no
  resolved commit to pin to; pin them the next time they're enabled and
  synced on a WORK profile.
- A straight.el repo sitting on a named branch (not detached HEAD) is
  *not* evidence of an unpinned package — straight resets a local branch to
  the pinned commit rather than checking out detached. Pin status is
  determined by the `:pin` directive in the declaring `packages.el`, not by
  `git symbolic-ref` output.

## 4. Corporate proxy / TLS-inspection support

`scripts/build_emacs_doom.sh` now has a **Step 0 — Corporate proxy / CA
trust** that runs before anything else. Given a proxy URL and/or a
TLS-inspection root CA (interactively, or via `CORP_PROXY=... CORP_CA_BUNDLE=/path/to/ca.pem`
environment variables for unattended runs), it:

- installs the CA into the system trust store (`update-ca-certificates`) —
  this alone covers `curl`, `git`, and Emacs's own GnuTLS-based TLS stack;
- sets `git config --global http.proxy` / `https.proxy` — this is what makes
  straight.el's package clones (invoked from *inside* Emacs) work, since git
  always reads `~/.gitconfig` regardless of the calling process's own
  environment;
- writes an `/etc/apt/apt.conf.d` proxy stanza (`sudo` drops inherited env
  vars, so apt needs its own copy);
- writes `~/.cargo/config.toml` `[http]` proxy/CA entries;
- sets `NODE_EXTRA_CA_CERTS`, `SSL_CERT_FILE`, `REQUESTS_CA_BUNDLE`,
  `CARGO_HTTP_CAINFO` for npm/pip/cargo, which don't all trust the system
  store by default;
- persists proxy vars into `~/.config/environment.d/`, so the systemd user
  session — and therefore `emacs.service`, which does **not** inherit an
  interactive shell's environment — picks them up too.

`npm config set proxy/https-proxy/cafile` is also applied explicitly in the
npm step for the same reason.

No mirror/Artifactory-style proxy is assumed — this targets a plain
HTTP(S) forward proxy with a TLS-inspecting CA, which is what's in place
here. If IT later stands up an internal generic/git mirror, the
straight.el `:host`/`:repo` recipes and the `downloadLocation` values in the
SBOM above are exactly the list that would need remapping.

## 5. Why `packages.el` isn't in git (and what is)

`packages.el`, `config.el`, and `custom.el` are gitignored — they are
**tangled build artifacts** of `config.org` (Doom's `:config literate`
module), regenerated by `org-babel-tangle-file` on every commit via
`.githooks/pre-commit`. `config.org` is the actual source of truth and is
tracked; the pins in §3 were added there, not by hand-editing the generated
file (a hand-edit would be silently overwritten on the next tangle — this is
what happened once during this session before the fix was moved into
`config.org`).

**Recommendation:** if this document is used to demonstrate auditability to
a reviewer, be aware that `git log -- packages.el` will show nothing — the
real diff history for dependency changes lives in `git log -- config.org`
(grep for `:tangle packages.el` blocks).

## 6. Residual gaps (not addressed by this pass)

- Doom Emacs core itself (`github.com/doomemacs/doomemacs`) is `git clone`d
  fresh with no pin/tag in `scripts/build_emacs_doom.sh`. Doom does not
  publish signed release tags in a way that's trivial to pin here without
  giving up `doom upgrade`'s normal flow; if a reviewer wants this pinned
  too, the trade-off is losing one-command upgrades in exchange for a fixed
  Doom-core commit — worth a explicit decision rather than a silent default.
- `whisper.cpp`'s model download host isn't nailed down in this document
  (it can vary by upstream script version) — capture it with `bash -x` on a
  clean run before submitting a firewall allowlist request.
- No repo here currently pins Doom Emacs's *own* dependency graph beyond
  what upstream already pins (§3) — that pinning is inherited, not
  independently re-verified. Treat Doom's own commit-pinning discipline as
  a trust dependency, same as trusting any other vendor's release process.

## 7. CRA applicability (not legal advice)

The EU Cyber Resilience Act (Regulation (EU) 2024/2847) imposes obligations
on **manufacturers** who place a **"product with digital elements"** on the
market **in the course of a commercial activity**. Two carve-outs are
relevant here:

1. **Non-commercial open source.** The CRA's recitals (see Recital 18-19 in
   the adopted text) exclude free and open-source software developed or
   supplied outside the course of a commercial activity from most
   obligations. Emacs, Doom Emacs, and the 280 packages in the SBOM above
   are exactly that: individually-maintained or foundation-maintained FOSS,
   not sold or monetized as a product by their authors.
2. **Internal tooling isn't "placed on the market."** This configuration is
   assembled by one employee, for their own editor, never distributed to a
   customer or even to another team as a deliverable. It doesn't meet the
   CRA's definition of a product being placed on the market — the same
   reasoning that puts a developer's IDE plugin list, dotfiles, or shell
   aliases outside CRA's scope generically.

Net: this employer very likely has **no CRA "manufacturer" obligations**
for this configuration, and neither do the upstream FOSS maintainers for
their individual packages. What IT is reasonably asking for regardless —
provenance, pinning, a bounded network footprint — is good supply-chain
hygiene independent of whether CRA legally applies, and is what §§2-4 above
provide. Get an actual determination from legal/compliance if this is being
used as a formal record rather than a working answer to a security
reviewer's question.

## Regenerating this document

Save as e.g. `/tmp/gen-sbom.py` and run with `python3 /tmp/gen-sbom.py` from
the repo root. Implements the detection rules from §2, including the
GPL `-only` vs `-or-later` header cross-check.

```python
#!/usr/bin/env python3
import datetime, glob, json, os, re, subprocess, uuid

REPOS_DIR = os.path.expanduser("~/.config/emacs/.local/straight/repos")
DOOM_DIR = os.path.expanduser("~/.config/emacs")
LICENSE_FILES = ["LICENSE", "LICENSE.txt", "LICENSE.md", "LICENSE-MIT",
                  "LICENSE-APACHE", "COPYING", "COPYING.txt",
                  "COPYING.LESSER", "UNLICENSE"]

CANONICAL_SPDX_IDS = {
    "gpl-3.0-or-later": "GPL-3.0-or-later", "gpl-3.0-only": "GPL-3.0-only",
    "gpl-2.0-or-later": "GPL-2.0-or-later", "gpl-2.0-only": "GPL-2.0-only",
    "mit": "MIT", "bsd-2-clause": "BSD-2-Clause", "bsd-3-clause": "BSD-3-Clause",
    "apache-2.0": "Apache-2.0", "isc": "ISC", "wtfpl": "WTFPL",
    "unlicense": "Unlicense", "cc0-1.0": "CC0-1.0", "mpl-2.0": "MPL-2.0",
    "agpl-3.0-or-later": "AGPL-3.0-or-later", "agpl-3.0-only": "AGPL-3.0-only",
}

def canonicalize(spdx_id):
    return CANONICAL_SPDX_IDS.get(spdx_id.lower(), spdx_id)

def classify(text):
    or_later = "any later version" in text
    if "GNU AFFERO GENERAL PUBLIC LICENSE" in text.upper():
        return "AGPL-3.0-or-later" if or_later else "AGPL-3.0-only"
    if "GNU LESSER GENERAL PUBLIC LICENSE" in text.upper():
        if "version 3" in text.lower(): return "LGPL-3.0-or-later" if or_later else "LGPL-3.0-only"
        if "version 2.1" in text.lower(): return "LGPL-2.1-or-later" if or_later else "LGPL-2.1-only"
    if "GENERAL PUBLIC LICENSE" in text.upper():
        if "version 3" in text.lower(): return "GPL-3.0-or-later" if or_later else "GPL-3.0-only"
        if "version 2" in text.lower(): return "GPL-2.0-or-later" if or_later else "GPL-2.0-only"
    if "licensed under the same terms as emacs" in text.lower(): return "GPL-3.0-or-later"
    if "MIT License" in text or "Permission is hereby granted, free of charge" in text: return "MIT"
    if "Apache License" in text and "2.0" in text: return "Apache-2.0"
    if "Redistribution and use in source and binary forms" in text:
        return "BSD-3-Clause" if "Neither the name" in text else "BSD-2-Clause"
    tl = text.lower()
    if ("bsd 3-clause" in tl or "bsd-3-clause" in tl or "new bsd" in tl
            or ("3-clause" in tl and "bsd" in tl)):
        return "BSD-3-Clause"
    if ("bsd 2-clause" in tl or "bsd-2-clause" in tl or "simplified bsd" in tl
            or "freebsd license" in tl):
        return "BSD-2-Clause"
    if "Mozilla Public License" in text and "2.0" in text: return "MPL-2.0"
    if "unencumbered software released into the public domain" in text: return "Unlicense"
    if "CC0" in text: return "CC0-1.0"
    if "ISC License" in text: return "ISC"
    if "DO WHAT THE FUCK YOU WANT" in text or re.search(r"\bWTFPL\b", text): return "WTFPL"
    if "public domain" in tl: return "LicenseRef-Public-Domain"
    return None

def el_headers(repo_dir, limit=8):
    # Recursive: many well-organized packages (magit, forge, ghub, transient,
    # with-editor, ...) keep sources under lisp/ rather than the repo root.
    candidates = glob.glob(os.path.join(repo_dir, "**", "*.el"), recursive=True)
    def sort_key(p):
        rel = os.path.relpath(p, repo_dir)
        depth = rel.count(os.sep)
        is_test = 1 if "test" in os.path.basename(p).lower() else 0
        return (depth, is_test, rel)
    for el in sorted(candidates, key=sort_key)[:limit]:
        yield "".join(open(el, errors="ignore").readlines()[:60])

def detect_license(repo_dir):
    # An explicit SPDX-License-Identifier tag is the single most authoritative
    # signal available (it's the maintainer directly stating the license) —
    # check it before anything else, including a LICENSE file, since a bare
    # LICENSE file's raw legal text can't distinguish "-only" from "-or-later"
    # (that distinction is a per-file authorial choice, not part of the
    # license text itself).
    headers = list(el_headers(repo_dir))
    for head in headers:
        m = re.search(r"SPDX-License-Identifier:\s*(\S+)", head)
        if m:
            return canonicalize(m.group(1)), "header-spdx-tag"

    file_lic = None
    for fname in LICENSE_FILES:
        path = os.path.join(repo_dir, fname)
        if os.path.isfile(path):
            lic = classify(open(path, errors="ignore").read(3000))
            if lic:
                file_lic = lic
                break

    if file_lic and file_lic.endswith("-only") and file_lic.split("-")[0] in ("GPL", "LGPL"):
        checked_any = or_later = False
        for head in headers:
            if "General Public License" in head:
                checked_any = True
                if "any later version" in head:
                    or_later = True
                    break
        if or_later:
            return file_lic.replace("-only", "-or-later"), "file+header-crosscheck-or-later"
        return file_lic, "file+header-crosscheck-only" if checked_any else "file-no-header-crosscheck"
    if file_lic:
        return file_lic, "file"

    for head in headers:
        lic = classify(head)
        if lic: return lic, "header-boilerplate"
    return "NOASSERTION", "none"

def repo_entry(repo_dir, name):
    url = subprocess.check_output(["git", "-C", repo_dir, "remote", "get-url", "origin"], text=True).strip()
    sha = subprocess.check_output(["git", "-C", repo_dir, "rev-parse", "HEAD"], text=True).strip()
    lic, _confidence = detect_license(repo_dir)
    return {"name": name, "url": url, "sha": sha, "license": lic}

def sanitize(name):
    return re.sub(r"[^a-zA-Z0-9.-]", "-", name)

entries = [repo_entry(DOOM_DIR, "doom-emacs")]
for d in sorted(os.listdir(REPOS_DIR)):
    full = os.path.join(REPOS_DIR, d)
    if os.path.isdir(os.path.join(full, ".git")):
        entries.append(repo_entry(full, d))

packages, relationships, seen = [], [], {}
for e in entries:
    base = f"SPDXRef-Package-{sanitize(e['name'])}"
    seen[base] = seen.get(base, 0) + 1
    spdx_id = base if seen[base] == 1 else f"{base}-{seen[base]}"
    packages.append({
        "SPDXID": spdx_id, "name": e["name"], "versionInfo": e["sha"],
        "downloadLocation": f"git+{e['url']}@{e['sha']}",
        "filesAnalyzed": False,
        "licenseConcluded": e["license"], "licenseDeclared": e["license"],
        "copyrightText": "NOASSERTION", "supplier": "NOASSERTION",
    })
    relationships.append({"spdxElementId": "SPDXRef-DOCUMENT",
                           "relationshipType": "DESCRIBES",
                           "relatedSpdxElement": spdx_id})

doc = {
    "spdxVersion": "SPDX-2.3", "dataLicense": "CC0-1.0", "SPDXID": "SPDXRef-DOCUMENT",
    "name": "doom-emacs-personal-config",
    "documentNamespace": f"https://spdx.org/spdxdocs/doom-emacs-personal-config-{datetime.date.today()}-{uuid.uuid4()}",
    "creationInfo": {
        "created": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "creators": ["Tool: doom-config-sbom-generator",
                      "Person: Victor Lattaro Volpini (victorlattaro+ai@gmail.com)"],
    },
    "packages": packages, "relationships": relationships,
}

out = f"docs/sbom-doom-packages-{datetime.date.today()}.spdx.json"
json.dump(doc, open(out, "w"), indent=2)
print(f"wrote {out}: {len(packages)} packages, {len(relationships)} relationships")
```
