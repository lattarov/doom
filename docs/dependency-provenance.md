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
`git rev-parse HEAD` per repo) rather than parsed from config — this is what
is actually on disk on this machine.

| Metric | Value |
|---|---|
| Distinct upstream package repos (straight.el-managed) | **280** |
| — via github.com | 270 |
| — via codeberg.org | 7 |
| — via gitlab.com | 3 |
| Doom Emacs core packages declared upstream | 586, of which 574 carry an explicit `:pin <sha>` (~98%) |
| This user's `packages.el` overlay | 23 packages, 21 pinned as of this revision |

Full machine-readable listing: [`sbom-doom-packages-2026-08-21.csv`](./sbom-doom-packages-2026-08-21.csv)
(name, host, resolved commit, commit date, origin URL — one row per repo).

Outside straight.el's package graph, three more hosts are touched, only at
install/upgrade time, never at runtime:

| Component | Host | When | Integrity check |
|---|---|---|---|
| Emacs itself (source build) | `git.savannah.gnu.org` (GNU's own infra) | once, `build_emacs_doom.sh` Step 5 | git commit/tag pinned by version choice |
| Doom Emacs core | `github.com/doomemacs/doomemacs` | once, Step 6 | none currently (see §6) |
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
straight.el `:host`/`:repo` recipes and the git remotes in the CSV above are
exactly the list that would need remapping.

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
   obligations. Emacs, Doom Emacs, and the ~280 packages in the SBOM above
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

```sh
# 1. Rebuild the SBOM CSV from the live straight.el checkout:
for d in ~/.config/emacs/.local/straight/repos/*/; do
  name=$(basename "$d")
  url=$(git -C "$d" remote get-url origin)
  commit=$(git -C "$d" rev-parse HEAD)
  date=$(git -C "$d" log -1 --format=%cI HEAD)
  printf '%s|%s|%s|%s\n' "$name" "$url" "$commit" "$date"
done > /tmp/sbom-raw.csv

# 2. Add a header + host column, drop into docs/:
awk -F'|' 'BEGIN{OFS=","; print "name,host,commit,commit_date,url"}
  NF==4 { host="other"
    if ($2 ~ /github\.com/) host="github.com"
    else if ($2 ~ /codeberg\.org/) host="codeberg.org"
    else if ($2 ~ /gitlab\.com/) host="gitlab.com"
    print $1, host, $3, $4, $2 }' /tmp/sbom-raw.csv > docs/sbom-doom-packages-$(date +%F).csv
```
