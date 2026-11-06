---
name: release
description: Prepare an elsar release from develop to main (version bump, NEWS, checks, release PR, tag and GitHub release) so downstream repos like elsar_pipeline can pin a version. Use when asked to "release", "cut a version" or "merge develop into main".
---

# Release elsar

`elsar_pipeline` installs elsar from GitHub, so a release is what it gets on
its next build. Releases need to be deliberate, tagged and recorded.

## 1. Decide the version

Follow semantic versioning on `DESCRIPTION`'s `Version`:

- **patch** (0.4.1 → 0.4.2): bug fixes only;
- **minor** (0.4.x → 0.5.0): new functions or arguments, deprecations, or
  changed defaults (anything in NEWS under "Breaking changes" or with `!` in
  the commit type);
- **major** (→ 1.0.0): agreed with the maintainers.

List what is going out with `git log --oneline origin/main..origin/develop`,
and check every user-visible change has a NEWS bullet.

## 2. Prepare on a branch from `develop`

1. Set `Version:` in `DESCRIPTION`.
2. In `NEWS.md`, rename `# elsar (development version)` to `# elsar X.Y.Z`.
   Group bullets under "Breaking changes", "New features" and "Bug fixes".
   Call out downstream impact explicitly (renamed or deprecated exports,
   changed defaults, different numbers).
3. Run the `check-package` skill. It must be clean apart from the known
   baseline.
4. Open a PR into `develop`; after it merges, open the release PR
   `develop` → `main` titled "Release X.Y.Z" with the NEWS section as the
   body.

## 3. After the release PR merges (ask before each step)

1. Tag the merge commit on `main` and publish a GitHub release:
   `git tag -a vX.Y.Z -m "elsar X.Y.Z"`, `git push origin vX.Y.Z`, and
   `gh release create vX.Y.Z --notes-file <NEWS section>`.
2. On `develop`, add a fresh `# elsar (development version)` heading to NEWS
   and bump to `X.Y.Z.9000`.
3. Tell `elsar_pipeline`'s maintainers. Once tags exist, it can pin
   `remotes::install_github("ELSA-UNDP/elsar@vX.Y.Z")` instead of tracking
   the default branch.

The pkgdown site deploys from `main` automatically.
