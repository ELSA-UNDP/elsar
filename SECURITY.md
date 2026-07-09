# Security policy

## Reporting a vulnerability

If you believe you've found a security-sensitive problem in elsar, **please do
not open a public issue.** Instead, email the maintainers at
**scott.atkinson@undp.org** with:

- a description of the problem and its potential impact, and
- steps to reproduce it, if you have them.

We'll acknowledge your report as soon as we can and keep you informed as we
investigate and address it.

## Scope

elsar is an R package for conservation-planning workflows. The most relevant
security considerations are around **credentials and external services**:

- Google Earth Engine and Google Drive authentication tokens,
- PostgreSQL connection details passed to `elsar_load_data()`,
- any file paths or data the package reads and writes.

Please **never paste real credentials, tokens, connection strings, or passwords**
into a GitHub issue, a pull request, or a reproducible example. Redact them
first. If you think a code path causes credentials to be logged, cached, or
written to disk unexpectedly, that's exactly the kind of thing to report
privately using the email above.

## Supported versions

elsar is under active development. Security fixes are applied to the `main`
branch and included in the next release. We don't backport fixes to older
tagged versions.
