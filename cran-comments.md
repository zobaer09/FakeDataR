## Test environments
* Local: Windows 11, R 4.3.2
* GitHub Actions: macOS 14 (release), Windows Server 2022 (release),
  Ubuntu 22.04 (devel, release, oldrel-1)

## R CMD check results

- Local (macOS), GitHub Actions (macOS/Windows/Ubuntu), and rhub: 0 ERRORs, 0 WARNINGs.
- Win-builder (R-devel, Windows): 2 NOTEs
  1. “unable to verify current time” – due to CI/VM clock, no user-facing effect.
  2. “New submission” – this is the package’s first CRAN release.

The package has no compiled code and no non-standard system requirements.

