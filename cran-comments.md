## Test environments
- Local Windows (R 4.3.x): R CMD check --as-cran: 0 errors | 0 warnings | 0 notes (or list notes, if any)
- GitHub Actions: ubuntu-latest, macos-latest (Intel & arm64), windows-latest: passing
- R-hub: linux, macOS (Intel/arm64), Windows: passing
- win-builder: R-release & R-devel: passing

## Notes
- First CRAN submission for package name “FakeDataR”.
- Examples and vignettes are fast; no network or long-running code.
- No non-CRAN dependencies required to build vignettes.

## Downstream dependencies
- None.


