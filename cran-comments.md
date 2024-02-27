## Release summary

This is the second resubmission of a patch release.


This resubmission makes some test, vignette and example conditionnal. It fixes
problems highlighted by a CRAN NoSuggests check. It also fixes missing tags in
documentation raising NOTES on CRAN systems. It also adds the revdep/ folder in
.Rbuildignore.

This release fixes an issue raised because of a recent {see} update. It also
removes some internal {dplyr} code that had been deprecated.

## Test environments
* local win R install, R 4.2.2
* GitHub Actions (macOS-latest): release
* GitHub Actions (windows-latest): relaese
* GitHub Actions (ubuntu-20.04): release, devel
* `devtools::check_win_devel()`

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTE

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
