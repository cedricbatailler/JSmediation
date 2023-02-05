## Release summary

This is a patch release. 

This release removes a test depending on a package that have been archived by
CRAN ({processR}). It also fixes how test relying on external package are ran.
Tests depending on Suggests package are now conditional.

Some parts of the documentation have also been reworked to account for a the new
{pkgdown} version.

We also remove a dependency that was not used.

## Test environments
* local win R install, R 4.2.2
* GitHub Actions (macOS-latest): release
* GitHub Actions (windows-latest): relaese
* GitHub Actions (ubuntu-20.04): release, devel

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTE

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
