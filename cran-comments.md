## Resubmission

This is a resubmission.

We fixed a broken link in a vignette. We also added a reminder to run
`urlchecker::url_check()` when calling `devtools::release()`.

## Release summary

This is a minor release. 

This release introduces new functions (`standardize_variable`,
`compute_indirect_effect_for`, `check_assumptions`, `plot_assumptions`) and a
new vignette (`"moderated-mediation"`).

We also rewrote an old vignette (`"jsmediation"`) to account for the changes,
added new tests, and improved the package coverage.

## Test environments
* local macOS R install, R 4.1.0
* GitHub Actions (macOS-latest): release
* GitHub Actions (windows-latest): relaese
* GitHub Actions (ubuntu-20.04): release, devel

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTE

## Downstream dependencies

There are currently no downstream dependencies for this package.
