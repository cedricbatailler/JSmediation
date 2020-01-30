## Release summary

This is a patch release.

Some of the functions used internally became incompatible with the glue class 
some of our character variables had. Because of this, one of the main function
was broken. Problematic variables are now coerced to characters which solving 
the problem. 

## Test environments
* local macosR install, R 3.6.1
* R-hub via `rhub::check_for_cran()`
* ubuntu 16.04 (on travis-ci)
* win-builder (old, release, andisnt devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTE

## Downstream dependencies

There are currently no downstream dependencies for this package.