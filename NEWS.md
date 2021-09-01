# JSmediation (0.2.0)

## New features

* New functions `check_assumptions` and `plot_assimptions` which helps testing
  the OLS assumptions of the regressions models underlying the mediation test.
* New function `compute_indirect_effect_for()`which computes the indirect effect
  confidence interval for a specific value of the moderator in a moderated
  mediation model.
* New function `standardize_variable` which helps centering and reducing numeric
  variables.
  
## Documentation features 

* New vignette `vignette("moderated-mediation")` illustrates moderated mediation
  analyses.
* `vignette("jsmediation")` has been rewritten to account for the recent
  changes.

was updated  of the vignette illustrating simple mediation have been rewritten.

# JSmediation 0.1.2

* Add new test to check if the computation of confidence interval works as
intended.
* New home for the documentation at https://jsmediation.cedricbatailler.me
* Using testthat 3rd edition.

## Bug fixes

* Fix an error in how confidence intervals for moderated mediation index was
computed when `stage = "total"`.

# JSmediation 0.1.1

## Bug fixes

* Fix typo in the documentation.
* Fix an error related to the type of internal variables. 

# JSmediation 0.1.0

* Initial release.
* Added a `NEWS.md` file to track changes to the package.
