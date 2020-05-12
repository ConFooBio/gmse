## Test environments
* ubuntu 16.04, R 3.5.2
* win-builder (devel)

## R CMD check results
There were no ERRORs, WARNINGs. One NOTE recognises the
non-standard files/directories '_pkgdown.ml' and 'docs',
which are used to produce the website associated with
this R package on GitHub.

## dontrun wraps
As with previous CRAN  submissions of
  GMSE, there are several function examples wrapped in
  dontrun that are also included in the documentation.
  While the DESCRIPTION and README clearly specify that
  all simulations can be run using the primary gmse,
  gmse_apply, and gmse_gui functions, documentation of
  other functions has been included for reference and
  for developers. 