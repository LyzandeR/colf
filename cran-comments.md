## New Submission - colf Package

## Test environments
* local windows, R 3.3.2, R devel (also winbuilder on CRAN)
* ubuntu 12.04 LTS (on travis-ci), R 3.3.1

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Submitting after Uwe's comments

Thanks, we see:

Possibly mis-spelled words in DESCRIPTION:
  lm (13:82)

Please write lm().


The Description field should not start with the package name,
  'This package' or similar.

Just start "Performs .....". 

Fixed both.