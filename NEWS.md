# edwards 0.2.0

## Modelling Functions

I will be moving functions for modelling to a new package jemodel. To start this:

* Deleted modelling development code (moved to jemodel).
* Deprecate all glmnet, ranger, and roc functions (everything in `glmnet.R`, `ranger.R`, and `roc.R`).

## Other

* Add NEWS file.
* Add package-edwards.R for imports. 

# edwards 0.3.0

## Breaking Changes

Removed all deprecated modelling functions (ones starting `rang_`, `glmnet_`, or `roc_`). Use are all available in the jemodel package. A simplified version of `glmnet_to_table()` has been kept in the package because I have used this most often out of these functions. 
