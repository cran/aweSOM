# aweSOM 1.3

## Fixed bugs:

- Fixed html issues in documentation, to comply with R.4.2.0



# aweSOM 1.2

## Fixed bugs:

- Fixed cell placement in interactive graphs (they were flipped wrt kohonen package, and incorrect for hex topology with odd number of rows)
- Fixed cell placement in smooth distance plot
- Fixed bug in somInit when using PCA initialization (reverted to prcomp)

## Improvements:

- New legend for graph, interactive in html, present in svg
- New "cloud" plot
- Improved "smooth distance" plot
- Several improvements in the shiny UI, more user-friendly and stable
- In shiny UI, possibility to change numeric to factor and vice-versa

## Misc:
- New logo, and updated UI theme to match


# aweSOM 1.1

## Fixed bugs:

- In reproducible script, when data was unscaled it was not in matrix form.
- In the plot panel, the scales option was not available for CatBarplot.
- Variables weights are no longer standardized to sum to 1.
- Boxplot on prototype values is now drawn for the prototype values.

## Improvements:

- Added support for categorical variables, and NA values.
- In the plot panel, variables selection changes only when necessary.
- New functions: cdt for complete disjunctive tables, and aweSOMreorder to reorder variables for pretty display.
- Option to hide observations names in the plots.

## Misc:

- Added hex-sticker logo.
- Improved appearance of the shiny UI.
- More detailed help messages in UI.
