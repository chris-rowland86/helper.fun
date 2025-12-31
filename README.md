# helperFun

Functions that make my life easier - an R package containing bespoke helper functions for routine tasks across various projects.

## Installation

You can install the development version of helperFun from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("chris-rowland86/helper-fun")
```

## Usage

After installation, load the package:

```r
library(helperFun)
```

### Example

```r
# Example function (replace with your actual functions)
result <- add_numbers(2, 3)
```

## Adding Your Functions

To add your functions from ChrisFlow:

1. Place your R function files in the `R/` directory
2. Document your functions using roxygen2 comments (see `R/example.R` for format)
3. Run `devtools::document()` to generate documentation
4. Update the `NAMESPACE` file by running `devtools::document()`
5. Add tests in `tests/testthat/` directory

## Development

To work on this package locally:

```r
# Load the package for development
devtools::load_all()

# Run tests
devtools::test()

# Generate documentation
devtools::document()

# Check the package
devtools::check()
```

## License

MIT License - see LICENSE file for details
