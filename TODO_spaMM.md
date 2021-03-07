## ROADMAP FOR SPAMM SUPPORT

This document describes what should be done so that spaMM becomes supported by broom.mixed.


### Basic setup
- [x] fork and clone broom.mixed
- [x] install broom.mixed dependencies
- [x] check that R CMD check is clean

**Note:** 1 failed test for stanarm (commented) + 15 warnings in tests (untouched)

- [x] add spaMM to DESCRIPTION
- [x] create empty file for tests
- [x] create empty file for tidiers

### Implement the basic versions of the main methods
- [ ] implement basic version for tidy.HLfit()
- [ ] implement basic version for augment.HLfit()
- [ ] implement basic version for glance.HLfit()

See https://www.tidymodels.org/learn/develop/broom/ and the broom.mixed vignette for detailed instructions

### Improve the main methods
See the dedicated spaMM vignette in this package

### Checks / Tests
Make sure it works for:

- [ ] all common random effect structures
- [ ] all common families and links
- [ ] the 3 fitting functions (`fitme()`, `HLfit()`, `corrHLfit()`)
- [ ] multivariate models?

See the dedicated spaMM vignette in this package

### Polish
- [ ] cleanup test file
- [ ] uncomment added comment in test-stanarm.R
- [ ] merge latest broom.mixed
- [ ] store fitted models as rda in inst/extdata (produced in run_example.R)
- [ ] update documentation (see https://roxygen2.r-lib.org/articles/markdown.html)
- [ ] update examples
- [ ] remove special spaMM vignette
- [ ] update vignette
- [ ] update NEWS
- [ ] delete this file
- [ ] remove mention of this file in .Rbuildignore
- [ ] PR
