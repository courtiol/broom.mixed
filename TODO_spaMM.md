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
TO COME

### Check that it works for all common random effect structures
- [ ] check with no random effect
- [ ] check one and two random effects
- [ ] check one and two random slopes
- [ ] check Matern(1|x, y)
- [ ] check AR1(1|time)
- [ ] check corrMatrix(1|group)

### Check that it works with the 3 fitting functions
- [ ] check output from fitme()
- [ ] check output from HLfit()
- [ ] check output from corrHLfit()

### Check that it works with common families and links
- [ ] check gaussian (log + identity)
- [ ] check Gamma (log + identity)
- [ ] check Binomial (logit + probit)
- [ ] check Poisson (log + identity)
- [ ] check truncated Poisson (log)
- [ ] check NegBin (log)
- [ ] check truncated NegBin (log)

### Handle multivariate models?
- [ ] tidy for HLfitlist?
- [ ] augment for HLfitlist?
- [ ] glance for HLfitlist?

### Polish
- [ ] cleanup test file
- [ ] uncomment added comment in test-stanarm.R
- [ ] merge latest broom.mixed
- [ ] store fitted models as rda in inst/extdata (produced in run_example.R)
- [ ] update examples
- [ ] update vignette
- [ ] update NEWS
- [ ] delete this file
- [ ] remove mention of this file in .Rbuildignore
- [ ] PR
