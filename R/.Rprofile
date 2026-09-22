# renv scaffolding exists (R/renv.lock, R/renv/) so a real, versioned
# dependency manifest is committed -- see audit fix-pass C3. Deliberately NOT
# activating renv's isolated project library here yet: this project's
# packages live in the normal user/system R library, the live bot's CI
# workflow invokes Rscript --vanilla (which skips .Rprofile anyway), and
# renv's default sandboxing would hide that library from any interactive R
# session started with R/ as the working directory. Wiring up full
# renv::restore()-based isolation (both locally and in nfl_live.yml) is a
# deliberate follow-up, not done blind mid-season.
#
# source("renv/activate.R")
