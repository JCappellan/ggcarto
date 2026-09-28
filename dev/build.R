# Build helper. Run from the package root (the folder holding DESCRIPTION).
#
# The vignette does library(ggcarto), and Quarto renders in a fresh R session,
# so devtools::load_all() is not enough -- the package has to be installed.

# 1. Dependencies -----------------------------------------------------------
needed <- c("classInt", "ggplot2", "scales")          # Imports
suggested <- c("dplyr", "maps", "patchwork", "quarto",  # vignette
               "RColorBrewer", "viridisLite",           # palettes
               "devtools", "roxygen2", "testthat")      # development
missing <- setdiff(c(needed, suggested), rownames(installed.packages()))
if (length(missing)) install.packages(missing)

stopifnot(packageVersion("ggplot2") >= "4.0.0")

# 2. Documentation ----------------------------------------------------------
# NAMESPACE ships hand-written and man/ is empty; this generates both from the
# roxygen comments already in R/.
devtools::document()

# 3. Install and test -------------------------------------------------------
devtools::install(dependencies = FALSE, upgrade = "never")
devtools::test()

# 4. Vignette ---------------------------------------------------------------
# Only after step 3 -- library(ggcarto) fails otherwise.
quarto::quarto_render("vignettes/ggcarto.qmd")

# ...or from a terminal in the package root:
#   quarto preview vignettes/ggcarto.qmd --to html --no-watch-inputs --no-browse

# 5. Full check -------------------------------------------------------------
# devtools::check()
