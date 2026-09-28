# Guide 2 — Building, testing and publishing an R package

Everything here is about ggcarto specifically, but none of it is specific to
ggcarto.

## 1. What a package is

A package is a folder with a fixed layout that R knows how to install. The
layout is the contract; `R CMD check` enforces it.

```
ggcarto/
├── DESCRIPTION        who, what, version, dependencies. The one required file.
├── NAMESPACE          what the package exports and imports. Generated, never hand-edited.
├── R/                 the code. Plain .R files; every file is sourced on install.
├── man/               help pages (.Rd). Generated from comments in R/.
├── tests/
│   ├── testthat.R     the runner (three fixed lines)
│   └── testthat/      test-*.R files
├── vignettes/         long-form docs, rendered to HTML on install
├── inst/              anything to ship verbatim (not used yet)
├── LICENSE            the MIT text
├── NEWS.md            changelog, one section per version
├── README.md          the GitHub landing page
├── .Rbuildignore      what to leave out of the build (dev/, README, .Rproj)
└── dev/               your scratch scripts; ignored by the build
```

### DESCRIPTION, field by field

* `Package`, `Title` (Title Case, no period), `Version`, `Description`
  (one paragraph, full sentences, package names in single quotes).
* `Authors@R` — you, with roles `aut` (author) and `cre` (maintainer).
* `License: MIT + file LICENSE`.
* `Imports` — packages your code *calls*. They are installed with yours.
  Call them as `pkg::fun()` in code; do not `library()` inside a package.
* `Suggests` — packages only examples, tests or vignettes use, or optional
  features (`paletteer`). Guard with `requireNamespace("pkg", quietly = TRUE)`.
* `Depends: R (>= 4.1)` — the R version. Do not put packages here.
* `Encoding: UTF-8`, `Roxygen: list(markdown = TRUE)`, `VignetteBuilder`.

### NAMESPACE

Lists `export(fn)` for every function users may call and `importFrom(pkg,
fn)` for anything you use without a `pkg::` prefix. roxygen2 writes it from
`@export` tags. If you ever edit it by hand roxygen will refuse to touch it
again — delete it and re-run `document()`.

### Documentation

Every exported function needs a help page. Write it as `#'` comments above
the function:

```r
#' Title in one line
#'
#' Longer description. Markdown works: `code`, [other_fn()], *italics*.
#'
#' @param x What it is and what it accepts.
#' @return What comes back.
#' @examples
#' my_fn(1:10)
#' @export
my_fn <- function(x) ...
```

`devtools::document()` turns these into `man/*.Rd` and updates NAMESPACE.
Examples are *run* during check, so they must work, be fast, and not need
packages that may be missing (wrap those in `@examplesIf
requireNamespace("paletteer")`).

### Tests

`tests/testthat/test-<topic>.R`, each a set of `test_that("what it should
do", { expect_equal(...) })` blocks. Write a test for every promise the
documentation makes, and one for every bug you fix so it stays fixed.
`testthat::test_local()` runs them; `devtools::test()` is the same thing.

The ggcarto tests are a decent model: one file per source file, checks of
the *behaviour* (n classes give n colours; breaks come from the data not the
limits; corners of a bivariate palette are the corner colours), not of the
implementation.

### Vignettes

Long-form documentation that runs. The Quarto vignette needs the package
*installed* (not `load_all()`'d) because it renders in a fresh session.

## 2. The daily loop

```r
devtools::load_all()   # source everything into the session, as if installed
# ... edit, try things ...
devtools::document()   # regenerate man/ and NAMESPACE after changing comments
devtools::test()       # run the tests
devtools::check()      # the full audit, before every commit that matters
```

`load_all()` is the one to internalise: it makes the package's *internal*
functions available too, so you can poke at `compute_breaks()` helpers that
are not exported.

## 3. `R CMD check`

`devtools::check()` builds a tarball and runs 50-odd checks: does it install,
do the examples run, do the tests pass, does every exported function have
documentation that matches its signature, are there undeclared dependencies,
are there undefined variables in the code. The output ends with

```
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

and that line is the goal. Errors block; warnings must be fixed before CRAN;
notes are explained in the submission comment if you cannot remove them.
Common ones and the fix:

| Message | Fix |
|---|---|
| "no visible binding for global variable 'x'" | you used a column name bare inside dplyr/ggplot code; use `.data$x` or `utils::globalVariables()` |
| "Undocumented code objects" | missing `@export`-ed function's roxygen block, or `document()` not re-run |
| "Namespace in Imports field not imported from" | a package in Imports you do not actually call; remove it |
| "Packages suggested but not available" | fine locally; on CRAN they are installed |
| "checking examples ... ERROR" | an example fails or needs a Suggests package; guard it |

ggcarto-specific: the current `check()` on ggplot2 4.x should be clean. The
`ggplot2 (>= 4.0.0)` pin in DESCRIPTION is deliberate — `theme_palettes()`
needs it and the shim in `utils.R` can go once 3.x is dropped.

## 4. Version control and GitHub

```
git init && git add . && git commit -m "ggcarto 0.2.0"
gh repo create jcapellan/ggcarto --public --source . --push
```

Then `usethis::use_github_action("check-standard")` adds a workflow that runs
`R CMD check` on Linux, macOS and Windows for every push. That is your free
CI, and the first time it catches a Windows-only path bug you will be glad.

Version numbers: `major.minor.patch`. Bump `minor` for new features
(0.1.0 → 0.2.0), `patch` for fixes. Development versions carry a fourth
number (`0.2.0.9000`) so an installed dev build is distinguishable from the
release. `usethis::use_version()` does the bump and opens NEWS.md for you.

## 5. Installing from GitHub

Once pushed, anyone can

```r
remotes::install_github("jcapellan/ggcarto")
```

This is a perfectly respectable way to publish. Most R packages live only
here. Do this first, use the package for a few weeks, and fix what annoys
you before considering CRAN.

## 6. pkgdown (a website, free)

```r
usethis::use_pkgdown_github_pages()
```

builds a site from README, the reference pages and the vignette, and
publishes it to `jcapellan.github.io/ggcarto` on every push. Ten minutes of
setup and it makes the package look finished.

## 7. CRAN

CRAN is the official repository: `install.packages("ggcarto")` works, and the
package is checked daily on a dozen platforms. It is worth it for reach and
credibility; it costs a stricter check and a human reviewer.

**Before submitting**

1. `devtools::check()` clean locally.
2. `devtools::check_win_devel()` — emails you a Windows check from CRAN's own
   machine. `rhub::rhub_check()` for more platforms.
3. `urlchecker::url_check()` — every URL in the docs must resolve.
4. `spelling::spell_check_package()`.
5. Examples run in under 5 seconds each; tests in under a minute total;
   nothing writes to the user's home directory; nothing needs the network
   without a guard.
6. `cran-comments.md` (`usethis::use_cran_comments()`): what you checked on,
   and any notes you could not remove and why.
7. Every Suggests package must be *usable* when missing — CRAN checks with
   `_R_CHECK_FORCE_SUGGESTS_=false`.

**Submitting**

`devtools::release()` walks you through it: it re-checks, asks the questions,
uploads to cran.r-project.org/submit.html, and you confirm by email. A human
reviews within a few days. First submissions almost always get a reply with
small requests (Title case; `\value` sections in every Rd; `\dontrun` → `\donttest`;
"please add a reference for the methods in the Description"). Fix, bump the
patch version, resubmit. Be polite and brief in the response.

**After acceptance**

Tag the release on GitHub (`usethis::use_github_release()`), and from then on
every push to CRAN is a *release*: fix bugs on a branch, accumulate features,
and submit no more than every couple of months — CRAN dislikes frequent
updates. When ggplot2 or classInt release a new version that breaks you,
CRAN emails you with a deadline; the GitHub Action running on the
development versions (`check-standard` does this) warns you first.

## 8. Order of operations for ggcarto, today

```r
setwd("path/to/ggcarto")
install.packages(c("devtools", "roxygen2", "testthat", "usethis",
                   "classInt", "ggplot2", "patchwork", "quarto"))
devtools::document()
devtools::test()
devtools::check()
devtools::install()
quarto::quarto_render("vignettes/ggcarto.qmd")   # look at it
usethis::use_git(); usethis::use_github()
usethis::use_github_action("check-standard")
usethis::use_pkgdown_github_pages()
```

Use it for a while. When the API stops changing, section 7.
