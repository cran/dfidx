## -----------------------------------------------------------------------------
#| label: setup
#| include: false
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, widht = 50)
old_options <- options(width = 70,
                       tibble.print_max = 3,
                       tibble.print_min = 3
)


## -----------------------------------------------------------------------------
#| label: load_dfidx
library(dfidx)


## -----------------------------------------------------------------------------
#| label: print.tibble
head(munnell, 3)


## -----------------------------------------------------------------------------
#| label: print.dfidx
dfidx(munnell) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: extract_idx
munnell |> dfidx() |> idx() |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: dfidx_integer
munnell |> dfidx(48) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: dfidx_integer_pretty
munnell |> dfidx(48, idnames = c("state", "year"),
                 levels = 1970:1986) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: dfidx_one_index
munnell |> dfidx("state", idnames = c(NA, "date"),
                 levels = 1970:1986) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: dfidx_two_indexes
munnell |> dfidx(c("state", "year")) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: one_or_two_nests
mn <- munnell |> dfidx(c(region = "state", "year"))
mn <- munnell |> dfidx(c(region = "state", president = "year"))
mn |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: idx_two_nests
idx(mn) |> print(n = 3)


## ----position_name------------------------------------------------------------
dfidx(munnell, idx = c(region = "state", president = "year"),
            name = "index", position = 4) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: munnell_wide
head(munnell_wide, 3)


## -----------------------------------------------------------------------------
#| label: varying
munnell_wide |> dfidx(varying = 3:36, sep = "_") |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: varying_pretty
munnell_wide |> dfidx(idx = c(region = "state"), varying = 3:36, 
                      sep = "_", idnames = c(NA, "year")) |>
    print(n = 3)


## -----------------------------------------------------------------------------
#| label: idx_names
#| collapse: true
idx_name(mn)


## -----------------------------------------------------------------------------
#| label: one_idx
#| collapse: true
idx_name(mn, 2)
idx_name(idx(mn), 2)


## -----------------------------------------------------------------------------
#| label: nested_idx
#| collapse: true
idx_name(mn, 1, 1)
idx_name(mn, 1, 2)


## -----------------------------------------------------------------------------
#| collapse: true
#| label: extract_index_with_idx
id_index1 <- idx(mn, n = 1, m = 2)
id_index2 <- idx(idx(mn), n = 1, m = 2)
head(id_index1)
identical(id_index1, id_index2)


## -----------------------------------------------------------------------------
#| label: one_bracket
mn[mn$unemp > 10, ] |> print(n = 3)
mn[mn$unemp > 10, c("highway", "utilities")] |> print(n = 3)
mn[mn$unemp > 10, "highway"] |> print(n = 3)


## -----------------------------------------------------------------------------
#| collapse: true
#| label: return_series
mn1 <- mn[, "highway", drop = TRUE]
mn2 <- mn[["highway"]]
mn3 <- mn$highway
c(identical(mn1, mn2), identical(mn1, mn3))


## -----------------------------------------------------------------------------
#| label: xseries
mn1 |> print(n = 3)
class(mn1)
idx(mn1) |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: extract_index_1
head(mn$idx$president)


## -----------------------------------------------------------------------------
#| label: extract_index_2
idx(mn)$president |> head()


## -----------------------------------------------------------------------------
#| label: extract_index_3
mn$president |> print(n = 3)


## -----------------------------------------------------------------------------
#| label: pkg_series
#| collapse: true
mn <- dfidx(munnell, idx = c(region = "state", president = "year"), 
                                pkg = "pnl")
mn1 <- mn$gsp
class(mn)
class(mn1)


## -----------------------------------------------------------------------------
#| label: pnl_seriex
lag.xseries_pnl <- function(x, ...){
    .idx <- idx(x)
    class <- class(x)
    x <- unclass(x)
    id <- .idx[[1]]
    lgt <- length(id)
    lagid <- c("", id[- lgt])
    sameid <- lagid ==  id
    x <- c(NA, x[- lgt])
    x[! sameid] <- NA
    structure(x, class = class, idx = .idx)
}
lmn1 <- stats::lag(mn1)
lmn1 |> print(n = 3)
class(lmn1)
rbind(mn1, lmn1)[, 1:20]


## -----------------------------------------------------------------------------
#| label: model_frame
mf_mn <- mn |> model.frame(gsp ~ utilities + highway | unemp | labor,
                            subset = unemp > 10)
mf_mn |> print(n = 3)
formula(mf_mn)


## -----------------------------------------------------------------------------
#| label: model_matrix
mf_mn |> model.matrix(rhs = 1) |> print(n = 5)
mf_mn |> model.matrix(rhs = 2:3) |> print(n = 5)

