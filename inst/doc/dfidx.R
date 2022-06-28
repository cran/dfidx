## ----label = setup, include = FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, widtht = 65)

## -----------------------------------------------------------------------------
library("dfidx")

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
data("TravelMode", package = "AER")
head(TravelMode)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM1 <- dfidx(TravelMode, drop.index = FALSE)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
idx(TM1) %>% print(n = 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM1 %>% print(n = 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM1 <- dfidx(TravelMode)
TM1 %>% print(n = 3)
}

## ----collapse = TRUE----------------------------------------------------------
if (requireNamespace("AER")){
TM2 <- dfidx(TravelMode, idx = c("individual", "mode"))
TM3 <- dfidx(TravelMode, idx = list("individual", "mode"))
c(identical(TM2, TM3), identical(TM1, TM2))
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM3b <- dfidx(TravelMode, idnames = c(NA, "trmode"))
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TravelMode2 <- dplyr::select(TravelMode, - mode)
TM4 <- dfidx(TravelMode2, idx = "individual", idnames = c("individual", "mode"))
TM4 %>% print(n = 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TravelMode3 <- dplyr::select(TravelMode, - mode)
TM5 <- dfidx(TravelMode3, idx = 210, idnames = c("individual", "mode"))
TM5 %>% print(n = 3)
}

## ----collapse = TRUE----------------------------------------------------------
if (requireNamespace("AER")){
TM4b <- dfidx(TravelMode2, levels = c("air", "train", "bus", "car"),
                   idnames = c("individual", "mode"),
                   idx = "individual")
TM5b <- dfidx(TravelMode3, idx = 210, idnames = c("individual", "mode"),
                    levels = c("air", "train", "bus", "car"))
c(identical(TM4b, TM1), identical(TM5b, TM1))
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
data("JapaneseFDI", package = "mlogit")
JapaneseFDI <- dplyr::select(JapaneseFDI, 1:8)
head(JapaneseFDI, 3)
JP1 <- dfidx(JapaneseFDI, idx = list("firm", c("region", "country")))
head(JP1, 3)
JP1b <- dfidx(JapaneseFDI, idx = list("firm", c("region", "country")),
              idnames = c("japf", "iso80"))
head(JP1b, 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("plm")){
data("Produc", package = "plm")
head(Produc, 3)
Pr <- dfidx(Produc, idx = list(c("state", "region"), "year"))
head(Pr, 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
data("Fishing", package = "mlogit")
head(Fishing, 3)
Fi <- dfidx(Fishing, shape = "wide", varying = 2:9)
Fi2 <- dfidx(Fishing, varying = 2:9)
identical(Fi, Fi2)
head(Fi, 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
data("Fishing", package = "mlogit")
Fi <- dfidx(Fishing, shape = "wide", varying = 2:9, idnames = c("chid", "alt"))
head(Fi, 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
data("Train", package = "mlogit")
Train$choiceid <- 1:nrow(Train)
head(Train, 3)
Tr <- dfidx(Train, shape = "wide", varying = 4:11, sep = "_",
                  idx = list(c("choiceid", "id")), idnames = c(NA, "alt"))
head(Tr, 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
    idx_name(Tr)
    }

## ----collapse = TRUE----------------------------------------------------------
if (requireNamespace("mlogit")){
idx_name(Tr, 1)
idx_name(idx(Tr), 1)
idx_name(Tr$change, 1)
idx_name(Tr, 2)
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
idx_name(Tr, 1, 2)
idx_name(Tr, 2, 2)
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
idx1 <- idx(Tr)
idx2 <- idx(idx(Tr))
idx3 <- idx(Tr$change)
c(identical(idx1, idx2), identical(idx1, idx3))
}

## -----------------------------------------------------------------------------
if (requireNamespace("mlogit")){
id_index1 <- idx(Tr, n = 1, m = 2)
id_index2 <- idx(idx(Tr), n = 1, m = 2)
id_index3 <- idx(Tr$change, n = 1, m = 2)
c(identical(id_index1, id_index2), identical(id_index2, id_index3))
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM <- dfidx(TravelMode)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TMsub <- TM[TM$size == 1, ]
TMsub %>% print(n = 2)
idx(TMsub) %>% print(n = 2)
TMsub2 <- TM[TM$size == 1, c("wait", "vcost")]
TMsub3 <- TM[TM$size == 1, "wait", drop = FALSE]
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM[, c("wait", "gcost")] %>% print(n = 2)
wait1 <- TM[, c("wait"), drop = FALSE]
wait2 <- TM["wait"]
identical(wait1, wait2)
wait1 %>% print(n = 2)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
wait1 <- TM[, "wait"]
wait2 <- TM[["wait"]]
wait3 <- TM$wait
c(identical(wait1, wait2), identical(wait1, wait3))
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
wait1 %>% print(n = 3)
class(wait1)
idx(wait1) %>% print(n = 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM1$idx$mode %>% head
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
idx(TM1)$mode %>% head
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TM1$mode %>% print(n = 3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("plm")){
Pr2 <- dfidx(Produc, idx = list(c("state", "region"), "year"), pkg = "plm")
gsp1 <- Pr2[, "gsp"]
gsp2 <- Pr2[["gsp"]]
gsp3 <- Pr2$gsp
c(identical(gsp1, gsp2), identical(gsp1, gsp3))
class(gsp1)
}

## -----------------------------------------------------------------------------
if (requireNamespace("plm")){
lag.xseries_plm <- function(x, ...){
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
lgsp1 <- stats::lag(gsp1)
lgsp1 %>% print(n = 3)
class(lgsp1)
rbind(gsp1, lgsp1)[, 1:20]
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TMtb <- as_tibble(TravelMode)
class(TMtb)
TMtb %>% head(2)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
TMtb <- dfidx(TMtb, clseries = "pseries")
class(TMtb)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
ext1 <- TMtb[c("wait", "vcost")]
ext2 <- TMtb[, c("wait", "vcost")]
ext2 %>% head(2)
idx(ext2) %>% head(2)
identical(ext1, ext2)
un1 <- TMtb[, c("wait"), drop = FALSE]
un2 <- TMtb["wait"]
identical(un1, un2)
sub1 <- TMtb[TMtb$size == 2, c("wait", "vcost")]
sub2 <- TMtb[TMtb$size == 2, "wait", drop = FALSE]
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
wait1 <- TMtb[, "wait"]
wait2 <- TMtb[["wait"]]
wait3 <- TMtb$wait
c(identical(wait1, wait2), identical(wait1, wait3))
class(idx(wait1))
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
select(TM, vcost, idx, size) %>% print(n = 2)
select(TM, vcost, size) %>% print(n = 2)
arrange(TM, income, desc(vcost)) %>% print(n = 2)
mutate(TM, linc = log(income), linc2 = linc ^ 2) %>% print(n = 2)
transmute(TM, linc = log(income), linc2 = linc ^ 2) %>% print(n = 2)
filter(TM, wait <= 50, income  == 35) %>% print(n = 2)
slice(TM, 1:3)
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
pull(TM, vcost)
}

## ----eval = FALSE, include = FALSE--------------------------------------------
#  if (requireNamespace("AER")){
#  TM[, "wait"]
#  TM[, "wait", drop = FALSE]
#  TM[, "wait", drop = TRUE]
#  TM[, c("wait", "vcost")]
#  TM[, c("wait", "vcost"), drop = TRUE]
#  TM[, c("wait", "vcost"), drop = FALSE]
#  
#  TM[TM$size == 1, "wait"]
#  TM[TM$size == 1, "wait", drop = FALSE]
#  TM[TM$size == 1, "wait", drop = TRUE]
#  TM[TM$size == 1, c("wait", "vcost")]
#  TM[TM$size == 1, c("wait", "vcost"), drop = TRUE]
#  TM[TM$size == 1, c("wait", "vcost"), drop = FALSE]
#  
#  TM[1:5, "wait"]
#  TM[1:5, "wait", drop = FALSE]
#  TM[1:5, "wait", drop = TRUE]
#  TM[1:5, c("wait", "vcost")]
#  TM[1:5, c("wait", "vcost"), drop = TRUE]
#  TM[1:5, c("wait", "vcost"), drop = FALSE]
#  
#  TM["wait"]
#  TM[c("wait", "vcost")]
#  }

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
mfTM <- model.frame(TM, choice ~ vcost | income + size | travel, subset = income > 50)
mfTM %>% print(n = 3)
attr(mfTM, "terms")
attr(mfTM, "formula")
}

## -----------------------------------------------------------------------------
if (requireNamespace("AER")){
model.matrix(mfTM, rhs = 1) %>% head(2)
model.matrix(mfTM, rhs = 2) %>% head(2)
model.matrix(mfTM, rhs = 1:3) %>% head(2)
}

