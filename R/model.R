#' model.frame/matrix for dfidx objects
#'
#' Specific model.frame/matrix are provided for dfidx objects. This
#' leads to an unusual order of arguments compared to the
#' usage. Actually, the first two arguments of the model.frame method
#' are a dfidx and a formula and the only main argument of the
#' model.matrix is a dfidx which should be the result of a call to the
#' model.frame method, i.e. it should have a term attribute.
#' @param formula a `dfidx`
#' @param data a `formula`
#' @param ...,lhs,rhs,dot see the `Formula` method
#' @param alt.subset a subset of levels for the second index
#' @param reflevel a user-defined first level for the second index
#' @param balanced a boolean indicating if the resulting data.frame
#'     has to be balanced or not
#' @param x a model matrix
#' @param n the number of lines to be printed
#' @importFrom Formula as.Formula Formula
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @return a `dfidx` object for the `model.frame` method and a matrix
#'     for the `model.matrix` method.
#' @export
#' @author Yves Croissant
#' @examples
#' mn <- dfidx(munnell)
#' mf <- model.frame(mn, gsp ~ privatecap | publiccap + utilities | unemp + labor)
#' model.matrix(mf, rhs = 1)
#' model.matrix(mf, rhs = 2)
#' model.matrix(mf, rhs = 1:3)
model.frame.dfidx <- function(formula, data = NULL, ...,
                              lhs = NULL, rhs = NULL, dot = "previous",
                              alt.subset = NULL, reflevel = NULL,
                              balanced = FALSE){
    # get the data and the formula (in this unusual order)
    .data <- formula
    .is_tibble <- inherits(.data, "tbl_df")
    # coerce the formula to a Formula object if necessary
    if(inherits(data, "Formula")) .formula <- data else .formula <- as.Formula(data)
    # update the formula in order to include the indexes in the last part
    .oformula <- .formula
    .formula <- add_idx(.formula, .data)
    # coerce the data to a data.frame (index series become just
    # ordinary series) with an ids attribute (a data.frame with the
    # index series and a digit 1/2 indicating to which index they
    # refer to)
    .choice <- attr(.data, "choice")
    if (class(.data)[1] != "dfidx") .pkg <- strsplit(class(.data)[1], "_")[[1]][2]
    else .pkg <- NULL
    .data <- unfold_idx(.data)
    .idx_vector <- attr(.data, "idx_vector")
    .name <- attr(.data, "name")
    # use the Formula's model.frame method
    .data <- model.frame(.formula, .data, ...,
                         lhs = lhs, rhs = rhs, dot = dot)# %>% as_tibble
    .nterms <- attr(.data, "terms")
    # add the previously saved ids attribute
    attr(.data, "idx_vector") <- .idx_vector
#    attr(.data, "name") <- .name
    attr(.data, "choice") <- .choice
    # "fold" the index in a data.frame column to get an dfidx object
    .data <- fold_idx(.data, pkg = .pkg)

    # select a subset of alternatives if requires, which implies
    # removing the choice situations for which the chosen alternative
    # is not in the subset
    if (! is.null(alt.subset) | balanced){
        .idx <- idx(.data)
    }
    if (! is.null(alt.subset)){
        choice <- attr(.data, "choice")
        if (is.null(choice)){
            #stop("the use of alt.subset requires that a choice variable is defined")
            choice <- paste(deparse(.formula[[2]]))
        }
        .idx <- idx(.data)
        name_id1 <- idx_name(.data, 1)
        name_id2 <- idx_name(.data, 2)
        id1 <- .idx[[name_id1]]
        id2 <- .idx[[name_id2]]
        chid_kept <- subset(.idx, .data[[choice]] & id2 %in% alt.subset)[[1]]
        rows_kept <- (id1 %in% chid_kept) & (id2 %in% alt.subset)
        .data <- .data[rows_kept, ]
        .data[[idx_name(.data)]][[idx_name(.data, 2)]] <-
            .data[[idx_name(.data)]][[idx_name(.data, 2)]][drop = TRUE]
    }
    if (balanced){
        .idx <- idx(.data)
        name_id1 <- idx_name(.data, 1)
        name_id2 <- idx_name(.data, 2)
        id1 <- .idx[[name_id1]]
        id2 <- .idx[[name_id2]]        
        un_id1 <- unique(id1)
        un_id2 <- unique(id2)
        complete <- expand.grid(un_id2, un_id1, stringsAsFactors = FALSE)[, 2:1]
        names(complete) <- c(idx_name(.data, 1), idx_name(.data, 2))
        .ids <- attr(.idx, "ids")
        complete <- merge(complete, unique(.idx[.ids == 1]), all.x = TRUE)
        .data <- unfold_idx(.data)
        .idx_vector <- attr(.data, "idx_vector")
        .data <- merge(.data, complete, all.y = TRUE)
#        .data <- .data[order(.data[[name_id1]], .data[[name_id2]]),]
        attr(.data, "idx_vector") <- .idx_vector
        .data <- fold_idx(.data, pkg = .pkg)
    }
    if (! is.null(reflevel)){
        .levels <- levels(idx(.data)[[idx_name(.data, 2)]])
        if (! reflevel %in% .levels) stop("unknown reference level")
        else .data[[idx_name(.data)]][[idx_name(.data, 2)]] <-
                 relevel(.data[[idx_name(.data)]][[idx_name(.data, 2)]], reflevel)
    }
    else .levels <- NULL
    
    attr(.data, "terms") <- .nterms
    attr(.data, "formula") <- .oformula
    attr(.data, "alt.ordering") <- .levels
    if (.is_tibble){
        spec_class <- setdiff(class(.data), c("dfidx", "tbl_df", "tbl", "data.frame"))
        class(.data) <- c(spec_class, "dfidx", "tbl_df", "tbl", "data.frame")
    }
    .data
}

#' @rdname model.frame.dfidx
#' @param object a dfidx object
#' @export
model.matrix.dfidx <- function(object, ..., lhs = NULL, rhs = 1, dot = "separate"){
    if (is.null(attr(object, "formula")))
        stop("the argument is an ordinary dfidx object")
    .formula <- attr(object, "formula")
    result <- model.matrix(.formula, object, lhs = lhs, rhs = rhs, dot = dot)
    class(result) <- c("dfidx_matrix", class(result))
    result
}

#' @rdname model.frame.dfidx
#' @param n the number of lines to print
#' @export
print.dfidx_matrix <- function(x, ..., n = 10L){
    cat(glue("# [", nrow(x), " x ", ncol(x), "]"), "\n")
    class(x) <- setdiff(class(x), "dfidx_matrix")
    print(x[1:n, ])
}
