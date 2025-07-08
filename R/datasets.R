#' Productivity in the United States
#' @name munnell
#' @keywords dataset
#' @description a panel data of 48 American States for 17 years, from 1970 to 1986
#' @format a data frame containing:
#' - state: the state
#' - year: the year
#' - region: one of the 9 regions of the United States
#' - president: the name of the president for the given year
#' - publiccap: public capital stock
#' - highway: highway and streets
#' - water: water and sewer facilities
#' - utilities: othe public building and structures
#' - privatecap: private capital stock
#' - gsp: gross state product
#' - labor: labor input measured by the employment in non--agricultural payrolls
#' - unemp: state unemployment rate
#' @source
#' Online complements to Baltagi (2001): \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#' Online complements to Baltagi (2013): \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @references
#' \insertRef{BALT:01}{dfidx}
#'
#' \insertRef{BALT:13}{dfidx}
#'
#' \insertRef{BALT:PINN:95}{dfidx}
#' 
#' \insertRef{MUNN:90}{dfidx}
#' @importFrom Rdpack reprompt
"munnell"

#' @name munnell_wide
#' @rdname munnell
"munnell_wide"

