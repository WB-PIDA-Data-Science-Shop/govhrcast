#######################################################################
###### PREPARE THE ALLOWANCE PANEL FOR SIMULATION MODELLING ###########
#######################################################################
#
# Source: govhr::bra_hrmis_allowance -- a long-format panel keyed by
# personnel_id/contract_id/ref_date/allowance_type. govhr's own dictionary
# documents allowance_lcu as numeric, but the raw extract currently ships it
# as unparsed Brazilian currency text (e.g. "R$402,65", "R$3.156,85", and
# "R$-" for zero). That's a bug upstream in govhr's extraction, not a
# governhrcast concern -- but govhrcast should only ever simulate on data
# that already meets the clean schema, so fix it here for the bundled
# example data to the same standard we expect of any user's data.

library(data.table)

bra_hrmis_allowance <- as.data.table(govhr::bra_hrmis_allowance)

# Parse Brazilian currency strings: "R$1.234,56" -> 1234.56 (strip the "R$"
# prefix, drop "." thousands separators, convert the "," decimal separator
# to "."); "R$-" is the extract's zero marker -> 0. Values that are already
# plain numeric strings (e.g. "275", "140.73") pass through as.numeric()
# unchanged -- only "R$"-prefixed values need parsing.
.parse_brl_currency <- function(x) {
  is_brl <- grepl("^R\\$", x)

  out <- x
  out[is_brl] <- sub("^R\\$", "", out[is_brl])
  out[is_brl & out == "-"] <- "0"
  out[is_brl] <- gsub("\\.", "", out[is_brl])   # drop thousands separators
  out[is_brl] <- gsub(",", ".", out[is_brl])    # decimal comma -> decimal point

  as.numeric(out)
}

bra_hrmis_allowance[, allowance_lcu := .parse_brl_currency(allowance_lcu)]

stopifnot(!anyNA(bra_hrmis_allowance$allowance_lcu))

usethis::use_data(bra_hrmis_allowance, overwrite = TRUE)
