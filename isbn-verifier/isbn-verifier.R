is_valid <- function(isbn) {
  chars <- gsub("-", "", isbn) |>
    strsplit("") |>
    unlist()
  len <- length(chars)
  if (len > 0 && chars[len] == "X") chars[len] <- "10"

  # need to suppress the "NAs introduced by coercion" warning
  suppressWarnings(nums <- as.numeric(chars))
  if (len != 10 || any(is.na(nums))) {
    return(FALSE)
  }
  sum(nums * 10:1) %% 11 == 0
}
