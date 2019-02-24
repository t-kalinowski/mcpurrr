

x <- as.list(1:5)
x[6] <- list(1:3)
x
storage.mode(x) <- "integer"
x

x <- as.list("")
vapply(x, identity, 0L)
storage.mode(x) <- "integer"
x


x <- as.list(1:10000)
set <- function() {
  storage.mode(x) <- "integer"
  x
}

set_w_chk <- function() {
  stopifnot(lengths(x) == 1L, vapply(x, typeof, "") == "integer")
  storage.mode(x) <- "integer"
  x
}

vap <- function() {
  vapply(x, identity, 0L)
}

library(purrr)

flt <- function() {
  flatten_int(x)
}

unl <- function() {
  stopifnot(lengths(x) == 1L, vapply(x, typeof, "") == "integer")
  unlist(x, recursive = FALSE)
}

unl_noch <- function() {
  # stopifnot(lengths(x) == 1L, vapply(x, typeof, "") == "integer")
  unlist(x, recursive = FALSE)
}

bench::mark(set(), vap(), flt(), unl(), unl_noch(), set_w_chk()) %>%
  print() %>% plot()
