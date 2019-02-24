
detectCores <- parallel::detectCores

as_function <- rlang::as_function

#' @importFrom rlang as_function
#' @importFrom parallel detectCores
#' @export
mcmap <- function(.x, .f, ...,
                  mc.cores = getOption("mc.cores", max(2L, detectCores() - 1L))) {
  .f <- as_mapper(.f, ...)
  mclapply(.x, FUN = .f, mc.cores = mc.cores)
}

#' @export
#' @importFrom purrr flatten_int flatten_lgl flatten_dbl flatten_chr flatten_dfr flatten_dfc
mcmap_int <- function(...) {
  flatten_int(mcmap(...))
}

#' @export
mcmap_dbl <- function(...) {
  flatten_dbl(mcmap(...))
}

#' @export
mcmap_chr <- function(...) {
  flatten_chr(mcmap(...))
}

#' @export
mcmap_lgl <- function(...) {
  flatten_lgl(mcmap(...))
}

#' @export
mcmap_dfr <- function(..., .id = NULL) {
  flatten_dfr(mcmap(...), .id = .id)
}

mcmap_dfc <- function(...) {
  flatten_dfc(mcmap(...))
}



# TODO:
# DONE map_if() map_at()   walk()  pwalk()
# walk2() lmap() lmap_if() lmap_at()
# bring in background()
# document + examples + readme
# unit tests


# map_if <- function (.x, .p, .f, ...) {
#   sel <- probe(.x, .p)
#   out <- vector("list", length(.x))
#   out[sel] <- mcmap(.x[sel], .f, ...)
#   out[!sel] <- .x[!sel]
#   names(out) <- names(.x)
#   out
# }

#' @export
mcmap_if <- function (.x, .p, .f, ...) {
  sel <- probe(.x, .p)
  .x[sel] <- mcmap(.x[sel], .f, ...)
  .x
}

is_scalar <- function(x) identical(length(x), 1L)

probe <- function(.x, .p, ...) {
  if (is.logical(.p)) {
    if(is_scalar(.p))
      .p <- rep.int(.p, length(.x))
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    vapply(.x, as_mapper(.p), FALSE, ..., USE.NAMES = FALSE)
  }
}

#' @export
mcmap_at <- function (.x, .at, .f, ...) {
  sel <- inv_which(.x, .at)
  .x[sel] <- mcmap(.x[sel], .f, ...)
  .x
}

inv_which <- function (x, sel) {
  if (is.character(sel)) {
    names <- names(x)
    if (is.null(names)) {
      stop("character indexing requires a named object",
           call. = FALSE)
    }
    names %in% sel
  }
  else if (is.numeric(sel)) {
    seq_along(x) %in% sel
  }
  else {
    stop("unrecognised index type", call. = FALSE)
  }
}

#' @export
mcwalk <- function (.x, .f, ...) {
  .f <- as_mapper(.f)
  for (i in seq_along(.x))
    background(.f(.x[[i]], ...))
  background()
  invisible(.x)
}


#' @importFrom purrr transpose
mcpwalk <- function (.l, .f, ...) {
  .f <- as_mapper(.f)
  args_list <- transpose(recycle_args(.l))
  e <- environment()
  call_expr <- as.call(c(quote(.f), args, quote(...)))
  for (args in args_list)
    background(eval(call_expr, envir = e))
  invisible(.l)
}








#' @importFrom parallel mcmapply
#' @export
mcmap2 <- function(.x, .y, .f, ...,
                   mc.cores = getOption("mc.cores", max(2L, detectCores() - 1L))) {
  .f <- as_mapper(.f, ...)
  res <- mcmapply(FUN = .f, .x, .y,
    SIMPLIFY = FALSE, mc.cores = mc.cores, USE.NAMES = FALSE)
  names(res) <- names(.x)
  res
}

#' @export
mcmap2_int <- function(...) {
  flatten_int(mcmap2(...))
}

#' @export
mcmap2_dbl <- function(...) {
  flatten_dbl(mcmap2(...))
}

#' @export
mcmap2_chr <- function(...) {
  flatten_chr(mcmap2(...))
}

#' @export
mcmap2_lgl <- function(...) {
  flatten_lgl(mcmap2(...))
}

#' @export
mcmap2_dfr <- function(..., .id = NULL) {
  flatten_dfr(mcmap2(...), .id = .id)
}


# https://github.com/r-lib/rlang/blob/master/R/compat-purrr.R
recycle_args <- function(args) {
  lens <- lengths(args, use.names = FALSE)
  n <- max(lens)

  stopifnot(lens %in% c(1L, n))
  to_recycle <- lens == 1L
  args[to_recycle] <- lapply(args[to_recycle], function(x) rep.int(x, n))

  args
}

#' @export
mcpmap <- function(.l, .f, ...,
                   mc.cores = getOption("mc.cores", pmax(2L, detectCores() - 1L))
                   ) {
  .f <- as_mapper(.f, ...)
  args <- recycle_args(.l)
  res <- do.call(mcmapply,
                 c(list(.f), args,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE))
  names(res) <- names(.l[[1]])
  res
}

#' @export
mcpmap_dbl <- function(...) {
  flatten_dbl(mcpmap(...))
}

#' @export
mcpmap_int <- function(...) {
  flatten_int(mcpmap(...))
}

#' @export
mcpmap_chr <- function(...) {
  flatten_chr(mcpmap(...))
}

#' @export
mcpmap_lgl <- function(...) {
  flatten_lgl(mcpmap(...))
}

#' @export
mcpmap_dfr <- function(..., .id = NULL) {
  flatten_dfr(mcpmap(...), .id = .id)
}

