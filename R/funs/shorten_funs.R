# Define more compact versions of often used functions.

# Definitions mostly belonging to base-package


# pastes
p <- function(..., sep = " ", collapse = NULL, recycle0 = FALSE)
  paste(..., sep = sep, collapse = collapse, recycle0 = recycle0)

p0 <- function(...,            collapse = NULL, recycle0 = FALSE)
  paste0(...,            collapse = collapse, recycle0 = recycle0)


# numerics
as.num <- function(x, ...)
  as.numeric(x = x, ...)
is.num <- function(x)
  is.numeric(x = x)

# characters
as.char <- function(x)
  as.character(x = x)
is.char <- function(x)
  is.character(x = x)

# logicals
isT <- function(x)
  isTRUE(x = x)
all.eq <- function(target, current, ...) 
  all.equal(target = target, current = current, ...)

# file.path
f.path <- function(..., fsep = .Platform$file.sep) 
  file.path(..., fsep = fsep)

