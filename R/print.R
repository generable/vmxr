# S3 print / format / as_tibble methods for the typed resource objects.
#
# Concrete methods are added here as each resource S3 class lands. The
# `as_tibble` generic is re-exported from tibble so `as_tibble()` dispatches on
# vmxr objects without the caller attaching tibble.

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble
