# Shared scanner for the "no disallowed identifiers" guard (AGE-223).
#
# vmxr is a public package, and some client / workspace / study identifiers are
# not meant to appear in it. This scanner reports any file in the package tree
# whose contents or whose path contains one of those identifiers. It is used by:
#   * tests/testthat/test-no-client-identifiers.R  (runs in the test suite), and
#   * .github/workflows/scrub-check.yml            (runs over the full checkout in CI).
#
# The list of identifiers is not part of this repository. It is supplied at run
# time in the environment variable VMXR_FORBIDDEN_IDENTIFIERS, one identifier per
# line. When that variable is unset or empty there is nothing to scan for and the
# callers say so explicitly (the test skips; the CI job is a no-op).
#
# Matching is on whole normalised tokens. Nothing in this file needs a package
# beyond base R and `tools`.

# --- configured identifier list --------------------------------------------------

# Normalise one identifier or candidate token: fold case and drop the intra-name
# separators ". - _", so "A.C-M_E" and "acme" are the same token.
scrub_normalise <- function(x) {
  gsub("[._-]+", "", tolower(trimws(x)), perl = TRUE)
}

# Read the identifier list from the environment. One identifier per line; blank
# lines are dropped. Returns character(0) when the variable is unset or empty.
scrub_forbidden <- function(value = Sys.getenv("VMXR_FORBIDDEN_IDENTIFIERS", "")) {
  if (!nzchar(value)) return(character(0))
  items <- strsplit(value, "\r?\n", perl = TRUE)[[1]]
  items <- scrub_normalise(items)
  unique(items[nzchar(items)])
}

# --- token extraction -----------------------------------------------------------

# Extract candidate tokens of one of the allowed `lengths` from a blob of text
# (file contents or a path string).
#
# The text is cut into "fields" on any character that is neither alphanumeric nor
# an intra-name separator (". - _"). Within a field, the "." "-" "_" runs are the
# atom boundaries, and we emit every CONTIGUOUS run of atoms (joined, separators
# removed) whose combined length is one of `lengths`. This single rule catches
# every reintroduction form the guard must stop -- all on WHOLE tokens, never
# substrings, so an ordinary word that merely contains a short slug as a substring
# cannot match:
#   * plain                "acme"                    -> atom "acme"
#   * separators between    "a-c-m-e" / "a.c.m.e"    -> atoms a,c,m,e -> join "acme"
#     the letters
#   * path / dir / id       "fixtures/acme-022-dv/x" -> field "acme-022-dv"
#     segment                                           -> atom "acme"
#   * separated letters     "a-c-m-e-022-dv", "a.c.m.e.json"
#     fused into a compound                             -> atoms a,c,m,e,.. -> "acme"
# A separator-free word is a single atom and yields only itself, so there are no
# substring false positives. Only lengths in `lengths` are built, which bounds the
# work per field to that maximum.
scrub_candidates <- function(text, lengths) {
  maxlen <- max(lengths)
  lset <- unique(lengths)
  low <- tolower(paste(text, collapse = "\n"))
  fields <- strsplit(low, "[^a-z0-9._-]+", perl = TRUE)[[1]]
  # Deduplicate fields first: candidates are unioned anyway, and one repeated token
  # (e.g. "022" across a large JSON body) then costs work once, not once per copy.
  fields <- unique(fields[nzchar(fields)])
  if (!length(fields)) return(character(0))

  has_sep <- grepl("[._-]", fields, perl = TRUE)
  # Fast path: a separator-free field is a single atom -> it is itself the only
  # candidate, kept when its length is one we care about. This covers the large
  # majority of tokens in recorded JSON bodies and source, vectorised.
  simple <- fields[!has_sep]
  cand <- simple[nchar(simple) %in% lset]

  # Slow path: only the fields that actually contain "." "-" "_".
  multi <- fields[has_sep]
  if (length(multi)) {
    out <- vector("list", length(multi))
    for (fi in seq_along(multi)) {
      atoms <- strsplit(multi[fi], "[._-]+", perl = TRUE)[[1]]
      atoms <- atoms[nzchar(atoms)]
      n <- length(atoms)
      if (!n) next
      cc <- character(0)
      for (i in seq_len(n)) {
        acc <- ""
        for (j in i:n) {
          acc <- paste0(acc, atoms[j])
          w <- nchar(acc)
          if (w > maxlen) break      # monotonic: no shorter join possible past here
          if (w %in% lset) cc <- c(cc, acc)
        }
      }
      out[[fi]] <- cc
    }
    cand <- c(cand, unlist(out, use.names = FALSE))
  }
  unique(cand)
}

# Match against a pre-computed set of candidate lengths (internal; `scrub_scan`
# computes the lengths once for the whole run rather than per file).
.scrub_match <- function(text, forbidden, lengths) {
  if (!length(forbidden)) return(character(0))
  cand <- scrub_candidates(text, lengths)
  if (!length(cand)) return(character(0))
  cand[cand %in% forbidden]
}

# Return the subset of `text`'s candidate tokens that are in `forbidden`.
# (The returned tokens ARE the configured identifiers; callers must not print them.)
scrub_match_text <- function(text, forbidden) {
  forbidden <- unique(forbidden[nzchar(forbidden)])
  if (!length(forbidden)) return(character(0))
  .scrub_match(text, forbidden, unique(nchar(forbidden)))
}

# --- file enumeration -----------------------------------------------------------

# Directories never scanned.
.scrub_exclude_dirs <- c(".git", ".Rproj.user", ".Rcheck")
# Files whose *contents* are not read as text. Their PATHS are still scanned -- a
# client-named directory holding only a binary recorded body must still be caught.
.scrub_binary_ext <- c(
  "png", "jpg", "jpeg", "gif", "ico", "svg", "pdf", "woff", "woff2", "ttf", "eot",
  "rds", "rda", "rdata", "rdx", "rdb", "zip", "gz", "bz2", "xz", "tar", "o", "so",
  "dll", "dylib", "class", "jar", "parquet", "feather", "arrow", "xlsx", "xls"
)

# List files under `root` to scan, as paths relative to `root`. Skips VCS/build
# dirs. Binary files ARE included (their paths get scanned; their contents are
# skipped later).
scrub_list_files <- function(root) {
  all <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE,
                    include.dirs = FALSE)
  keep <- vapply(all, function(rel) {
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    !any(parts %in% .scrub_exclude_dirs)
  }, logical(1), USE.NAMES = FALSE)
  all[keep]
}

.scrub_is_binary_ext <- function(rel) {
  ext <- tolower(tools::file_ext(rel))
  nzchar(ext) && ext %in% .scrub_binary_ext
}

# Read a file as text; returns NA if it looks binary (extension or NUL byte).
.scrub_read_text <- function(path, rel) {
  if (.scrub_is_binary_ext(rel)) return(NA_character_)
  info <- file.info(path)
  n <- if (is.na(info$size)) 0 else info$size
  raw <- readBin(path, "raw", n = n)
  if (length(raw) && any(raw == as.raw(0L))) return(NA_character_)
  rawToChar(raw)
}

# --- top-level scan -------------------------------------------------------------

# Scan every in-scope file under each root in `roots` for the identifiers in
# `forbidden` (already normalised, e.g. by scrub_forbidden()). Returns a data frame
# of hits with columns: root, file, where ("path" or "content"). The matched token
# is deliberately NOT returned, so hits can be reported by location alone.
scrub_scan <- function(roots, forbidden) {
  forbidden <- unique(forbidden[nzchar(forbidden)])
  empty <- data.frame(root = character(), file = character(), where = character(),
                      stringsAsFactors = FALSE)
  if (!length(forbidden)) return(empty)
  lengths <- unique(nchar(forbidden))

  hits <- list()
  for (root in roots) {
    if (!dir.exists(root)) next
    files <- scrub_list_files(root)
    for (rel in files) {
      # The path itself can disclose a name (e.g. a fixture directory name); scan
      # it for EVERY file, including binaries whose contents we do not read.
      if (length(.scrub_match(rel, forbidden, lengths))) {
        hits[[length(hits) + 1L]] <- data.frame(
          root = root, file = rel, where = "path", stringsAsFactors = FALSE)
      }
      txt <- .scrub_read_text(file.path(root, rel), rel)
      if (is.na(txt)) next
      if (length(.scrub_match(txt, forbidden, lengths))) {
        hits[[length(hits) + 1L]] <- data.frame(
          root = root, file = rel, where = "content", stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(hits)) return(empty)
  do.call(rbind, hits)
}
