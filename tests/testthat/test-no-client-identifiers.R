# Guard: none of the identifiers configured in VMXR_FORBIDDEN_IDENTIFIERS may
# appear in this package's own text -- in any file's contents or in any file path.
# See AGE-223. The scanning logic lives in tests/testthat/helper-scrub.R.
#
# The list is read from the environment at run time, one identifier per line. With
# the variable unset or empty this test skips rather than passing, so a missing
# configuration is visible in the test output instead of looking like a clean run.
# A hit is reported by file location and count only; the matched token is never
# printed.

# Walk up from `start` to the package source root (a dir with both DESCRIPTION and
# R/). Returns NULL when run from an installed/checked layout where the sources are
# not present -- e.g. inside `R CMD check`, where only the copied tests/ tree and
# the installed package are reachable.
scrub_find_pkg_root <- function(start) {
  d <- start
  for (i in seq_len(12)) {
    if (file.exists(file.path(d, "DESCRIPTION")) && dir.exists(file.path(d, "R"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NULL
}

test_that("no disallowed identifiers appear in the package tree", {
  forbidden <- scrub_forbidden()
  if (!length(forbidden)) {
    skip("VMXR_FORBIDDEN_IDENTIFIERS is not configured; identifier list unavailable")
  }

  here <- normalizePath(test_path("."), winslash = "/", mustWork = TRUE)
  pkg_root <- scrub_find_pkg_root(here)

  if (!is.null(pkg_root)) {
    # Source checkout (devtools::test(), testthat::test_local(), the scrub-check
    # CI job): scanning the package root covers NEWS.md, README.md, R/, tests/
    # (incl. fixtures and recorded response bodies), man/, vignettes/, DESCRIPTION.
    roots <- pkg_root
  } else {
    # Installed/checked layout: scan what is reachable -- the copied tests/ tree
    # (test files + fixtures + recorded bodies) and the installed package dir
    # (which carries NEWS.md and DESCRIPTION). The full-source pass runs in the
    # scrub-check CI workflow and in local dev runs.
    roots <- c(here, system.file(package = "vmxr"))
    roots <- roots[nzchar(roots)]
  }

  hits <- scrub_scan(roots, forbidden)

  msg <- NULL
  if (nrow(hits) > 0) {
    # Locations and a count only -- the matched token is not interpolated here.
    msg <- paste0(
      "A configured identifier matched in ", nrow(hits), " location(s); ",
      "the matched text is not printed. Clean up:\n",
      paste0("  - ", hits$file, " [", hits$where, "]", collapse = "\n")
    )
  }
  expect_equal(nrow(hits), 0L, info = msg)
})

test_that("the environment list parses one identifier per line", {
  expect_equal(scrub_forbidden(""), character(0))
  expect_equal(scrub_forbidden("   \n\n  "), character(0))
  expect_equal(scrub_forbidden("zzqxprobe"), "zzqxprobe")
  # Blank lines trimmed, case folded, separators normalised away, duplicates dropped.
  expect_equal(
    scrub_forbidden("ZzqxProbe\n\n  zqqxprobe-two  \nzzqxprobe\n"),
    c("zzqxprobe", "zqqxprobetwo")
  )
})

test_that("the scanner detects a configured token in every obfuscation form", {
  # Positive control that never references a real identifier: a synthetic probe
  # token confirms the machinery (candidate extraction + matching) fires.
  probe <- scrub_forbidden("zzqxprobe")

  # Detected: plain, and obfuscated with . - _ between the letters.
  expect_length(scrub_match_text("uses the zzqxprobe workspace", probe), 1)
  expect_length(scrub_match_text("z-z-q-x-p-r-o-b-e", probe), 1)
  expect_length(scrub_match_text("z.z.q.x.p.r.o.b.e", probe), 1)
  expect_length(scrub_match_text("z_z_q_x_p_r_o_b_e", probe), 1)

  # Detected: as a segment of a compound id and inside a file path.
  expect_length(scrub_match_text("zzqxprobe-022-dv", probe), 1)
  expect_length(scrub_match_text("fixtures/zzqxprobe-1/dv.json", probe), 1)

  # Detected: separated letters fused into a compound id or a dotted filename.
  expect_length(scrub_match_text("z-z-q-x-p-r-o-b-e-022-dv", probe), 1)
  expect_length(scrub_match_text("z.z.q.x.p.r.o.b.e.json", probe), 1)

  # NOT detected: token embedded in a larger word (no boundary), or a partial.
  expect_length(scrub_match_text("azzqxprobeb", probe), 0)
  expect_length(scrub_match_text("zzqxprob", probe), 0)

  # Generic API prefixes, environment kinds and synthetic fixture constants stay
  # clean: nothing matches unless it is on the configured list.
  expect_length(scrub_match_text("test-022-dv staging std_FIXTURE", probe), 0)

  # An empty list matches nothing at all.
  expect_length(scrub_match_text("zzqxprobe", character(0)), 0)
})

test_that("the scan catches a configured token in a file path even for a binary file", {
  probe <- scrub_forbidden("zzqxprobe")

  root <- file.path(tempdir(), paste0("scrub-", as.integer(Sys.time())))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zzqxprobe-1"), recursive = TRUE)
  # A binary recorded body whose contents are never read as text: the name
  # survives only in the directory path, and must still be caught.
  writeBin(as.raw(c(0L, 1L, 2L, 255L)), file.path(root, "zzqxprobe-1", "body.rds"))

  hits <- scrub_scan(root, probe)
  expect_true(any(hits$where == "path"))
})
