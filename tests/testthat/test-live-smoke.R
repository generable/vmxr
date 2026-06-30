# Opt-in live smoke test against a real environment (staging). Skipped unless
# VMX_RUN_LIVE_TESTS=1 and VMX_API_BASE_URL / VMX_API_TOKEN are set. Covers the
# Phase-0 read path; does not create or upload anything.

skip_if_no_live <- function() {
  if (!identical(Sys.getenv("VMX_RUN_LIVE_TESTS"), "1")) {
    testthat::skip("live tests disabled (set VMX_RUN_LIVE_TESTS=1 to enable)")
  }
  if (!nzchar(Sys.getenv("VMX_API_BASE_URL")) || !nzchar(Sys.getenv("VMX_API_TOKEN"))) {
    testthat::skip("VMX_API_BASE_URL / VMX_API_TOKEN not set")
  }
}

test_that("health, whoami, and treatments work against the live API", {
  skip_if_no_live()
  con <- vmx_client()

  health <- vmx_health(con)
  expect_equal(health$status, "ok")

  me <- vmx_whoami(con)
  expect_s3_class(me, "vmx_me")
  expect_true(nzchar(me$email))

  tbl <- vmx_treatments(client = con)
  expect_s3_class(tbl, "tbl_df")
})
