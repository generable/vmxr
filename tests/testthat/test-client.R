test_that("vmx_client resolves explicit args", {
  con <- vmx_client(base_url = "https://example.test/", token = "pat_abc")
  expect_s3_class(con, "vmx_client")
  expect_equal(con$base_url, "https://example.test") # trailing slash trimmed
})

test_that("vmx_client trims surrounding whitespace (e.g. from .Renviron)", {
  con <- vmx_client(base_url = "  https://example.test  ", token = "\tpat_abc\n")
  expect_equal(con$base_url, "https://example.test")
  expect_equal(con$token, "pat_abc")
})

test_that("vmx_client errors without a base_url", {
  withr::local_envvar(VMX_API_BASE_URL = "", VMX_API_TOKEN = "")
  expect_error(vmx_client(), class = "vmx_usage_error")
})

test_that("vmx_client errors without a token", {
  withr::local_envvar(VMX_API_TOKEN = "")
  expect_error(
    vmx_client(base_url = "https://example.test"),
    class = "vmx_auth_error"
  )
})

test_that("print redacts the token", {
  con <- vmx_client(base_url = "https://example.test", token = "pat_secret")
  out <- cli::cli_fmt(print(con))
  expect_true(any(grepl("redacted", out)))
  expect_false(any(grepl("pat_secret", out)))
})

test_that("not-yet-implemented verbs raise the skeleton error", {
  con <- vmx_client(base_url = "https://example.test", token = "pat_abc")
  expect_error(vmx_data_version_table("dv_1", client = con), class = "vmx_unimplemented_error")
})
