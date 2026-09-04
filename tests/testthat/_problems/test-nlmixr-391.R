# Extracted from test-nlmixr.R:391

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vmxr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")
nlmixr_core_cols <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "CMT", "RATE", "II",
                      "ADDL", "SS", "CENS", "LIMIT", "subject_id")

# test -------------------------------------------------------------------------
log <- new.env()
httr2::local_mocked_responses(nlmixr_mock(log, dv = nlmixr_legacy_dv_body()))
tbl <- vmx_pk("dv_1", client = con)
expect_null(attr(tbl, "time_basis"))
