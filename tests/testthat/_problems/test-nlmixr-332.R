# Extracted from test-nlmixr.R:332

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vmxr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")
nlmixr_core_cols <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "CMT", "RATE", "II",
                      "ADDL", "SS", "CENS", "LIMIT", "subject_id")

# test -------------------------------------------------------------------------
dosing <- nlmixr_dosing_body()
dosing$rows[[3]]["dose_rate"] <- list(NULL)
httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = nlmixr_pk_body(), dosing = dosing,
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con),
               class = "vmx_response_error", regexp = "dose_rate")
pk <- nlmixr_pk_body()
pk$rows[[3]]["lloq"] <- list(NULL)
httr2::local_mocked_responses(nlmixr_mock(tables = list(
    subjects = nlmixr_subjects_body(), pk = pk, dosing = nlmixr_dosing_body(),
    pd = nlmixr_pd_body(), covariates = nlmixr_covariates_body()
  )))
expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con),
               class = "vmx_response_error", regexp = "lloq")
httr2::local_mocked_responses(function(req) {
    resp <- nlmixr_mock()(req)
    body <- httr2::resp_body_json(resp)
    if (!is.null(body$domain)) body$time_basis <- "nominal"
    httr2::response_json(body = body)
  })
expect_error(vmx_nlmixr_data("dv_1", analyte = "drug", client = con), class = "vmx_response_error")
httr2::local_mocked_responses(function(req) {
    resp <- nlmixr_mock()(req)
    body <- httr2::resp_body_json(resp)
    if (!is.null(body$domain)) body$time_basis <- NULL
    httr2::response_json(body = body)
  })
expect_error(vmx_pk("dv_1", client = con), class = "vmx_response_error")
