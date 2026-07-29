test_that("new options do not shift existing positional client arguments", {
  expect_identical(
    head(names(formals(vmx_treatments)), 2),
    c("status", "client")
  )
  expect_identical(
    head(names(formals(vmx_studies)), 3),
    c("treatment", "status", "client")
  )
  expect_identical(
    head(names(formals(vmx_upload)), 7),
    c("study", "files", "mode", "treatment", "config", "wait", "client")
  )
  expect_identical(
    head(names(formals(vmx_datasets)), 3),
    c("study", "treatment", "client")
  )
  expect_identical(
    head(names(formals(vmx_dataset_files)), 2),
    c("dataset", "client")
  )
  expect_identical(
    head(names(formals(vmx_data_versions)), 5),
    c(
      "treatment", "study", "include_archived", "eligible_for_modeling",
      "client"
    )
  )
  expect_identical(
    head(names(formals(vmx_nca_analyses)), 6),
    c(
      "data_version", "study", "treatment", "status", "time_basis",
      "client"
    )
  )
  expect_identical(
    head(names(formals(vmx_model_build_runs)), 5),
    c("data_version", "study", "treatment", "status", "client")
  )
  expect_identical(
    head(names(formals(vmx_model_build_logs)), 2),
    c("run", "client")
  )
  expect_identical(
    head(names(formals(vmx_model_fits)), 6),
    c("run", "data_version", "model_type", "marker_name", "status", "client")
  )
  expect_identical(
    head(names(formals(vmx_dosing_input)), 4),
    c("fit", "dosing_text", "scenario_name", "client")
  )
  expect_identical(
    head(names(formals(vmx_sim_jobs)), 2),
    c("fit", "client")
  )
  expect_identical(
    head(names(formals(vmx_analysis_log)), 8),
    c(
      "study", "kind", "event_type", "outcome", "severity", "since",
      "resource", "client"
    )
  )

  collection_functions <- list(
    vmx_treatments,
    vmx_studies,
    vmx_datasets,
    vmx_dataset_files,
    vmx_data_versions,
    vmx_nca_analyses,
    vmx_model_build_runs,
    vmx_model_build_logs,
    vmx_model_fits,
    vmx_sim_jobs,
    vmx_analysis_log
  )
  for (fn in collection_functions) {
    expect_false(any(c("cursor", "limit") %in% names(formals(fn))))
  }
})
