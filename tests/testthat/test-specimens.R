# Specimen selections for NCA, modeling options and model builds (api-contract
# §5.6.2), using httr2 mocks. Request and response bodies are built by hand from
# the contract's examples.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

pd_uuid <- "8ffb3792-8bc9-4d38-b123-fc3e2170f6ee"
pd_uuid_2 <- "9bfa1c5a-3f2e-4c81-9e04-d12f8a991a7e"

# Mock that records every request and answers by URL path.
route_mock <- function(routes) {
  env <- new.env()
  env$reqs <- list()
  httr2::local_mocked_responses(function(req) {
    env$reqs[[length(env$reqs) + 1L]] <- req
    path <- sub("\\?.*$", "", req$url)
    for (pattern in names(routes)) {
      if (grepl(pattern, path)) {
        r <- routes[[pattern]]
        return(if (inherits(r, "httr2_response")) r else httr2::response_json(body = r))
      }
    }
    httr2::response(status_code = 404L)
  }, env = parent.frame())
  env
}

api_error <- function(status, code, reason, field = NULL) {
  httr2::response_json(
    status_code = status,
    body = list(error = vmx_compact(list(
      code = code, reason = reason, message = "Request rejected.", field = field
    )))
  )
}

marker_analysis <- function(specimens = c("plasma", "serum"), recommended = "plasma") {
  list(
    data_version_id = "dv_1",
    recommended_pk_specimen = if (is.null(recommended)) NA else
      list(value = recommended, reason = "Most PK observations."),
    marker_analysis = list(
      pk_markers = lapply(specimens, function(s) list(
        gen_uuid = "77777777-7777-4777-8777-777777777777",
        name = "drug", specimen_type = s, display_name = "Drug",
        type = "positive_continuous", unit = "mg/L"
      )),
      pd_markers = list(),
      time_bases = list()
    )
  )
}

nca_created <- list(
  nca_id = "nca_1", data_version_id = "dv_1", status = "queued",
  inputs = list(time_basis = "observed", bloq_handling = "discard")
)
run_created <- function(pd_markers = list()) {
  list(
    run_id = "run_1", data_version_id = "dv_1", time_basis = "observed",
    status = "queued", label = NA, pd_markers = pd_markers
  )
}
options_echo <- list(data_version_id = "dv_1", options = list(), modeling_population = list())

# ---- PK specimen -----------------------------------------------------------

test_that("vmx_nca sends a single PK specimen", {
  env <- route_mock(list("/nca-analyses$" = nca_created))
  vmx_nca("dv_1", "observed", pk_specimen = "plasma", wait = FALSE, client = con)
  expect_equal(env$reqs[[1]]$body$data$pk_specimen, list(specimen_type = "plasma"))
})

test_that("vmx_nca sends All specimens only when asked explicitly", {
  env <- route_mock(list("/nca-analyses$" = nca_created))
  vmx_nca("dv_1", "observed", pk_specimen = vmx_all_specimens(), wait = FALSE,
          client = con)
  body <- env$reqs[[1]]$body$data
  expect_equal(body$pk_specimen, list(all_specimens = TRUE))
  # Serialised exactly as the contract's request example.
  json <- jsonlite::toJSON(body$pk_specimen, auto_unbox = TRUE)
  expect_equal(as.character(json), '{"all_specimens":true}')
})

test_that("a missing PK specimen fails, lists the choices and never applies the recommendation", {
  env <- route_mock(list("/marker-analysis$" = marker_analysis()))
  for (call in list(
    function() vmx_nca("dv_1", "observed", wait = FALSE, client = con),
    function() vmx_modeling_options("dv_1", "observed", client = con),
    function() vmx_model_build("dv_1", "observed", client = con),
    function() vmx_nca("dv_1", "observed", pk_specimen = NULL, wait = FALSE, client = con)
  )) {
    err <- expect_error(call(), class = "vmx_usage_error")
    expect_match(conditionMessage(err), "`pk_specimen` is required")
    expect_match(conditionMessage(err), '"plasma", "serum", or vmx_all_specimens()',
                 fixed = TRUE)
    expect_match(conditionMessage(err), 'recommends "plasma"; vmxr does not apply it')
  }
  # Only the marker-analysis lookups went out; nothing was created.
  methods <- vapply(env$reqs, function(r) r$method %||% "GET", character(1))
  expect_true(all(methods == "GET"))
  expect_true(all(grepl("/marker-analysis$", vapply(env$reqs, `[[`, "", "url"))))
})

test_that("a data version without marker analysis offers only All specimens", {
  ma <- list(data_version_id = "dv_1", recommended_pk_specimen = NA,
             marker_analysis = NA)
  route_mock(list("/marker-analysis$" = ma))
  expect_error(
    vmx_nca("dv_1", "observed", wait = FALSE, client = con),
    "only `pk_specimen = vmx_all_specimens\\(\\)` is available"
  )
})

test_that("a failed specimen lookup still gives a usable missing-specimen error", {
  route_mock(list("/marker-analysis$" = api_error(409L, "conflict", "data_version_output_unavailable")))
  expect_error(
    vmx_nca("dv_1", "observed", wait = FALSE, client = con),
    "vmx_all_specimens\\(\\)",
    class = "vmx_usage_error"
  )
})

test_that("a malformed PK specimen is rejected client-side", {
  for (bad in list(c("plasma", "serum"), NA_character_, "", 1, list(all_specimens = TRUE))) {
    expect_error(
      vmx_nca("dv_1", "observed", pk_specimen = bad, wait = FALSE, client = con),
      class = "vmx_usage_error"
    )
  }
})

# ---- PD markers ------------------------------------------------------------

test_that("modeling options send PD markers with one specimen and opt into marker scopes", {
  env <- route_mock(list("/modeling-options$" = options_echo))
  vmx_modeling_options(
    "dv_1", "observed", pk_specimen = "plasma",
    pd_marker = vmx_pd_marker(pd_uuid, "serum", "decreasing"),
    client = con
  )
  req <- env$reqs[[1]]
  expect_match(req$url, "/modeling-options\\?representation=marker_scopes$")
  # Direction is a build-time assumption; the preview's MarkerSelection omits it.
  expect_equal(
    req$body$data$pd_markers,
    list(list(gen_uuid = pd_uuid, specimen_type = "serum"))
  )
  expect_equal(req$body$data$pk_specimen, list(specimen_type = "plasma"))
})

test_that("modeling options send a PD marker across all specimens", {
  env <- route_mock(list("/modeling-options$" = options_echo))
  vmx_modeling_options(
    "dv_1", "observed", pk_specimen = vmx_all_specimens(),
    pd_marker = list(vmx_pd_marker(pd_uuid, vmx_all_specimens())),
    client = con
  )
  expect_equal(
    env$reqs[[1]]$body$data$pd_markers,
    list(list(gen_uuid = pd_uuid, all_specimens = TRUE))
  )
})

test_that("model build sends the contract's request example", {
  env <- route_mock(list("/model-build-runs$" = run_created()))
  vmx_model_build(
    "dv_1", "observed", pk_specimen = vmx_all_specimens(),
    pd_marker = vmx_pd_marker(pd_uuid, "serum", "decreasing"),
    covariate = c("age", "crcl"), client = con
  )
  req <- env$reqs[[1]]
  expect_match(req$url, "/model-build-runs\\?representation=marker_scopes$")
  json <- jsonlite::toJSON(req$body$data, auto_unbox = TRUE)
  expect_equal(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    list(
      data_version_id = "dv_1",
      time_basis = "observed",
      pk_specimen = list(all_specimens = TRUE),
      covariates = list("age", "crcl"),
      pd_markers = list(list(
        gen_uuid = pd_uuid, specimen_type = "serum", direction = "decreasing"
      ))
    )
  )
})

test_that("model build selects two specimens of one marker as separate inputs", {
  env <- route_mock(list("/model-build-runs$" = run_created()))
  vmx_model_build(
    "dv_1", "observed", pk_specimen = "plasma",
    pd_marker = list(
      vmx_pd_marker(pd_uuid, "serum", "decreasing"),
      vmx_pd_marker(pd_uuid, "plasma", "increasing"),
      vmx_pd_marker(pd_uuid_2, vmx_all_specimens(), "decreasing")
    ),
    client = con
  )
  expect_equal(
    env$reqs[[1]]$body$data$pd_markers,
    list(
      list(gen_uuid = pd_uuid, specimen_type = "serum", direction = "decreasing"),
      list(gen_uuid = pd_uuid, specimen_type = "plasma", direction = "increasing"),
      list(gen_uuid = pd_uuid_2, all_specimens = TRUE, direction = "decreasing")
    )
  )
})

test_that("PD marker selections are validated before any request", {
  route_mock(list())
  build <- function(pd) {
    vmx_model_build("dv_1", "observed", pk_specimen = "plasma", pd_marker = pd,
                    client = con)
  }
  # A specimen is always required; there is no default.
  expect_error(vmx_pd_marker(pd_uuid), "`specimen` is required", class = "vmx_usage_error")
  expect_error(vmx_pd_marker(pd_uuid, NULL), class = "vmx_usage_error")
  expect_error(vmx_pd_marker(pd_uuid, c("serum", "plasma")), class = "vmx_usage_error")
  expect_error(vmx_pd_marker(pd_uuid, "serum", "up"), class = "vmx_usage_error")
  # Bare UUIDs and the old shorthand cannot say which specimen.
  expect_error(build(pd_uuid), "vmx_pd_marker", class = "vmx_usage_error")
  expect_error(build(paste0(pd_uuid, ":decreasing")), class = "vmx_usage_error")
  expect_error(
    vmx_modeling_options("dv_1", "observed", pk_specimen = "plasma",
                         pd_marker = pd_uuid, client = con),
    class = "vmx_usage_error"
  )
  # Builds need a direction.
  expect_error(build(vmx_pd_marker(pd_uuid, "serum")), "needs a direction",
               class = "vmx_usage_error")
  # Duplicate selection.
  expect_error(
    build(list(vmx_pd_marker(pd_uuid, "serum", "decreasing"),
               vmx_pd_marker(pd_uuid, "serum", "increasing"))),
    "more than once", class = "vmx_usage_error"
  )
  # All specimens and a named specimen of the same marker overlap.
  expect_error(
    build(list(vmx_pd_marker(pd_uuid, vmx_all_specimens(), "decreasing"),
               vmx_pd_marker(pd_uuid, "serum", "decreasing"))),
    "both in all specimens", class = "vmx_usage_error"
  )
})

test_that("vmx_pd_marker and vmx_all_specimens print compactly", {
  expect_output(print(vmx_all_specimens()), "<vmx_all_specimens>")
  expect_output(
    print(vmx_pd_marker(pd_uuid, vmx_all_specimens(), "decreasing")),
    "all specimens, decreasing"
  )
})

# ---- API specimen errors ---------------------------------------------------

test_that("an invalid PK specimen error names the available specimens", {
  route_mock(list(
    "/nca-analyses$" = api_error(422L, "validation_error", "invalid_pk_specimen", "pk_specimen"),
    "/marker-analysis$" = marker_analysis()
  ))
  err <- expect_error(
    vmx_nca("dv_1", "observed", pk_specimen = "urine", wait = FALSE, client = con),
    class = "vmx_specimen_error"
  )
  expect_s3_class(err, "vmx_api_error")
  expect_equal(err$status, 422L)
  expect_equal(err$reason, "invalid_pk_specimen")
  expect_equal(err$field, "pk_specimen")
  expect_match(conditionMessage(err), '"plasma", "serum", or vmx_all_specimens()', fixed = TRUE)
})

test_that("PD, marker-analysis and representation rejections are readable", {
  cases <- list(
    list(api_error(422L, "validation_error", "invalid_pd_markers"), "Each PD marker"),
    list(api_error(409L, "conflict", "marker_analysis_unavailable"), "only vmx_all_specimens"),
    list(api_error(422L, "validation_error", "marker_scopes_representation_required"),
         "marker-scopes representation")
  )
  for (case in cases) {
    route_mock(list("/model-build-runs$" = case[[1]]))
    err <- expect_error(
      vmx_model_build("dv_1", "observed", pk_specimen = "plasma",
                      pd_marker = vmx_pd_marker(pd_uuid, "serum", "decreasing"),
                      client = con),
      class = "vmx_specimen_error"
    )
    expect_match(conditionMessage(err), case[[2]])
  }
})

test_that("unrelated API errors pass through unchanged", {
  route_mock(list("/modeling-options$" = api_error(422L, "validation_error", "time_basis_unavailable")))
  err <- expect_error(
    vmx_modeling_options("dv_1", "nominal", pk_specimen = "plasma", client = con),
    class = "vmx_api_error"
  )
  expect_false(inherits(err, "vmx_specimen_error"))
  expect_equal(err$reason, "time_basis_unavailable")
})

# ---- Reads opt into marker scopes -------------------------------------------

test_that("run and fit reads request the marker-scopes representation", {
  pooled <- list(gen_uuid = pd_uuid, all_specimens = TRUE, direction = "decreasing")
  status <- c(run_created(), list(input = list(
    time_basis = "observed", pd_markers = list(pooled), covariates = list()
  )))
  env <- route_mock(list(
    "/model-build-runs/run_1/status$" = status,
    "/model-build-runs/run_1/results$" = list(
      run_id = "run_1", status = "succeeded",
      model_fits = list(pd_final = list(list(
        model_fit_id = "mf_4", status = "succeeded", model_type = "pd",
        marker = list(gen_uuid = pd_uuid, name = "tumor_size", all_specimens = TRUE)
      )))
    ),
    "/model-build-runs/run_1/cancel$" = c(run_created(), list(status = "cancelled")),
    "/model-build-runs$" = list(
      items = list(run_created(list(pooled))), next_cursor = NA_character_, has_next_page = FALSE
    ),
    "/model-fits/mf_4/details$" = list(metadata = list(model_fit_id = "mf_4")),
    "/model-fits/mf_4/vpc$" = list(model_fit_id = "mf_4"),
    "/model-fits$" = list(
      items = list(list(
        model_fit_id = "mf_4", model_type = "pd", run_id = "run_1",
        data_version_id = "dv_1", status = "succeeded",
        marker = list(gen_uuid = pd_uuid, name = "tumor_size", all_specimens = TRUE)
      )),
      next_cursor = NA_character_, has_next_page = FALSE
    )
  ))
  run <- vmx_model_build_status("run_1", client = con)
  expect_equal(run$input$pd_markers[[1]], pooled)
  res <- vmx_model_build_results("run_1", client = con)
  expect_true(res$model_fits$pd_final[[1]]$marker$all_specimens)
  vmx_model_build_cancel("run_1", client = con)
  runs <- vmx_model_build_runs(data_version = "dv_1", client = con)
  expect_equal(runs$run_id, "run_1")
  fits <- vmx_model_fits(run = "run_1", client = con)
  expect_true(fits$marker_all_specimens)
  vmx_model_fit("mf_4", client = con)
  vmx_fit_vpc("mf_4", client = con)

  urls <- vapply(env$reqs, `[[`, "", "url")
  expect_length(urls, 7L)
  expect_true(all(grepl("representation=marker_scopes", urls)))
})

test_that("NCA reads do not send a representation", {
  env <- route_mock(list("/nca-analyses/nca_1$" = nca_created))
  vmx_nca_get("nca_1", client = con)
  expect_false(grepl("representation", env$reqs[[1]]$url))
})

# ---- Accepted-selection readers ---------------------------------------------

test_that("vmx_nca_marker_selection reads the accepted PK specimen", {
  env <- route_mock(list(
    "/nca-analyses/nca_1/marker-selection$" = list(pk_specimen = list(specimen_type = "plasma"))
  ))
  out <- vmx_nca_marker_selection("nca_1", client = con)
  expect_equal(out$pk_specimen, list(specimen_type = "plasma"))
  expect_false(grepl("representation", env$reqs[[1]]$url))
})

test_that("vmx_model_build_marker_selection reads PK and PD selections", {
  sel <- list(
    pk_specimen = list(all_specimens = TRUE),
    pd_markers = list(
      list(gen_uuid = pd_uuid, specimen_type = "serum", direction = "decreasing"),
      list(gen_uuid = pd_uuid, specimen_type = "plasma", direction = "decreasing"),
      list(gen_uuid = pd_uuid_2, all_specimens = TRUE, direction = "increasing"),
      # Saved before PD specimens were selectable: returned as-is.
      list(gen_uuid = "33333333-3333-4333-8333-333333333333", direction = "increasing")
    )
  )
  route_mock(list("/model-build-runs/run_1/marker-selection$" = sel))
  run <- new_vmx_resource(run_created(), "vmx_model_build_run", "run_id")
  expect_equal(vmx_model_build_marker_selection(run, client = con), sel)
})

test_that("vmx_model_fit_marker_selection handles PK, scoped and saved UUID-only fits", {
  for (pd in list(
    NULL,
    list(gen_uuid = pd_uuid, all_specimens = TRUE),
    list(gen_uuid = pd_uuid, specimen_type = "serum"),
    list(gen_uuid = pd_uuid)
  )) {
    # NA serialises as JSON null.
    body <- list(pk_specimen = list(specimen_type = "plasma"), pd_marker = pd %||% NA)
    route_mock(list("/model-fits/mf_1/marker-selection$" = body))
    out <- vmx_model_fit_marker_selection("mf_1", client = con)
    expect_equal(out$pk_specimen, list(specimen_type = "plasma"))
    expect_equal(out$pd_marker, pd)
  }
})

test_that("marker-selection readers reject malformed selections", {
  bad <- list(
    list(pk_specimen = list()),
    list(pk_specimen = list(all_specimens = FALSE)),
    list(pk_specimen = list(specimen_type = "")),
    list(pk_specimen = list(all_specimens = TRUE, specimen_type = "plasma"))
  )
  for (body in bad) {
    route_mock(list("/marker-selection$" = body))
    expect_error(vmx_nca_marker_selection("nca_1", client = con),
                 class = "vmx_response_error")
  }
  route_mock(list("/marker-selection$" = list(
    pk_specimen = list(all_specimens = TRUE),
    pd_markers = list(list(gen_uuid = pd_uuid, all_specimens = TRUE,
                           specimen_type = "serum", direction = "decreasing"))
  )))
  expect_error(vmx_model_build_marker_selection("run_1", client = con),
               class = "vmx_response_error")
  route_mock(list("/marker-selection$" = list(
    pk_specimen = list(all_specimens = TRUE),
    pd_markers = list(list(gen_uuid = pd_uuid, specimen_type = "serum", direction = "up"))
  )))
  expect_error(vmx_model_build_marker_selection("run_1", client = con),
               class = "vmx_response_error")
})
