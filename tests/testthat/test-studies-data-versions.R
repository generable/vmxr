# Unit tests for the studies + data-versions verbs, using httr2 response mocks.
# A capturing mock lets us assert both the request (method/url/body) and the
# parsed return shape.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

# Records the last request, replies with `body` (or a queue of bodies).
capturing_mock <- function(bodies) {
  if (!is.null(names(bodies)) || is.list(bodies) && !is.null(bodies$items)) {
    bodies <- list(bodies)
  }
  i <- 0
  captured <- new.env()
  mock <- function(req) {
    i <<- i + 1
    captured$req <- req
    httr2::response_json(body = bodies[[min(i, length(bodies))]])
  }
  list(mock = mock, captured = captured)
}

study_item <- function(id, name, tmt = "tmt_1") {
  list(study_id = id, treatment_id = tmt, name = name, status = "active",
       counts = list(data_versions = 0L), last_activity_at = "2026-01-01T00:00:00Z",
       created_at = "2026-01-01T00:00:00Z")
}
dv_item <- function(id, study = "std_1") {
  list(data_version_id = id, study_id = study, treatment_id = "tmt_1",
       source_dataset_id = "ds_1", status = "ready", eligible_for_modeling = TRUE)
}

test_that("vmx_studies combines filtered pages automatically", {
  cm <- capturing_mock(list(
    list(
      items = list(study_item("std_1", "A")),
      next_cursor = "c1",
      has_next_page = TRUE
    ),
    list(
      items = list(study_item("std_2", "B")),
      next_cursor = NA_character_,
      has_next_page = FALSE
    )
  ))
  httr2::local_mocked_responses(cm$mock)
  out <- vmx_studies("tmt_1", client = con)
  expect_equal(out$study_id, c("std_1", "std_2"))
  # treatment_id forwarded as a query param
  expect_match(cm$captured$req$url, "treatment_id=tmt_1")
  expect_match(cm$captured$req$url, "cursor=c1")
})

test_that("vmx_studies forwards a validated created_since filter", {
  cm <- capturing_mock(list(
    items = list(),
    next_cursor = NA_character_,
    has_next_page = FALSE
  ))
  httr2::local_mocked_responses(cm$mock)

  vmx_studies(
    created_since = as.POSIXct("2026-01-02 03:04:05", tz = "UTC"),
    client = con
  )

  expect_match(
    utils::URLdecode(cm$captured$req$url),
    "created_since=2026-01-02T03:04:05Z",
    fixed = TRUE
  )
})

test_that("collection flattening keeps nested arrays in one row", {
  markers <- list(
    list(name = "effect", type = "continuous"),
    list(name = "response", type = "count")
  )
  item <- study_item("std_1", "A")
  item$pd_markers <- markers
  cm <- capturing_mock(list(
    items = list(item),
    next_cursor = NA_character_,
    has_next_page = FALSE
  ))
  httr2::local_mocked_responses(cm$mock)

  tbl <- vmx_studies(client = con)

  expect_equal(nrow(tbl), 1L)
  expect_type(tbl$pd_markers, "list")
  expect_equal(tbl$pd_markers[[1]], markers)
})

test_that("vmx_studies rejects a non-treatment id", {
  expect_error(vmx_studies("std_1", client = con), class = "vmx_usage_error")
})

test_that("vmx_study_create posts the resolved treatment id + defaults", {
  cm <- capturing_mock(study_item("std_9", "New"))
  httr2::local_mocked_responses(cm$mock)
  s <- vmx_study_create("tmt_1", "New", phase = "1", client = con)
  expect_s3_class(s, "vmx_study")
  body <- cm$captured$req$body$data
  expect_equal(body$treatment_id, "tmt_1")
  expect_equal(body$name, "New")
  expect_equal(body$study_type, "clinical")
  expect_equal(body$phase, "1")
})

test_that("vmx_study_create accepts a vmx_treatment object", {
  tmt <- new_vmx_resource(list(treatment_id = "tmt_7"), "vmx_treatment", "treatment_id")
  cm <- capturing_mock(study_item("std_7", "X", tmt = "tmt_7"))
  httr2::local_mocked_responses(cm$mock)
  vmx_study_create(tmt, "X", client = con)
  expect_equal(cm$captured$req$body$data$treatment_id, "tmt_7")
})

test_that("vmx_study_update preserves an explicitly supplied JSON null", {
  cm <- capturing_mock(study_item("std_1", "A"))
  httr2::local_mocked_responses(cm$mock)

  vmx_study_update(
    "std_1",
    route_of_administration = NULL,
    client = con
  )

  body <- cm$captured$req$body$data
  expect_true("route_of_administration" %in% names(body))
  expect_null(body$route_of_administration)

  expect_error(
    vmx_study_update("std_1", pd_markers = NULL, client = con),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_study_update("std_1", status = NULL, client = con),
    class = "vmx_usage_error"
  )
})

test_that("vmx_data_versions forwards filters as query params", {
  cm <- capturing_mock(list(
    items = list(dv_item("dv_1")),
    next_cursor = NA_character_,
    has_next_page = FALSE
  ))
  httr2::local_mocked_responses(cm$mock)
  tbl <- vmx_data_versions(study = "std_1", eligible_for_modeling = TRUE, client = con)
  expect_equal(nrow(tbl), 1L)
  url <- cm$captured$req$url
  expect_match(url, "study_id=std_1")
  expect_match(url, "eligible_for_modeling=true")   # logical lower-cased
  expect_match(url, "include_archived=false")
})

test_that("vmx_data_versions accepts a vmx_study object", {
  cm <- capturing_mock(list(
    items = list(dv_item("dv_1", study = "std_7")),
    next_cursor = NA_character_,
    has_next_page = FALSE
  ))
  httr2::local_mocked_responses(cm$mock)
  study <- new_vmx_resource(list(study_id = "std_7"), "vmx_study", "study_id")
  vmx_data_versions(study = study, client = con)
  expect_match(cm$captured$req$url, "study_id=std_7")
})

test_that("vmx_data_version fetches and types the resource", {
  cm <- capturing_mock(dv_item("dv_42"))
  httr2::local_mocked_responses(cm$mock)
  dv <- vmx_data_version("dv_42", client = con)
  expect_s3_class(dv, "vmx_data_version")
  expect_equal(vmx_resource_id(dv), "dv_42")
  expect_match(cm$captured$req$url, "/data-versions/dv_42$")
})

test_that("vmx_data_version_create posts upload_ids and returns a prep-status", {
  cm <- capturing_mock(list(dataset_id = "ds_1", status = "formatting"))
  httr2::local_mocked_responses(cm$mock)
  ps <- vmx_data_version_create("ds_1", uploads = c("upl_a", "upl_b"), client = con)
  expect_s3_class(ps, "vmx_prep_status")
  expect_equal(cm$captured$req$body$data$upload_ids, list("upl_a", "upl_b"))
  expect_match(cm$captured$req$url, "/datasets/ds_1/data-versions$")
})

test_that("vmx_data_version_create validates its upload composition", {
  expect_error(
    vmx_data_version_create("ds_1", uploads = character(), client = con),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_data_version_create("ds_1", uploads = "ds_wrong", client = con),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_data_version_create(
      "ds_1", uploads = c("upl_a", "upl_a"), client = con
    ),
    class = "vmx_usage_error"
  )
})

test_that("vmx_data_version_export requires the canonical matching envelope", {
  cm <- capturing_mock(list(
    data_version_id = "dv_1",
    download_url = "https://storage.test/signed",
    expires_at = "2026-01-01T01:00:00Z",
    byte_size = 10,
    files = list()
  ))
  httr2::local_mocked_responses(cm$mock)
  out <- vmx_data_version_export("dv_1", client = con)
  expect_equal(out$download_url, "https://storage.test/signed")

  cm2 <- capturing_mock(list(
    data_version_id = "dv_other",
    download_url = "https://storage.test/signed"
  ))
  httr2::local_mocked_responses(cm2$mock)
  expect_error(
    vmx_data_version_export("dv_1", client = con),
    class = "vmx_response_error"
  )
})

test_that("archive/unarchive PATCH the right body", {
  cm <- capturing_mock(dv_item("dv_1"))
  httr2::local_mocked_responses(cm$mock)
  vmx_data_version_archive("dv_1", reason = "superseded", client = con)
  expect_equal(cm$captured$req$method, "PATCH")
  expect_true(cm$captured$req$body$data$archived)
  expect_equal(cm$captured$req$body$data$reason, "superseded")

  cm2 <- capturing_mock(dv_item("dv_1"))
  httr2::local_mocked_responses(cm2$mock)
  vmx_data_version_unarchive("dv_1", client = con)
  expect_false(cm2$captured$req$body$data$archived)
})
