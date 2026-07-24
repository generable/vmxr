# Unit tests for the HTTP layer and Phase-0 verbs, using httr2 response mocks
# (no live calls).

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

tmt_item <- function(id, name, status = "active") {
  list(
    treatment_id = id, name = name, status = status,
    counts = list(studies = 1L, data_versions = 0L),
    created_at = "2026-01-01T00:00:00Z"
  )
}

test_that("vmx_whoami parses /me into a typed object", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      user_id = "usr_1", email = "a@b.co", name = "Ada",
      workspace_id = "ws_1", roles = list("admin"),
      counts = list(treatments = 2L, data_versions = 0L, model_fits = 0L)
    ))
  ))
  me <- vmx_whoami(con)
  expect_s3_class(me, "vmx_me")
  expect_equal(me$email, "a@b.co")
  expect_equal(vmx_resource_id(me), "usr_1")
})

test_that("vmx_treatments returns one page and accepts its opaque cursor", {
  urls <- character()
  i <- 0L
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    i <<- i + 1L
    if (i == 1L) {
      httr2::response_json(body = list(
        items = list(tmt_item("tmt_1", "A")),
        next_cursor = "c1",
        has_next_page = TRUE
      ))
    } else {
      httr2::response_json(body = list(
        items = list(tmt_item("tmt_2", "B")),
        next_cursor = NA_character_,
        has_next_page = FALSE
      ))
    }
  })

  first <- vmx_treatments(client = con)
  expect_s3_class(first, "tbl_df")
  expect_equal(first$treatment_id, "tmt_1")
  expect_equal(vmx_next_cursor(first), "c1")
  expect_true(vmx_has_next_page(first))
  # nested counts flattened to prefixed columns
  expect_true(all(c("counts_studies", "counts_data_versions") %in% names(first)))

  second <- vmx_treatments(cursor = vmx_next_cursor(first), client = con)
  expect_equal(second$treatment_id, "tmt_2")
  expect_null(vmx_next_cursor(second))
  expect_false(vmx_has_next_page(second))
  expect_match(urls[[2]], "cursor=c1")
})

test_that("vmx_treatments returns an empty tibble when there are none", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      items = list(),
      next_cursor = NA_character_,
      has_next_page = FALSE
    ))
  ))
  expect_equal(nrow(vmx_treatments(client = con)), 0L)
})

test_that("treatment updates preserve only contract-nullable NULL fields", {
  captured <- new.env()
  httr2::local_mocked_responses(function(req) {
    captured$req <- req
    httr2::response_json(body = tmt_item("tmt_1", "A"))
  })

  vmx_treatment_update("tmt_1", description = NULL, client = con)
  expect_true("description" %in% names(captured$req$body$data))
  expect_null(captured$req$body$data$description)

  expect_error(
    vmx_treatment_update("tmt_1", name = NULL, client = con),
    class = "vmx_usage_error"
  )
  expect_error(
    vmx_treatment_update("tmt_1", unknown_field = "x", client = con),
    class = "vmx_usage_error"
  )
})

test_that("401 maps to vmx_auth_error", {
  httr2::local_mocked_responses(list(
    httr2::response_json(
      status_code = 401,
      body = list(error = list(reason = "unauthenticated", message = "bad token"))
    )
  ))
  expect_error(vmx_whoami(con), class = "vmx_auth_error")
})

test_that("404 envelope maps to vmx_api_error carrying the reason", {
  httr2::local_mocked_responses(list(
    httr2::response_json(
      status_code = 404,
      body = list(error = list(code = "not_found", reason = "not_found", message = "no such treatment"))
    )
  ))
  err <- tryCatch(vmx_treatment("tmt_missing", con), vmx_api_error = function(e) e)
  expect_s3_class(err, "vmx_api_error")
  expect_equal(err$status, 404)
  expect_equal(err$reason, "not_found")
})

test_that("bad id prefix fails fast client-side", {
  expect_error(vmx_treatment("nope_1", con), class = "vmx_usage_error")
})

test_that("vmx_wait polls prep-status to a terminal state", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(dataset_id = "ds_1", status = "formatting")),
    httr2::response_json(body = list(dataset_id = "ds_1", status = "formatted",
                                     data_version_id = "dv_1"))
  ))
  ps <- vmx_wait(structure(list(dataset_id = "ds_1"),
                           vmx_id_field = "dataset_id",
                           class = c("vmx_dataset", "vmx_resource")),
                 interval = 0.001, progress = FALSE, client = con)
  expect_s3_class(ps, "vmx_prep_status")
  expect_equal(ps$status, "formatted")
  expect_equal(ps$data_version_id, "dv_1")
})

test_that("vmx_wait raises on a failed terminal state", {
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(dataset_id = "ds_1", status = "failed"))
  ))
  expect_error(
    vmx_wait(structure(list(dataset_id = "ds_1"),
                       vmx_id_field = "dataset_id",
                       class = c("vmx_dataset", "vmx_resource")),
             interval = 0.001, progress = FALSE, client = con),
    class = "vmx_api_error"
  )
})
