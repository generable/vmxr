# Unit tests for the dataset + prep verbs, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")
capture_one <- function(body) {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = body)
  }, env = parent.frame())
  env
}

test_that("vmx_datasets lists with filters into a tibble", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$req <- req
    httr2::response_json(body = list(
      items = list(list(dataset_id = "ds_1", status = "formatted")), next_cursor = NULL
    ))
  })
  tbl <- vmx_datasets(study = "std_1", client = con)
  expect_equal(tbl$dataset_id, "ds_1")
  expect_match(env$req$url, "study_id=std_1")
})

test_that("vmx_dataset fetches and types the resource", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    dataset_id = "ds_1", status = "formatted", tags = list(name = "run-A")
  ))))
  ds <- vmx_dataset("ds_1", client = con)
  expect_s3_class(ds, "vmx_dataset")
  expect_equal(vmx_resource_id(ds), "ds_1")
})

test_that("vmx_dataset_files paginates into a tibble", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    items = list(list(tagged_upload_id = "tu_1", name = "conc.csv", size = 42L)),
    next_cursor = NULL
  ))))
  tbl <- vmx_dataset_files("ds_1", client = con)
  expect_equal(tbl$name, "conc.csv")
  expect_equal(tbl$size, 42L)
})

test_that("vmx_dataset_tags returns a key/value tibble from the object", {
  ds <- new_vmx_resource(list(dataset_id = "ds_1", tags = list(name = "run-A")),
                         "vmx_dataset", "dataset_id")
  tbl <- vmx_dataset_tags(ds)
  expect_equal(tbl$key, "name")
  expect_equal(tbl$value, "run-A")
})

test_that("vmx_dataset_tags handles no tags", {
  ds <- new_vmx_resource(list(dataset_id = "ds_1", tags = list()),
                         "vmx_dataset", "dataset_id")
  expect_equal(nrow(vmx_dataset_tags(ds)), 0L)
})

test_that("vmx_dataset_cancel posts and returns a prep-status", {
  env <- capture_one(list(dataset_id = "ds_1", status = "cancelled"))
  ps <- vmx_dataset_cancel("ds_1", client = con)
  expect_s3_class(ps, "vmx_prep_status")
  expect_match(env$req$url, "/datasets/ds_1/cancel$")
  expect_equal(env$req$method, "POST")
})

test_that("vmx_upload_ignore / unignore post upload_id", {
  env <- capture_one(list(dataset_id = "ds_1", status = "formatting"))
  vmx_upload_ignore("ds_1", "upl_9", client = con)
  expect_match(env$req$url, "/datasets/ds_1/ignore-upload$")
  expect_equal(env$req$body$data$upload_id, "upl_9")

  env2 <- capture_one(list(dataset_id = "ds_1", status = "formatting"))
  vmx_upload_unignore("ds_1", "upl_9", client = con)
  expect_match(env2$req$url, "/datasets/ds_1/unignore-upload$")
})

test_that("vmx_prep_questions builds a tibble from the prompt", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    dataset_id = "ds_1", status = "awaiting_input",
    prompt = list(message = "Need info", fields = list(
      list(field = "dose_unit", question = "Units?", required = TRUE,
           options = list("mg", "ug"), format = "enum")
    ))
  ))))
  q <- vmx_prep_questions("ds_1", client = con)
  expect_equal(q$field, "dose_unit")
  expect_true(q$required)
  expect_equal(q$options[[1]], list("mg", "ug"))
})

test_that("vmx_prep_questions is empty when no prompt", {
  httr2::local_mocked_responses(list(httr2::response_json(body = list(
    dataset_id = "ds_1", status = "formatting"
  ))))
  expect_equal(nrow(vmx_prep_questions("ds_1", client = con)), 0L)
})

test_that("vmx_prep_answer posts the answers body", {
  env <- capture_one(list(dataset_id = "ds_1", status = "formatting"))
  ps <- vmx_prep_answer("ds_1", list(dose_unit = "mg"), client = con)
  expect_s3_class(ps, "vmx_prep_status")
  expect_equal(env$req$body$data$dose_unit, "mg")
  expect_match(env$req$url, "/datasets/ds_1/prep-answers$")
})

test_that("vmx_prep_answer rejects a non-named answers arg", {
  expect_error(vmx_prep_answer("ds_1", list(1, 2), client = con), class = "vmx_usage_error")
})
