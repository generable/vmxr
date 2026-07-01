# Unit tests for the modeling-data access verbs, using httr2 mocks.

con <- vmx_client(base_url = "https://vmx.test", token = "pat_test")

pk_table <- function() {
  list(
    data_version_id = "dv_1", domain = "pk",
    columns = list(
      list(name = "gen_subject_uuid", type = "string"),
      list(name = "time", type = "number", unit = "h"),
      list(name = "dv", type = "number", unit = "ng/mL"),
      list(name = "evid", type = "integer"),
      list(name = "blq", type = "boolean")
    ),
    rows = list(
      list(gen_subject_uuid = "u1", time = 0, dv = NULL, evid = 1, blq = FALSE),
      list(gen_subject_uuid = "u1", time = 1.5, dv = 12.3, evid = 0, blq = FALSE)
    )
  )
}

test_that("vmx_data_version_table coerces columns by declared type", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) { env$req <- req; httr2::response_json(body = pk_table()) })
  tbl <- vmx_data_version_table("dv_1", "pk", client = con)
  expect_match(env$req$url, "/data-versions/dv_1/tables/pk$")
  expect_equal(nrow(tbl), 2L)
  expect_type(tbl$time, "double")
  expect_type(tbl$evid, "double")     # integer -> numeric
  expect_type(tbl$blq, "logical")
  expect_equal(tbl$dv, c(NA, 12.3))   # null cell -> NA
  expect_equal(tbl$gen_subject_uuid, c("u1", "u1"))
  expect_false(is.null(attr(tbl, "columns")))
})

test_that("vmx_data_version_table validates the domain", {
  expect_error(vmx_data_version_table("dv_1", "bogus", client = con))
})

test_that("vmx_pk / vmx_subjects / vmx_pd hit the right domain", {
  for (d in c("pk", "subjects", "pd")) {
    env <- new.env()
    httr2::local_mocked_responses(function(req) {
      env$req <- req
      httr2::response_json(body = list(data_version_id = "dv_1", domain = d,
                                       columns = list(list(name = "gen_subject_uuid", type = "string")),
                                       rows = list(list(gen_subject_uuid = "u1"))))
    })
    fn <- switch(d, pk = vmx_pk, subjects = vmx_subjects, pd = vmx_pd)
    tbl <- fn("dv_1", client = con)
    expect_match(env$req$url, paste0("/tables/", d, "$"))
    expect_equal(nrow(tbl), 1L)
  }
})

test_that("vmx_model_data bundles available tables + meta and skips absent ones", {
  dv <- new_vmx_resource(list(
    data_version_id = "dv_1", n_subjects = 8L,
    units = list(time = "h"), time_bases = list(),
    table_availability = list(subjects = TRUE, pk = TRUE, pd = FALSE)
  ), "vmx_data_version", "data_version_id")

  i <- 0
  httr2::local_mocked_responses(function(req) {
    i <<- i + 1
    dom <- if (grepl("subjects", req$url)) "subjects" else "pk"
    httr2::response_json(body = list(data_version_id = "dv_1", domain = dom,
                                     columns = list(list(name = "gen_subject_uuid", type = "string")),
                                     rows = list(list(gen_subject_uuid = "u1"))))
  })
  md <- vmx_model_data(dv, client = con)
  expect_s3_class(md, "vmx_model_data")
  expect_s3_class(md$subjects, "tbl_df")
  expect_s3_class(md$pk, "tbl_df")
  expect_null(md$pd)                      # pd not available -> not fetched
  expect_equal(md$meta$n_subjects, 8L)
  expect_equal(i, 2L)                     # only subjects + pk fetched
})

test_that("nlmixr2 / torsten adapters remain deferred stubs", {
  expect_error(vmx_nlmixr_data("dv_1", client = con), class = "vmx_unimplemented_error")
  expect_error(vmx_torsten_data("dv_1", client = con), class = "vmx_unimplemented_error")
})
