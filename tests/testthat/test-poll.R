test_that("polling fails immediately on an unrequested terminal failure", {
  fetches <- 0L
  fetch <- function(id) {
    fetches <<- fetches + 1L
    list(
      status = "failed",
      failure_reason = "The worker exceeded its execution time limit."
    )
  }

  err <- tryCatch(
    vmx_poll_status(
      "job_1", fetch,
      success = "succeeded",
      failed = "failed",
      known = c("queued", "succeeded", "failed"),
      until = "succeeded",
      timeout = 1,
      interval = 0.001,
      progress = FALSE,
      label = "Job"
    ),
    vmx_job_error = function(e) e
  )

  expect_s3_class(err, "vmx_job_error")
  expect_equal(err$resource_status, "failed")
  expect_match(conditionMessage(err), "execution time limit")
  expect_equal(fetches, 1L)
})

test_that("polling does not wait forever after an unrequested success", {
  fetches <- 0L
  fetch <- function(id) {
    fetches <<- fetches + 1L
    list(status = "succeeded")
  }

  expect_error(
    vmx_poll_status(
      "job_1", fetch,
      success = "succeeded",
      failed = "failed",
      known = c("queued", "succeeded", "failed"),
      until = "failed",
      timeout = 1,
      interval = 0.001,
      progress = FALSE,
      label = "Job"
    ),
    "before the requested status",
    class = "vmx_job_error"
  )
  expect_equal(fetches, 1L)
})

test_that("polling validates controls and closed status vocabularies", {
  fetch <- function(id) list(status = "new_server_state")
  args <- list(
    id = "job_1",
    fetch = fetch,
    success = "succeeded",
    failed = "failed",
    known = c("queued", "succeeded", "failed"),
    timeout = 1,
    interval = 0.001,
    progress = FALSE,
    label = "Job"
  )

  expect_error(
    do.call(vmx_poll_status, c(args, list(until = "not-a-status"))),
    class = "vmx_usage_error"
  )
  expect_error(
    do.call(
      vmx_poll_status,
      utils::modifyList(args, list(until = NULL, timeout = Inf))
    ),
    class = "vmx_usage_error"
  )
  expect_error(
    do.call(vmx_poll_status, c(args, list(until = NULL))),
    class = "vmx_response_error"
  )
})
