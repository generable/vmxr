# Prep status & answers — the async formatting pipeline.

#' Prep status for a dataset
#'
#' Calls `GET /datasets/{ds_id}/prep-status`.
#'
#' @param dataset A dataset id (`ds_...`) or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A `vmx_prep_status` (`status`, and `data_version_id` once settled).
#' @export
vmx_prep_status <- function(dataset, client = vmx_client()) {
  id <- vmx_id(dataset, "ds", arg = "dataset")
  data <- vmx_get(client, paste0("/datasets/", id, "/prep-status"))
  vmx_validate_response_id(data, "dataset_id", id, "prep status")
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
}

#' Questions raised by prep (when awaiting input)
#' @param dataset A dataset id or `vmx_dataset`.
#' @param client A `vmx_client`.
#' @return A tibble of pending questions. Variable-shape values such as
#'   `options`, `default`, and `data_preview` are retained as list-columns.
#' @export
vmx_prep_questions <- function(dataset, client = vmx_client()) {
  ps <- if (inherits(dataset, "vmx_prep_status")) dataset else vmx_prep_status(dataset, client = client)
  prompt <- ps[["prompt"]]
  if (is.null(prompt)) {
    return(vmx_empty_prep_questions())
  }
  if (!is.list(prompt) || is.null(names(prompt)) ||
      anyDuplicated(names(prompt))) {
    vmx_abort_response(
      "field 'prep status.prompt' must be an object.",
      field = "prompt"
    )
  }
  fields <- vmx_response_field(
    prompt, "fields", "prep status.prompt.fields"
  )
  if (!is.list(fields) || !is.null(names(fields))) {
    vmx_abort_response(
      "field 'prep status.prompt.fields' must be an array.",
      field = "prompt.fields"
    )
  }
  if (!length(fields)) {
    return(vmx_empty_prep_questions())
  }
  rows <- lapply(seq_along(fields), function(i) {
    f <- fields[[i]]
    context <- sprintf("prep status.prompt.fields[%d]", i)
    if (!is.list(f) || is.null(names(f)) || anyDuplicated(names(f))) {
      vmx_abort_response(
        sprintf("%s must be an object.", context),
        field = "prompt.fields"
      )
    }
    field <- vmx_response_scalar(
      vmx_response_field(f, "field", paste0(context, ".field")),
      paste0(context, ".field"),
      type = "character",
      nonempty = TRUE
    )
    question <- vmx_response_scalar(
      vmx_response_field(f, "question", paste0(context, ".question")),
      paste0(context, ".question"),
      type = "character",
      nonempty = TRUE
    )
    required <- vmx_response_scalar(
      vmx_response_field(f, "required", paste0(context, ".required")),
      paste0(context, ".required"),
      type = "logical"
    )
    resolution <- f[["resolution"]]
    if (!is.null(resolution) &&
        (!is.list(resolution) || is.null(names(resolution)) ||
          anyDuplicated(names(resolution)))) {
      vmx_abort_response(
        sprintf("field '%s.resolution' must be an object or null.", context),
        field = "prompt.fields.resolution"
      )
    }
    tibble::tibble(
      field = field,
      question = question,
      required = required,
      format = vmx_optional_prompt_string(f, "format", context),
      options = list(f[["options"]]),
      referent = vmx_optional_prompt_string(f, "referent", context),
      rationale = vmx_optional_prompt_string(f, "rationale", context),
      data_preview = list(f[["data_preview"]]),
      resolution_kind = vmx_optional_prompt_string(
        resolution, "kind", paste0(context, ".resolution")
      ),
      resolution_hint = vmx_optional_prompt_string(
        resolution, "hint", paste0(context, ".resolution")
      ),
      default = list(f[["default"]]),
      group = vmx_optional_prompt_string(f, "group", context)
    )
  })
  out <- vctrs::vec_rbind(!!!rows)
  if (anyDuplicated(out$field)) {
    vmx_abort_response(
      "field 'prep status.prompt.fields' contains duplicate answer keys.",
      field = "prompt.fields.field"
    )
  }
  out
}

#' Answer prep questions and resume formatting
#' @param dataset A dataset id or `vmx_dataset`.
#' @param answers A named list mapping each prompt `field` to its answer value.
#' @param client A `vmx_client`.
#' @param idempotency_key Optional idempotency key for safely repeating the
#'   submission.
#' @export
vmx_prep_answer <- function(dataset, answers, client = vmx_client(),
                            idempotency_key = NULL) {
  if (!is.list(answers) || !length(answers) || is.null(names(answers)) ||
      any(!nzchar(names(answers))) || anyDuplicated(names(answers))) {
    vmx_abort(
      "`answers` must be a non-empty named list with unique field names.",
      class = "vmx_usage_error"
    )
  }
  if ("idempotency_key" %in% names(answers)) {
    vmx_abort(
      "`idempotency_key` is reserved; pass it through the argument of that name.",
      class = "vmx_usage_error"
    )
  }
  body <- answers
  if (!is.null(idempotency_key)) {
    vmx_id_like_scalar(idempotency_key, "idempotency_key")
    body$idempotency_key <- idempotency_key
  }
  id <- vmx_id(dataset, "ds", arg = "dataset")
  data <- vmx_post(
    client, paste0("/datasets/", id, "/prep-answers"), body
  )
  vmx_validate_response_id(data, "dataset_id", id, "prep answer")
  new_vmx_resource(data, "vmx_prep_status", "dataset_id")
}

vmx_optional_prompt_string <- function(x, name, context) {
  if (is.null(x) || !name %in% names(x) || is.null(x[[name]])) {
    return(NA_character_)
  }
  vmx_response_scalar(
    x[[name]],
    paste0(context, ".", name),
    type = "character",
    nonempty = TRUE
  )
}

vmx_empty_prep_questions <- function() {
  tibble::tibble(
    field = character(0),
    question = character(0),
    required = logical(0),
    format = character(0),
    options = list(),
    referent = character(0),
    rationale = character(0),
    data_preview = list(),
    resolution_kind = character(0),
    resolution_hint = character(0),
    default = list(),
    group = character(0)
  )
}
