#' Join data frames by key
#'
#' @description
#'
#' `join()` is a flexible data frame joiner that accepts any number of
#' data frames as input. It joins the data frame together matched on a
#' key and supports the following features:
#'
#' * If the input data frame is named, it is namespaced in the output
#'   data frame under that name. It becomes a data frame column.
#'
#' * If the input data frame has been boxed with `nesting()`, its
#'   components are integrated in the output data frame as
#'   list-columns.
#'
#'   A namespaced and nested data frame becomes a single list-column
#'   of data frames. A non-namespaced and nested data frame gets all
#'   of its columns binded as lists.
#'
#' @param .by A key to match on. Currently has to be a scalar key.
#' @param ... Data frames to join. They should all have a column named
#'   after `.by`. If you supply the data frame with a name, it is
#'   namespaced as a df-col in the output data frame.
#' @param .keep Whether to keep the `.by` column in namespaced data
#'   frames.
#' @param .unmatched Whether to drop rows with unmatched keys or throw
#'   an error.
#'
#' @section Unmatched and duplicate keys:
#'
#' * The `.unmatched` argument allows you to control how to process
#'   rows with unmatched keys. You can either drop them or throw an
#'   error. Dropping unmatched rows correspond to a fully inner join.
#'   It should also be possible to keep the rows (fully outer join),
#'   but this is unimplemented yet.
#'
#' * Rows with duplicate keys are always an error. Ideally there would
#'   be a `.duplicates` argument to let you take the cartesian product
#'   of those rows instead.
#'
#' @seealso nesting
#' @noRd
join <- function(.by,
                 ...,
                 .keep = FALSE,
                 .unmatched = c("drop", "error")) {
  dfs <- list2(...)
  check_join_inputs(.by, dfs)

  to_nest <- map_lgl(dfs, is_box, "join_nesting_box")
  dfs[to_nest] <- map(dfs[to_nest], unbox)

  dfs <- map(dfs, as_tibble)
  to_splice <- names2(dfs) == ""

  strict <- match.arg(.unmatched) == "error"
  inds <- join_indices(.by, dfs, strict)

  by_col <- dfs[[1]][[.by]][inds[[1]]] %||% .by[int()]
  dfs <- map2(dfs, to_splice, unselect_by, .by, .keep)

  dfs[to_nest] <- pmap(
    list(dfs[to_nest], inds[to_nest], to_splice[to_nest]),
    subset_nested,
    keep = .keep,
    by = .by
  )

  dfs[!to_nest] <- pmap(
    list(dfs[!to_nest], inds[!to_nest], to_splice[!to_nest]),
    subset_unnested,
    keep = .keep,
    by = .by
  )

  dfs <- flatten_if(dfs, is_spliced)
  tibble(!!.by := by_col, !!!dfs)
}

#' Signal a data frame should be nested
#' @param df A data frame to nest.
#' @noRd
nesting <- function(df) {
  new_box(df, "join_nesting_box")
}
is_nesting_box <- function(x) {
  is_box(x, "join_nesting_box")
}

join_indices <- function(by, dfs, strict = FALSE) {
  keys <- map(dfs, `[[`, by)
  matched <- matched_keys(keys, strict)

  # Pair each key with a row index
  keys <- map(keys, tibble::enframe, name = "index")

  # Drop unmatched
  keys <- map2(keys, matched, function(key, idx) key[idx, ])

  # Take first key vector ordering
  model <- keys[[1]]$value

  # Match keys to model and reorder the key index
  map(keys, function(key) {
    key <- key[match(model, key$value), ]
    key$index
  })
}

matched_keys <- function(keys, strict) {
  matched <- new_list(length(keys))

  for (i in seq_along(keys)) {
    key <- keys[[i]]
    keep <- reduce(keys[-i], .init = TRUE, function(keep, other) {
      keep & key %in% other
    })

    if (strict && !all(keep)) {
      abort("Join keys can't be unmatched")
    }

    matched[[i]] <- which(keep)
  }

  matched
}

unselect_by <- function(df, splice, by, keep) {
  if (!keep || splice) {
    df <- df[-match(by, names(df))]
  }
  df
}

subset_nested <- function(df, idx, splice, keep, by) {
  n <- length(idx)
  ptype <- df[int(), ]

  list_col <- rep_len(list(ptype), n)
  for (i in seq_len(n)) {
    list_col[[i]] <- df[idx[[i]], ]
  }

  if (splice) {
    splice(transpose(list_col))
  } else {
    list_col
  }
}
subset_unnested <- function(df, idx, splice, keep, by) {
  df <- df[idx, ]

  if (splice) {
    splice(df)
  } else {
    df
  }
}

check_join_inputs <- function(by, dfs) {
  stopifnot(
    # Multiple keys are unimplemented
    is_string(by),

    is_list(dfs) && length(dfs) >= 1L,

    every(dfs, is_join_df, by = by)
  )

  spliced <- dfs[names(dfs) == ""]
  spliced <- map_if(spliced, is_nesting_box, unbox)

  spliced <- map(spliced, function(df) df[-match(by, names(df))])
  all_nms <- unlist(map(spliced, names))
  if (anyDuplicated(all_nms)) {
    abort("Can't join tibbles with homonym columns")
  }
}

is_join_df <- function(df, by) {
  if (is_nesting_box(df)) {
    df <- unbox(df)
  }

  if (!is.data.frame(df)) {
    abort("Can't join with objects that are not data frames")
  }

  if (!has_name(df, by)) {
    abort("Can't join data frame that has no key column")
  }

  if (anyDuplicated(df[[by]])) {
    abort("Join keys can't be duplicated")
  }

  TRUE
}

full_join <- function(x, y, by) {
  merge_(x, y, by, all.x = TRUE, all.y = TRUE)
}
left_join <- function(x, y, by) {
  merge_(x, y, by, all.x = TRUE, all.y = FALSE)
}
merge_ <- function(x, y, by, ...) {
  out <- merge(x, y, by, ...)

  # Sort columns with original order
  nms <- unique(c(names(x), names(y)))
  out <- out[nms]

  as_tibble(out)
}
