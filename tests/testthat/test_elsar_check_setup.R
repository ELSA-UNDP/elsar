test_that("elsar_check_setup() returns a well-formed data frame", {
  res <- elsar_check_setup(quiet = TRUE)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("tier", "item", "status", "detail", "fix"))
  expect_gt(nrow(res), 0)
  expect_true(all(res$status %in% c("ok", "missing", "warn", "info")))
  # row names should be clean (no leaked Sys.which() names)
  expect_equal(rownames(res), as.character(seq_len(nrow(res))))
})

test_that("the core tier always reports R, sf, terra and elsar", {
  res <- elsar_check_setup(tiers = "core", quiet = TRUE)

  expect_true(all(res$tier == "Core"))
  expect_true(all(c("R", "sf", "terra", "elsar") %in% res$item))
  # In a checkable environment sf/terra are installed, so core is ready.
  expect_true(all(res$status == "ok"))
})

test_that("tiers argument selects which tiers are checked", {
  core_only <- elsar_check_setup(tiers = "core", quiet = TRUE)
  expect_setequal(unique(core_only$tier), "Core")

  two <- elsar_check_setup(tiers = c("core", "earth_engine"), quiet = TRUE)
  expect_setequal(unique(two$tier), c("Core", "Earth Engine"))
})

test_that("an unknown tier is rejected", {
  expect_error(elsar_check_setup(tiers = "nonsense"))
})

test_that("quiet = TRUE prints nothing, quiet = FALSE prints a report", {
  expect_silent(elsar_check_setup(tiers = "core", quiet = TRUE))
  expect_output(elsar_check_setup(tiers = "core", quiet = FALSE),
                "elsar setup check")
})

test_that("every non-ok finding carries a fix hint", {
  res <- elsar_check_setup(quiet = TRUE)
  bad <- res[res$status %in% c("missing", "warn"), ]
  expect_true(all(nzchar(bad$fix)))
})
