# Note: these tests deliberately avoid the paths that create a conda env or hit
# the network (that would download hundreds of MB and mutate the machine). They
# exercise the pure logic and the argument-validation guards that run *before*
# any environment is built.

test_that("elsar_gee_env() is a stable, non-process-specific name", {
  expect_identical(elsar_gee_env(), "elsar_ee")
  # must not embed the PID (the old temp-env bug we're fixing)
  expect_false(grepl(Sys.getpid(), elsar_gee_env(), fixed = TRUE))
})

test_that("find_conda_exe() handles missing/invalid bases", {
  expect_null(find_conda_exe(NULL))
  expect_null(find_conda_exe(file.path(tempdir(), "no-such-conda-base")))
})

test_that("find_conda_exe() finds an executable in a real conda base", {
  base <- find_conda_base()
  skip_if(is.null(base), "no conda installation available")
  exe <- find_conda_exe(base)
  expect_true(is.character(exe) && file.exists(exe))
})

test_that("resolve_conda(install = FALSE) errors cleanly when nothing is found", {
  # Force the no-conda branch by stubbing find_conda_base to return NULL.
  testthat::local_mocked_bindings(find_conda_base = function() NULL)
  expect_error(resolve_conda(install = FALSE), "No conda installation found")
})

test_that("resolve_conda() returns the base when conda is present", {
  base <- find_conda_base()
  skip_if(is.null(base), "no conda installation available")
  expect_identical(resolve_conda(install = FALSE), base)
})

test_that("elsar_setup_gee() validates its arguments before doing any work", {
  # Bad types must error at the assertion stage, before conda/env creation.
  expect_error(elsar_setup_gee(python_version = 3.12))
  expect_error(elsar_setup_gee(install_conda = "yes"))
  expect_error(elsar_setup_gee(authenticate = NA))
  expect_error(elsar_setup_gee(gee_project = 123))
})

test_that("elsar_setup_gee() is exported", {
  expect_true("elsar_setup_gee" %in% getNamespaceExports("elsar"))
})
