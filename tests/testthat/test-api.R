test_that("ocp can be initialised with multiple dimensions", {
  empty_ocpd <- initOCPD(dims=5)
  expect_equal(length(empty_ocpd$update_params0), 5)
})

test_that("ocp can be initialised with a vector of parameters", {
  empty_ocpd <- initOCPD(dims=5, init_params = replicate(5, list(list(m=0, k=0.01, a=0.01, b=0.0001))))
  expect_equal(length(empty_ocpd$update_params0), 5)
})

test_that("ocp can be initialised with a single set of parameters", {
  empty_ocpd <- initOCPD(dims=5, init_params = list(list(m=0, k=0.01, a=0.01, b=0.0001)))
  expect_equal(length(empty_ocpd$update_params0), 5)
})

test_that("ocp can't be initialised with a vector of distributions without parameters", {
  expect_error(
    empty_ocpd <- initOCPD(dims=5, init_prob = replicate(5, poisson_init))
  )
})

test_that("ocp can't be initialised with a scalar distributions without parameters", {
  expect_error(
    empty_ocpd <- initOCPD(dims=5, init_prob = poisson_init)
  )
})

test_that("ocp can be initialised with a vector of distributions", {
  empty_ocpd <- initOCPD(dims=5, init_prob = replicate(5, poisson_init), init_params = list(list(a=1, b=1)))
  expect_equal(length(empty_ocpd$update_params0), 5)
})

test_that("ocp can be initialised with a scalar distributions", {
  empty_ocpd <- initOCPD(dims=5, init_prob = poisson_init, list(list(a=1, b=1)))
  expect_equal(length(empty_ocpd$update_params0), 5)
})

test_that("ocp can be initialised and then updated", {
  expect_error({
    empty_ocpd <- initOCPD(dims=5)
    onlineCPD(
      matrix(rnorm(50), ncol=5),
      multivariate = T,
      oCPD = empty_ocpd
    )
  }, NA)
})
