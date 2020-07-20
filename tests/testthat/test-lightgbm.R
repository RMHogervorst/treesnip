test_that("lightgbm", {

  model <- parsnip::boost_tree(mtry = 1, trees = 50)
  model <- parsnip::set_engine(model, "lightgbm", verbosity = -1)

  expect_regression_works(parsnip::set_mode(model, "regression"))
  expect_multiclass_classification_works(parsnip::set_mode(model, "classification"))
  expect_binary_classification_works(parsnip::set_mode(model, "classification"))

})


test_that("lightgbm with tune", {

  model <- parsnip::boost_tree(mtry = 5, trees = tune())
  model <- parsnip::set_engine(model, "lightgbm")
  model <- parsnip::set_mode(model, "regression")

  expect_can_tune_boost_tree(model)

})

test_that("factors and character become categorical features",{
  dataset <- dataset2 <- data.frame(
    num1 = 1:10,
    num2 = 10:1,
    cat = letters[1:10],
    stringsAsFactors = FALSE
  )
  dataset2$cat <- as.factor(dataset$cat)
  expected = 3
  expect_equal(
    retrieve_categorical_columns(dataset),
    expected
  )
  expect_equal(
    retrieve_categorical_columns(dataset2),
    expected
  )
})
