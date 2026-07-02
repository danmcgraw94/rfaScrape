test_that("scrape_rfa_sqlite runs without error and creates expected folders", {
  sample_path <- system.file(
    "extdata",
    "example_RFA_project.rfa.sqlite",
    package = "rfaScrape"
  )
  out_dir <- withr::local_tempdir()

  project_dir <- scrape_rfa_sqlite(sample_path, out_dir)

  expect_true(dir.exists(project_dir))
  expect_true(dir.exists(file.path(project_dir, "input_data")))
  expect_true(dir.exists(file.path(project_dir, "analyses")))
  expect_true(dir.exists(file.path(project_dir, "res_models")))
  expect_true(dir.exists(file.path(project_dir, "simulations")))
})
