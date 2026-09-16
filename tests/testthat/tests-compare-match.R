
# match_hash -------------------------------------------------------------------

# Tests:
# - Assert examples

htab <- utils::hashtab(size = 2)
utils::sethash(htab, 1.42, base::sum)
utils::sethash(htab, "oi", -Inf)

test_that("Examples - match_hash", {
  expect_identical(match_hash(1.42, 1.42 ~ base::sum, "oi" ~ -Inf), base::sum)
  expect_identical(match_hash(1.42, htab = htab), base::sum)
})



# match_id ---------------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - match_id", {
  expect_identical(match_id(1.42, "oi" ~ -Inf, 1.42 ~ .x * 2), 2.84)
})



# match_ptype ------------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - match_ptype", {
  expect_identical(match_ptype(1.42, character() ~ "chr", double() ~ "dbl"), "dbl")
})



# match_when -------------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - match_when", {
  expect_identical(match_when("oi", grepl("1", .x) ~ "1", grepl("o", .x) ~ "o"), "o")
})
