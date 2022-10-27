STAT 545B Assignment B4 Exercise 2
================

Reference: <https://github.com/expersso/piglatin/blob/master/R/main.R>

``` r
library(testthat)
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:testthat':
    ## 
    ##     is_null

``` r
library(stringr)
```

``` r
#' Translate word to My Pig Latin
#'
#' (Pig Letter include 'p', 'i', 'g', 'P', 'I', 'G')
#' My own rule is to add “pig” to the end of the word
#' after applying the following rearranagement rule:
#' 1. Find the first occurence of one Pig Letter
#' 2. Move that character plus all following ones to front 
#'
#' @param word A character string to be translated
#'
#' @return A character string in my Pig Latin format
pigify_word_new <- function(word) {
  stopifnot(class(word) == "character")
  pig <- c("p", "i", "g", "P", "I", "G")
  letters <- str_split(word, "")[[1]] 
  first_pig <- detect_index(letters, ~.x %in% pig)
  split(letters, cumsum(1 : length(letters) %in% first_pig)) %>%
    rev() %>% # Put all fish part to the end
    flatten_chr() %>%
    c("p", "i", "g") %>% # Append "pig" to the end
    paste(collapse = "")
}
```

The “Pig Letter” includes “p”, “i”, “g” (not case sensitive). My rule is
to add “pig” to the end of the word after rearranging the word’s letters
according to my rearrangement rule. My rearrangement rule is 1. Find the
first occurrence of one Pig Letter. 2. Move that first Pig Letter along
with all the following characters to the front.

``` r
# Examples: input must be character type 
pigify_word_new("FISH") # first Pig Letter is i
```

    ## [1] "ISHFpig"

``` r
pigify_word_new("dolphin") # first Pig Letter is p
```

    ## [1] "phindolpig"

``` r
pigify_word_new("shark") # no Pig Letter, only add "pig" at the end
```

    ## [1] "sharkpig"

``` r
# Test the function
test_that("Test one word with no Pig Letter", {
  expect_equal(pigify_word_new("cat"), "catpig")
})
```

    ## Test passed 🌈

``` r
test_that("Test one word with Pig Letter in the middle (upper case)", {
  expect_equal(pigify_word_new("FISH"), "ISHFpig")
})
```

    ## Test passed 🌈

``` r
test_that("Test one word with Pig Letter in the front (lower case)", {
  expect_equal(pigify_word_new("pet"), "petpig")
})
```

    ## Test passed 🌈
