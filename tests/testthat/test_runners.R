context("Runner functions")

test_that("zigzag() detects oscillating patterns", {
    expect_equal(zigzag(c(-1, 1, -1)), TRUE)
    expect_equal(zigzag(c(0, 0, 0)), FALSE)
    expect_equal(zigzag(c(0, 1, 1)), FALSE)
    expect_equal(zigzag(c(-1, 1)), TRUE)
    expect_equal(zigzag(c(-1, NA)), TRUE)    
    expect_equal(zigzag(-1), FALSE)
})

test_that("monotone() detects strictly increasing/decreasing patterns", {
    expect_equal(monotone(c(0, 1, 2)), TRUE)
    expect_equal(monotone(c(2, 1, 0)), TRUE)
    expect_equal(monotone(c(0, 1, 1)), FALSE)
    expect_equal(monotone(c(0, 1, 0)), FALSE)
    expect_equal(monotone(c(0, 1, NA)), FALSE)    
})

test_that("inside() detects vectors inside a stripe", {
    expect_equal(inside(c(0, 1, 0, 1), 4, stripe(-5, 5)), TRUE)
    expect_equal(inside(c(0, 1, 0, 1), 3, stripe(-5, 5)), TRUE)
    expect_equal(inside(c(10, -10, 10, -10), 4, stripe(-1, 1)), FALSE)
    expect_equal(inside(c(0, 0, 0, 10), 3, stripe(-1, 1)), TRUE)
})

test_that("outside() detects vectors outside a stripe (on the same side)", {
    expect_equal(outside(c(0, 1, 0, 1), 4, stripe(-5, 5)), FALSE)
    expect_equal(outside(c(0, 1, 0, 1), 3, stripe(-5, 5)), FALSE)
    expect_equal(outside(c(10, 10, 10, 10), 4, stripe(-1, 1)), TRUE)
    expect_equal(outside(c(-10, -10, -10, 0), 3, stripe(-1, 1)), TRUE)
    expect_equal(outside(c(-10, -10, -10, -10), 4, stripe(-1, 1)), TRUE)    
    expect_equal(outside(c(-10, 10, -10, 10), 4, stripe(-1, 1)), FALSE) # Values are on different sides
    expect_equal(outside(c(0, 0, 0, 10), 3, stripe(-1, 1)), FALSE)
})

test_that("outside() detects vectors outside a stripe (on different sides)", {
    expect_equal(outside(c(0, 1, 0, 1), 4, stripe(-5, 5)), FALSE)
    expect_equal(outside(c(0, 1, 0, 1), 3, stripe(-5, 5)), FALSE)
    expect_equal(outside(c(0, 0, 0, 10), 3, stripe(-1, 1)), FALSE)
    expect_equal(outside(c(0, -10, 10, 10), 3, stripe(-1, 1)), FALSE)    
    expect_equal(outside(c(10, 10, 10, 10), 4, stripe(-1, 1), mixing=TRUE), FALSE)
    expect_equal(outside(c(-10, -10, -10, -10), 4, stripe(-1, 1), mixing=TRUE), FALSE) # Values are on different sides
    expect_equal(outside(c(-10, 10, -10, 10), 4, stripe(-1, 1), mixing=TRUE), TRUE) # Values are on different sides    
})

test_that("chunkify applies runners", {
    expect_equal(chunkify(c(1, 0, 1, 0), zigzag, 4), c(FALSE, FALSE, FALSE, TRUE))
    expect_equal(chunkify(c(1, 0, 1, 1), zigzag, 3), c(FALSE, FALSE, TRUE, FALSE))
    
    expect_equal(chunkify(c(2, 2, 2), function(x) outside(x, 3, stripe(0, 1)), 3), c(FALSE, FALSE, TRUE))
    expect_equal(chunkify(c(2, 2, 2), function(x) outside(x, 3, stripe(0, 1)), 4), c(FALSE, FALSE, TRUE))
})
