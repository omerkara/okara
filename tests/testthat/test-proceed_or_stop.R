test_that("Proceed.or.Stop() Tells to Proceed or Stop", {
    expect_identical(Proceed.or.Stop(5 == 5), message("Yeeaah: Everything is fine. You can proceed safely.\n"))
})
