context("tests the processDivRates function")

# relies heavily on readTrace(), so we only test for elements not
# included in the readTrace() testing.

test_that("processes birth-death scripts", {
        file_spectimes <-
                system.file("extdata",
                            "epi_bd/primates_EBD_speciation_times_mini.p",
                            package = "RevGadgets")
        file_specrates <-
                system.file("extdata",
                            "epi_bd/primates_EBD_speciation_rates_mini.p",
                            package = "RevGadgets")
        file_exttimes <-
                system.file("extdata",
                            "epi_bd/primates_EBD_extinction_times_mini.p",
                            package = "RevGadgets")
        file_extrates <-
                system.file("extdata",
                            "epi_bd/primates_EBD_extinction_rates_mini.p",
                            package = "RevGadgets")

        primates <- processDivRates(
                speciation_time_log = file_spectimes,
                speciation_rate_log = file_specrates,
                extinction_time_log = file_exttimes,
                extinction_rate_log = file_extrates,
                burnin = 0.25
        )
        expect_equal(ncol(primates), 5)
        expect_equal(class(primates), c("tbl_df", "tbl", "data.frame"))
        expect_equal(nrow(primates), 400)
})
