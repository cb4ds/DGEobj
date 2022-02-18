context("reset.R functions")


test_that('reset.R: ', {
    # testing valid t_obj
    new <- resetDGEobj(t_obj)
    expect_s3_class(new, "DGEobj")
    expect_setequal(names(new), c("counts_orig", "counts", "design_orig", "design",
                                  "geneData_orig", "geneData", "granges_orig", "granges"))
    expect_equal(dim(new), c(1000, 48))

    # testing t_obj without platformType (no longer required for reset)
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = NULL))
    expect_s3_class(resetDGEobj(test_t_obj), "DGEobj")

})
