context("reset.R functions")


test_that('reset.R: ', {

    new <- resetDGEobj(t_obj)

    # compare objects and classes
    expect_s3_class(new, "DGEobj")
    expect_equivalent(new, t_obj)
    expect_equivalent(inventory(new), inventory(t_obj))


    ## TEST DATA ITEM MANIPULATION - test that reset reverts all these changes
    # manipulations
    # testing t_obj with new type
    test_t_obj <- newType(t_obj, "MyType", "meta")
    test_t_obj <- resetDGEobj(test_t_obj) # should this be removing the type?
    expect_identical(showTypes(test_t_obj)$Type, showTypes(t_obj)$Type)


    # testing t_obj with add item
    test_t_obj <- addItem(t_obj,
                   item = 'Fred Flintstone',
                   itemName = 'Cartoon',
                   itemType = 'meta',
                   itemAttr = list('MyAttribute' = 'testObject'))
    expect_equivalent(resetDGEobj(test_t_obj), t_obj) # Not passing - needs refinement
    expect_identical(resetDGEobj(new), test_t_obj) # need to omit "dateCreated" attribute - noisey

    # testing t_obj with rm item
    test_t_obj <- rmItem(t_obj, "design")
    expect_equivalent(resetDGEobj(test_t_obj), t_obj) # Not passing - needs refinement
    expect_identical(resetDGEobj(new), test_t_obj) # need to omit "dateCreated" attribute - noisey


    ## TEST DIMENSIONS - test that dimension and data organization changes are reverted
    # test that dimensions, row/col names are returned to original state after change
    expect_mapequal(new, t_obj)

    expect_setequal(names(new), c("counts_orig", "counts", "design_orig", "design",
                                  "geneData_orig", "geneData", "granges_orig", "granges"))

    expect_named(new, c("counts_orig", "counts", "design_orig", "design",
                        "geneData_orig", "geneData", "granges_orig", "granges"))

    expect_equal(dim(new), c(1000, 48))

    expect_equal(dimnames(new), dimnames(t_obj)) # refine this with actual output


    ## TEST ATTRIBUTES - test attributes are reset back to original after changes

    # testing t_obj with new attributes
    new_attributes <- list("attribute1" = runif(100, min = 0, max = 2), "attribute2" = LETTERS)
    test_t_obj <- setAttributes(t_obj, new_attributes)

    expect_equivalent(getAttributes(resetDGEobj(test_t_obj)), getAttributes(t_obj))

    # testing t_obj with attributes set to NULL
    test_t_obj  <- t_obj
    test_t_obj[] <- lapply(test_t_obj,
                           function(x) {
                               attributes(x) <- NULL; x })
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "primaryAssayData must be specified as a matrix or a data.frame.",
                 fixed = TRUE)

    # testing t_obj with attribute values set to NULL
    test_t_obj <- lapply(attributes(test_t_obj), function(x) {
        attr(x, "label") <- NULL
        attr(x, "labels") <- NULL})
    expect_error(resetDGEobj(test_t_obj),
                             regexp = "The DGEobj must be of class 'DGEobj'.",
                             fixed = TRUE)

    # testing t_obj without platformType
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = NULL))
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "Required attribute \"PlatformType\" is missing.",
                 fixed  = TRUE)

    # testing t_obj with unavailable data
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = "RNA-Seq"))
    names(test_t_obj) <- c("counts_orig", "counts", "design_orig", "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # testing t_obj with bad platformType
    test_t_obj1 <- setAttributes(t_obj, list("PlatformType" = "fred"))
    expect_error(resetDGEobj(test_t_obj1),
                 regexp = "The PlatformType attribute value was not recognized!")


    # testing t_obj before and after conversion
    test_t_obj <- as.list(t_obj) # returns class() list
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "The DGEobj must be of class 'DGEobj'.")





})
