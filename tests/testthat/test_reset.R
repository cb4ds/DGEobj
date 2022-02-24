context("reset.R functions")


test_that('reset.R: ', {

    new <- resetDGEobj(t_obj)

    gene_obj <-  readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
    isoform_obj <- "placeholder_value"
    protein_obj <- "placeholder_value"
    exon_obj <- "placeholder_value"


    # test gene level data
    test_gene_obj <- function(gene_obj) {

        test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                 rowData = t_obj$geneData,
                                 colData = t_obj$design,
                                 level = "gene")


        # object validation
        expect_s3_class(test_t_obj, "DGEobj")


        # validate level is gene
        level_check <- function(test_t_obj) {
            test_t_meta <- showMeta(test_t_obj)
            expect_equal("gene", test_t_meta$Value[3])
            expect_equal(attr(test_t_obj, "level"), "gene") # temp
            expect_named(test_t_obj, c("counts_orig", "counts", "design_orig", "design",
                                       "geneData_orig", "geneData", "granges_orig", "granges")) }

        level_check(test_t_obj)

        # changes
        #add rm attribute- should persist
        #subset
        #etc

        # change level
        expect_failure(level_check(test_t_obj))

        # check level is reset
        expect_silent(resetDGEobj(test_t_obj))

        # test levels back to normal
        expect_success(level_check(test_t_obj))

    }

    #run
    expect_success(test_gene_obj(gene_obj))

    # test isoform level data
    test_isoform_obj <- function(isoform_obj) {

        test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                 rowData = t_obj$geneData,
                                 colData = t_obj$design,
                                 level = "isoform")

        # check level is set to isoform
        test_t_meta <- showMeta(test_t_obj)
        expect_equal("isoform", test_t_meta$Value[3])
        expect_named(new, c("counts_orig", "counts", "design_orig", "design",
                            "isoformData_orig", "isoformData", "granges_orig", "granges")) #specifically for gene one - do one for each level

        # check level is reset
        resetDGEobj(test_t_obj)
        # some test here

    }

    # test protein level data
    test_protein_obj <- function(protein_obj) {

        # test levels - will be replaced with actual data
        test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                 rowData = t_obj$geneData,
                                 colData = t_obj$design,
                                 level = "protein")

        # check level is set to protein
        test_t_meta <- showMeta(test_t_obj)
        expect_equal("protein", test_t_meta$Value[3])
        expect_named(new, c("counts_orig", "counts", "design_orig", "design",
                            "proteinData_orig", "proteinData", "granges_orig", "granges")) #specifically for gene one - do one for each level

        # check level is reset
        expect_silent(resetDGEobj(test_t_obj))

        # some test here
    }

    # test exon level data
    test_exon_obj <- function(exon_obj) {

        # test levels - will be replaced with actual data
        test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                 rowData = t_obj$geneData,
                                 colData = t_obj$design,
                                 level = "exon")


        # check level is set to protein
        test_t_meta <- showMeta(test_t_obj)
        expect_equal("exon", test_t_meta$Value[3])
        expect_named(new, c("counts_orig", "counts", "design_orig", "design",
                            "exonData_orig", "exonData", "granges_orig", "granges"))

        # check level is reset
        expect_silent(resetDGEobj(test_t_obj))

        # some test here
    }

    # test affy level data
    test_affy_obj <- function(exon_obj) {

        # test levels - will be replaced with actual data
        test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                 rowData = t_obj$geneData,
                                 colData = t_obj$design,
                                 level = "affy")


        # check level is set to protein
        test_t_meta <- showMeta(test_t_obj)
        expect_equal("affy", test_t_meta$Value[3])
        expect_named(new, c("counts_orig", "counts", "design_orig", "design",
                            "affyData_orig", "affyData", "granges_orig", "granges"))

        # check level is reset
        resetDGEobj(test_t_obj)

        # some test here
    }

    #test invalid level
    expect_error(test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                          rowData = t_obj$geneData,
                                          colData = t_obj$design,
                                          level = "Fred Flinstone"),
                                          regexp = "The specified level must be one of:  gene, exon, isoform, protein, affy")

    # compare objects and classes
    expect_s3_class(new, "DGEobj")
    expect_equivalent(new, t_obj)
    expect_equivalent(showMeta(new), showMeta(t_obj)) # add more thorough test, check for removals

    # reset subsetting
    # check dim before
    expect_equal(t_dim, c(1000, 48))
    test_t_obj <- t_obj[c(1:10), ]
    test_t_obj <- resetDGEobj(test_t_obj)
    expect_equal(dim(test_t_obj), c(1000, 48))

    # testing t_obj with new attributes
    new_attributes <- list("attribute1" = runif(100, min = 0, max = 2), "attribute2" = LETTERS)
    test_t_obj <- setAttributes(t_obj, new_attributes)
    expect_equivalent(getAttributes(resetDGEobj(test_t_obj)), getAttributes(test_t_obj)) # should be equivalent

    # testing t_obj with attributes set to NULL
    test_t_obj  <- t_obj
    getAttributes(test_t_obj)
    test_t_obj <- setAttributes(test_t_obj, NULL) #for loop through list to set to NULL
    resetDGEobj(test_t_obj)
    getAttributes(test_t_obj)
    # use for loop instead to preserve DGEobj class, make sure it's empty, can reset after - keep in mind level
    # check empty
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "primaryAssayData must be specified as a matrix or a data.frame.",
                 fixed = TRUE)

    # testing t_obj without platformType (no longer required for reset)
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = NULL))
    expect_s3_class(resetDGEobj(test_t_obj), "DGEobj")

    # testing t_obj before and after conversion
    test_t_obj <- as.list(t_obj) # returns class() list
    # coerce back into DGEobj, expect equiv after
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "The DGEobj must be of class 'DGEobj'.")

})
