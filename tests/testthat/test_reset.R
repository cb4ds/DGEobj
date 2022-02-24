context("reset.R functions")


test_that('reset.R: ', {

    new <- resetDGEobj(t_obj)

    # The following are defined in setup.R: gene_data, isoform_data, protein_data, exon_data

    # test gene level data
    test_gene_obj <- function(gene_data) {

        test_t_obj <- gene_data
        test_t_reset <- resetDGEobj(test_t_obj)

        # object validation
        expect_s3_class(test_t_reset, "DGEobj")
        expect_equivalent(showMeta(test_t_reset), showMeta(gene_data))

        # validate level is gene before and after reset
        test_t_meta <- showMeta(test_t_obj)
        expect_equal("gene", test_t_meta$Value[3])
        expect_equal(attr(test_t_obj, "level"), "gene")

        test_t_meta_reset <- showMeta(test_t_reset)
        expect_equal("gene", test_t_meta_reset$Value[3])
        expect_equal(attr(test_t_reset, "level"), "gene")

        # reset after subsetting
        test_t_dim <- dim(test_t_obj)
        expect_equal(dim(test_t_obj),c(951, 48))
        test_t_obj <- test_t_obj[c(1:10), ]
        test_t_reset <- resetDGEobj(test_t_obj)
        expect_equal(dim(test_t_reset), c(1000, 48))

        # check names after reset
        expect_named(test_t_obj, c('counts_orig', 'counts', 'design_orig', 'design', 'geneData_orig',
                                   'geneData', 'granges_orig', 'granges', 'DGEList', 'ReplicateGroupDesign',
                                   'ReplicateGroupDesign_Elist', 'ReplicateGroupDesign_fit',
                                   'ReplicateGroupDesign_fit_cm', 'ReplicateGroupDesign_fit_cf',
                                   'BDL_vs_Sham', 'EXT1024_vs_BDL', 'Nint_vs_BDL', 'Sora_vs_BDL'))
        expect_named(test_t_reset, c("counts_orig", "counts", "design_orig", "design",
                                   "geneData_orig", "geneData", "granges_orig", "granges"))


        # testing t_obj with rm item
        test_t_obj <- rmItem(test_t_obj, "design")
        expect_false("design" %in% names(test_t_obj))
        test_t_reset <- resetDGEobj(test_t_obj)
        expect_false("design" %in% names(test_t_reset)) # check if persists

        # testing t_obj with add item
        test_t_obj <- addItem(test_t_obj,
                              item = 'Fred Flintstone',
                              itemName = 'Cartoon',
                              itemType = 'meta',
                              itemAttr = list('MyAttribute' = 'testObject'))
        expect_true("Cartoon" %in% names(test_t_obj))
        test_t_reset <- resetDGEobj(test_t_obj)
        expect_false("Cartoon" %in% names(test_t_reset)) # check if removed

        # testing t_obj after class change
        test_t_obj <- as.list.DGEobj(test_t_obj)
        expect_equal(class(test_t_obj), "list")
        # coerce back into DGEobj, expect reset to work
        class(test_t_obj) <- "DGEobj"
        expect_silent(resetDGEobj(test_t_obj))
    }

    #run
    expect_success(test_gene_obj(t_obj))

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
