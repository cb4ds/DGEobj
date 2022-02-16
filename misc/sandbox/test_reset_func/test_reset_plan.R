# ----------------------------------
# Copyright(c) Aggregate Genius Inc.
# ----------------------------------

## TEST ATTRIBUTES - test attributes are reset back to original after changes
# * **setAttribute/s**: Allows setting attributes (facilitates attribute templates)
# * **getAttribute/s**: Return attributes

## TEST DIMENSIONS - test that dimension and data organization changes are reverted
# test that dimensions, row/col names are returned to original state after change
# * **dim**: Return the dimensions of the DGEobj (the assay dimensions)
# * **dimnames**:  Return the row (gene) and column (sample) names

## TEST DATA ITEM MANIPULATION - test that reset reverts all these changes
# manipulations
# * **addItem/s**:  Add a data item to a DGEobj
# * **rmItem/s**:  Remove a data item from a DGEobj
# * **newType**:  Define a new data type

# verifications
# test data item types are reset back to original
# * **getType**:  Return data item(s) by item type
# reset data item names to original
# * **getItem/s**:  Return a data item(s) by item name
# reset BaseTypes back to original, removes added ones(?)
# * **getBaseType**:  Return data item(s) by baseType
# test all types are reset- also see structure baseType tests
# * **showTypes**:  Show the type definitions of a DGEobj (all currently defined  types)


## TEST CONVERSIONS - convert and convert back using reset
# test that resetting an unclassed DGEObj resets it back to a valid s3 (not a list)
# * **as.list**:  unclass a DGEobj to simple list


## TEST ASSOCIATED OBJECTS AND FILES
# Should GRanges object be reset as well?
# If the gene data object (row annotation) contains chromosome position information (name, start, end, strand), a GRanges object will also be created

# test integrity of metadata files with _orig and that resetting DGEObj causes it to match this
# During initialization, a copy of the counts, gene annotation and sample annotation is duplicated and stored in the meta slot with an "_orig" suffix on the itemName.  This preserves the original data if you subset the original data.

# test summary has been reset back to original state
# * **inventory**:  Print a summary of the contents of a DGEobj, date created and optionally the funArgs history
