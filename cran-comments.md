## Comments from Maintainer

* made GenomicRanges package optional - used only for exon/gene levels
* added "protein" level data, reviewed/updated isoform data for proper handling
* added imputationMatrix as a type of metadata
* reworked reset to resolve issues and remove unneeded code
* updated tests and documentation

---  

## Test environments

RStudio Server Pro (ubuntu 18.04.2)  

* R 3.6.3
* R 4.0.5
* R 4.1.3

CircleCI

* R 4.0.5
* R 4.1.3

WinBuilder

* devtools::check_win_devel()  
* devtools::check_win_release()  

RHub

* devtools::check_rhub(interactive = F)

---  

## R CMD check results


```
devtools::check()  

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

---  

## Reverse dependencies


**NONE**

```
revdepcheck::cran_revdeps('DGEobj', bioc = T)

[1] "DGEobj"
```
