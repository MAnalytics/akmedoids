
## Test environments
* local OS Windows, R 3.5.3
* Debian Linux (R-devel), R 3.5.3
* win-builder (devel and release), R 3.5.3 


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

##

'Akmedoids' package updated 30th March 2021 (New version: v1.3.0)

### Updates:

'Akmedoids' package updated (Version: v1.3.0)

### Updates:
1. Added 4 new sample datasets, and used in the function examples. The datasets include `clustr`, `simulated`, `TO1Risk`, `traj_w_space`.
2. Added detailed descriptions of fields in each dataset. See `format` section of each dataset documentation.
3. Change the citation `Adepeju et al. 2019` to `Adepeju et al. 2021` based on the newly published article upon which the `akmedoids` package is based.
4. Modified the names of some functions, e.g. `akmedoids.clust` changed to `akclustr`,  `statPrint` changed to `print_akstats`, `population` changed to `popl`.
5. Added two new functions, namely; `remove_rows_n`, `plot_akstats` (see the documentation for details)
6. Ensured that the names of functions are in lower cases, e.g. `dataImputation` changed to `data_imputation`, `outlierDetect` changed to `outlier_detect`, etc.
7. Modify example in 'elbow_point' function to reduce run time

Your faithfully.
Monsuru.


