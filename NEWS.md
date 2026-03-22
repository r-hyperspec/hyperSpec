
# hyperSpec 0.101.0 & 0.200.0.9000 (2024-05-01; development version)


## User-Facing Changes from Previous Versions 


### Repository

* The GitHub repository of this package was moved to https://github.com/r-hyperspec/hyperSpec


### Documentation

* `NEWS.md` (this file) added so that users can readily see changes that may affect the use of the package.
* Vignettes converted to `.Rmd` and formatted consistently, thanks to @GegznaV.
* Introductory vignette `hyperSpec.Rmd` reorganized and enhanced, thanks to Bryan Hanson (@bryanhanson).
* Documentation is now available in the form of `pkgdown` sites at https://r-hyperspec.github.io/


### New Functions and Methods

* New color palette `palette_colorblind` introduced (@bryanhanson).
* New function `hy_browse_homepage()` opens the homepage of *R hyperSpec* in a web browser.
* New function `hy_list_available_hySpc_packages()` lists packages, that are available in GitHub organization `r-hyperSpec`.
* New function `hy_list_installed_hySpc_packages()` lists and function `hy_attach()` conveniently loads and attaches all installed **`r-hyperspec`** family packages (@cbeleites, @GegznaV, cbeleites/hyperSpec#219).
* New function `hyperSpec()` that initializes `hyperSpec` object in a similar way as `new("hyperSpec")` does but has autocompletion possibilities in RStudio (cbeleites/hyperSpec#283, #129).
* New function `wl_convert_units()` (cbeleites/hyperSpec#300).
* New function `wl_create_label_from_units()` that creates labels for wavelength axis (@GegznaV).
* New method `as.hyperSpec(<hyperSpec>)` (cbeleites/hyperSpec#282).


### Changes

* `as.hyperSpec()`: The default value of argument `wl` is now set to `wl = NULL` (cbeleites/hyperSpec#297).
* `as.hyperSpec()`: `wl = NULL` now means that the default values of wavelengths should be calculated inside the methods of `as.hyperSpec()` (cbeleites/hyperSpec#297).
* Methods `show(<hyperSpec>)` and `print(<hyperSpec>)` give more concise default output now (@GegznaV, cbeleites/hyperSpec#211).
* The default output of method `summary(<hyperSpec>)` was changed (@GegznaV, cbeleites/hyperSpec#211).
* Function `wl.eval()` is converted into S3 generic. Methods `wl.eval(<hyperSpec>)` and `wl.eval(<numeric>)` for numeric vectors were added (cbeleites/hyperSpec#287).
* Function `sample()` gains new argument `index`; `sample(..., index = TRUE)` replaced function `isample()` (@GegznaV, #17).
* Function `wl_convert_units()` converted into S3 generic. Default and hyperSpec methods were added (#29).
* Dataset `faux_cell` and function `generate_faux_cell()` replace `chondro` dataset (cbeleites/hyperSpec#125, cbeleites/hyperSpec#156, cbeleites/hyperSpec#180, cbeleites/hyperSpec#229).
* Documentation aliases have been updated. Now, ?hyperSpec points to the function `hyperSpec()`, and to refer to the package, `package?hyperSpec` should be used (#129).
* Vignette `hyperSpec.Rmd`: suggested packages (`pls`) are now loaded conditionally so the vignette builds even when they are not installed.


### Bugfixes

* Possibility to initialize `hyperSpec` object by providing wavelengths only (cbeleites/hyperSpec#288).
* Column names in spectra matrix (`$spc` column of `hyperSpec` object) are now returned correctly by functions `spc.bin()` (cbeleites/hyperSpec#237), and `spc.loess()` (cbeleites/hyperSpec#245
* `all.equal()` method for `hyperSpec` objects converted from S4 to S3 registration. The S4 `setMethod()` approach for this S3 generic caused roxygen2 (>= 7.3) to crash during documentation generation.
* `rbind.fill()` (internal): strip the `AsIs` class from matrix columns after data frame assembly. R >= 4.4 introduced `all.equal.AsIs()` which caused spurious class-mismatch failures when comparing `hyperSpec` objects that had been through `rbind` or `collapse`.
* `wl_create_label_from_units()`: fixed incorrect use of `grep()` where `sub()` was intended, which caused malformed wavelength axis labels for units containing `"_greek"`.
* `collapse()`: correctly sorts wavelengths in the merged result and preserves expected row order when combining `hyperSpec` objects with differing wavelength axes.


### Soft Deprecation: Functions That Will Be Moved to Other Packages

Portions of package `hyperSpec` were partitioned into individual packages to facilitate maintenance.

* Dataset `chondro` was moved to package **hySpc.chondro** (https://r-hyperspec.github.io/hySpc.chondro/).
* Functions `qplotspc()`, `qplotmap()`, `qplotc()`, `qplotmixmap()`, `legendright()`,  `qmixtile()`, `normalize.colrange()`, `normalize.range()`, `normalize.null()`, `normalize.minmax()`, `qmixlegend()`, `colmix.rgb()` were deprecated due to analogous functionality in package **hySpc.ggplot2** (https://r-hyperspec.github.io/hySpc.ggplot2/).
* Functions `read.ENVI()`, `read.ENVI.HySpex()`, `read.ENVI.Nicolet()` were deprecated due to analogous functionality in package **hySpc.read.ENVI** (https://r-hyperspec.github.io/hySpc.read.ENVI/).
* Functions `read.spc()`, `read.spc.Kaiser()`, `read.spc.KaiserMap()`, `read.spc.KaiserLowHigh()` were deprecated due to analogous functionality in package **hySpc.read.spc** (https://r-hyperspec.github.io/hySpc.read.spc/).
* Functions `read.spe()`, `spe.showcalpoints()` were deprecated due to analogous functionality in package **hySpc.read.spe** (https://r-hyperspec.github.io/hySpc.read.spe/).
* Functions `read.mat.Cytospec()`, `read.mat.Witec()` were deprecated due to analogous functionality in package **hySpc.read.mat** (https://r-hyperspec.github.io/hySpc.read.mat/).
* Function `read.jdx()` was deprecated due to analogous functionality in package **hySpc.read.jdx** (https://r-hyperspec.github.io/hySpc.read.jdx/).
* Functions `read.asc.Andor()`, `read.asc.PerkinElmer()`, `read.txt.Horiba()`, `read.txt.Horiba.xy()`, `read.txt.Horiba.t()`, `read.txt.long()`, `read.txt.Renishaw()`, `read.zip.Renishaw()`, `read.txt.Shimadzu()`, `read.txt.wide()`, `read.txt.Witec()`, `read.txt.Witec.Graph()`, `read.dat.Witec()`, `wc()`, `count_lines()` were deprecated due analogous functionality in package **hySpc.read.txt** (https://r-hyperspec.github.io/hySpc.read.txt/).


### Soft Deprecation: Function Name Standardization

The names of functions in `hyperSpec` started to be standardized to (a) have more self-explanatory names which reflect the essence of the functions and (b) to be more in alignment with the Tidyverse style.
The following functions were renamed (see the table and the list below). In the current version of package, both old and new functions exist. But gradually the old functions will go through the deprecation phases and finally will be removed in the future versions of the package.

* Wavelength unit conversion functions `ev2freq()`, `ev2invcm()`, `ev2nm()`, `ev2raman()`, `freq2ev()`, `freq2invcm()`, `freq2nm()`, `freq2raman()`, `invcm2ev()`, `invcm2freq()`, `invcm2nm()`, `invcm2raman()`, `nm2ev()`, `nm2freq()`, `nm2invcm()`, `nm2raman()`, `raman2ev()`, `raman2freq()`, `raman2invcm()`, `raman2nm()` are deprecated in favor of `wl_convert_units()` (cbeleites/hyperSpec#300).

 Function to Deprecate   | New (replacement) function       | Related issues
-------------------------|-------------------------------   | ----------------
 `.fileio.optional()`    | `.spc_io_postprocess_optional()` |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#302
 `.fix_spc_colnames()`   | `.spc_fix_colnames()`            |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `alois.palette()`       | `palette_alois()`                |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#299, @sangttruong 
 `chk.hy()`              | `assert_hyperSpec()`             |  #34
 `fitraster()`           | `raster_fit()`                   |  #47
 `guess.wavelength()`    | `extract_numbers()`              |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
 `hy.getOption()`        | `hy_get_option()`                |  #21
 `hy.getOptions()`       | `hy_get_options()`               |  #21
 `hy.setOptions()`       | `hy_set_options()`               |  #21
 `makeraster()`          | `raster_make()`                  |  #47
 `mark.dendrogram()`     | `mark_groups_in_dendrogram()`    |  #43
 `markpeak()`            | `mark_peak()`                    |  #44
 `matlab.dark.palette()` | `palette_matlab_dark()`          |  cbeleites/hyperSpec#299, cbeleites/hyperSpec#299, @sangttruong
 `matlab.palette()`      | `palette_matlab()`               |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#299, @sangttruong
 `mergeextra()`          | `merge_data()`                   |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#302
 `orderwl()`             | `wl_sort()`                      |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
 `normalize01()`         | `normalize_01()`                 |  #50
 `pearson.dist()`        | `dist_pearson()`                 |  #19
 `plotc()`               | `plot_c()`                       |  #48
 `plotmap()`             | `plot_map()`                     |  #48
 `plotmat()`             | `plot_matrix()`                  |  #48
 `plotspc()`             | `plot_spc()`                     |  #48
 `plotvoronoi()`         | `plot_voronoi()`                 |  #48
 `pooled.cov()`          | `cov_pooled()`                   |  #51
 `spc.bin()`             | `spc_bin()`                      |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `spc.fit.poly()`        | `spc_fit_poly()`                 |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `spc.fit.poly.below()`  | `spc_fit_poly_below()`           |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `spc.identify()`        | `identify_spc()`                 |  #40
 `spc.label.default()`   | `format_label_ispc_wl()`         |  #39
 `spc.label.wlonly()`    | `format_label_wl_only()`         |  #39
 `spc.loess()`           | `spc_loess()`                    |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `spc.NA.approx()`       | `spc_na_approx()`                |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `spc.point.default()`   | `locate_spc_point_clicked()`     |  #46
 `spc.point.max()`       | `locate_spc_point_max()`         |  #46
 `spc.point.min()`       | `locate_spc_point_min()`         |  #46
 `spc.point.sqr()`       | `locate_spc_point_parabola_max()`|  #46
 `spc.rubberband()`      | `spc_rubberband()`               |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `spc.smooth.spline()`   | `spc_smooth_spline()`            |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
 `stacked.offsets()`     | `calculate_offsets()`            |  #41
 `wl.eval()`             | `wl_eval()`                      |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
 `wlconv()`              | `wl_convert_units()`             |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309


### Defunct/Removed Features

* Function `spc.NA.linapprox()`, which was deprecated for long time, is now completely removed (cbeleites/hyperSpec#239).


## Non-User-Facing Changes from 0.99 Series

* Code style improved to be more aligned with the Tidyverse style.
* Package **hySpc.testthat** is now used for unit testing (cbeleites/hyperSpec#228).
* Unit testing improved to cover more cases.


## Acknowledgements

* The `hyperSpec` team gratefully acknowledges support from the Google Summer of Code program, which sponsored student Erick Oduniyi during summer 2020. Erick and the team carried out a significant overhaul of `hyperSpec` which led to this release.

