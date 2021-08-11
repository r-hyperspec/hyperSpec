# hyperSpec 1.0.0 2021-xx-xx

## Acknowledgements

* The `hyperSpec` team gratefully acknowledges support from the Google Summer of Code program, which sponsored student Erick Oduniyi during summer 2020. Erick and the team carried out a significant overhaul of `hyperSpec` which led to this major release.


## User-Facing Changes from 0.99 Series

* The GitHub repository of this package was moved to https://github.com/r-hyperspec/hyperSpec
* Documentation now available in `pkgdown` sites (https://r-hyperspec.github.io/).
* `NEWS.md` (this file) added so that users can readily see changes that may affect the use of the package.
* Introductory vignette reorganized and enhanced, thanks to Bryan Hanson.
* Dataset `faux_cell` and function `generate_faux_cell()` replace `chondro` dataset (cbeleites/hyperSpec#125, cbeleites/hyperSpec#156, cbeleites/hyperSpec#180, cbeleites/hyperSpec#229).
* Portions of `hyperSpec` were spun out into their own packages for ease of maintenance. 
**--- ELABORATE ---**
    - Dataset `chondro` was moved to package **hySpc.chondro** (https://r-hyperspec.github.io/hySpc.chondro/).
    - Functions `qplotspc()`, `qplotmap()`, `qplotc()`, `qplotmixmap()`, `legendright()`,  `qmixtile()`, `normalize.colrange()`, `normalize.range()`, `normalize.null()`, `normalize.minmax()`, `qmixlegend()`, `colmix.rgb()` were deprecated due analogous functionality in package **hySpc.ggplot2** (https://r-hyperspec.github.io/hySpc.ggplot2/).
    - Functions `read.ENVI()`, `read.ENVI.HySpex()`, `read.ENVI.Nicolet()` were deprecated due to analogous functionality in package **hySpc.read.ENVI** (https://r-hyperspec.github.io/hySpc.read.ENVI/).
    - Functions `read.spc()`, `read.spc.Kaiser()`, `read.spc.KaiserMap()`, `read.spc.KaiserLowHigh()` were deprecated due to analogous functionality in package **hySpc.read.spc** (https://r-hyperspec.github.io/hySpc.read.spc/).
    - Functions `read.spe()`, `spe.showcalpoints()` were deprecated due to analogous functionality in package **hySpc.read.spe** (https://r-hyperspec.github.io/hySpc.read.spe/).
    - Functions `read.mat.Cytospec()`, `read.mat.Witec()` were deprecated due to analogous functionality in package **hySpc.read.mat** (https://r-hyperspec.github.io/hySpc.read.mat/).
    - Function `read.jdx()` was deprecated due to analogous functionality in package **hySpc.read.jdx** (https://r-hyperspec.github.io/hySpc.read.jdx/).
    - Functions `read.asc.Andor()`, `read.asc.PerkinElmer()`, `read.txt.Horiba()`, `read.txt.Horiba.xy()`, `read.txt.Horiba.t()`, `read.txt.long()`, `read.txt.Renishaw()`,  `read.zip.Renishaw()`, `read.txt.Shimadzu()`, `read.txt.wide()`, `read.txt.Witec()`, `read.txt.Witec.Graph()`, `read.dat.Witec()`, `wc()`, `count_lines()` were deprecated due analogous functionality in package **hySpc.read.txt** (https://r-hyperspec.github.io/hySpc.read.txt/).
    - The following functions were renamed or replaced by new ones (see table below).
    
     Deprecated function     | New (replacement) function       | Related issues
    -------------------------|-------------------------------   | ----------------
     `.fileio.optional()`    | `.spc_io_postprocess_optional()` |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#302
     `.fix_spc_colnames()`   | `.spc_fix_colnames()`            |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `alois.palette()`       | `palette_alois()`                |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#299, @sangttruong 
     `chk.hy()`              | `assert_hyperSpec()`             |  #34
     `guess.wavelength()`    | `extract_numbers()`              |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
     `hy.getOption()`        | `hy_get_option()`                |  #21
     `hy.getOptions()`       | `hy_get_options()`               |  #21
     `hy.setOptions()`       | `hy_set_options()`               |  #21
     `matlab.dark.palette()` | `palette_matlab_dark()`          |  cbeleites/hyperSpec#299, cbeleites/hyperSpec#299, @sangttruong
     `matlab.palette()`      | `palette_matlab()`               |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#299, @sangttruong
     `mergeextra()`          | `merge_data()`                   |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#302
     `orderwl()`             | `wl_sort()`                      |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
     `pearson.dist()`        | `dist_pearson()`                 |  #19
     `spc.bin()`             | `spc_bin()`                      |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `spc.fit.poly()`        | `spc_fit_poly()`                 |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `spc.fit.poly.below()`  | `spc_fit_poly_below()`           |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `spc.loess()`           | `spc_loess()`                    |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `spc.NA.approx()`       | `spc_na_approx()`                |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `spc.rubberband()`      | `spc_rubberband()`               |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `spc.smooth.spline()`   | `spc_smooth_spline()`            |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#301
     `wl.eval()`             | `wl_eval()`                      |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
     `wlconv()`              | `wl_convert_units()`             |  cbeleites/hyperSpec#208, cbeleites/hyperSpec#309
    - Wavelength unit conversion functions `ev2freq()`, `ev2invcm()`, `ev2nm()`, `ev2raman()`, `freq2ev()`, `freq2invcm()`, `freq2nm()`, `freq2raman()`, `invcm2ev()`, `invcm2freq()`, `invcm2nm()`, `invcm2raman()`, `nm2ev()`, `nm2freq()`, `nm2invcm()`, `nm2raman()`, `raman2ev()`, `raman2freq()`, `raman2invcm()`, `raman2nm()` are deprecated in favor of `wl_convert_units()` (cbeleites/hyperSpec#300).
* Function `spc.NA.linapprox()`, which was deprecated for long time, is now completely removed (cbeleites/hyperSpec#239).
* Column names in spectra matrix (`$spc` column of `hyperSpec` object) are now returned correctly by functions `spc.bin()` (cbeleites/hyperSpec#237), and `spc.loess()` (cbeleites/hyperSpec#245).
* New function `hy_list_available_hySpc_packages()` lists packages, that are available in GitHub organization `r-hyperSpec`.
* New function `hy_browse_homepage()` opens the homepage of *R hyperSpec* in a web browser.
* New function `hy_list_installed_hySpc_packages()` lists and function `hy_attach()` conveniently loads and attaches all installed **`r-hyperspec`** family packages (@cbeleites, @GegznaV, cbeleites/hyperSpec#219).
* Changes related to function `as.hyperSpec()`:
    - New method `as.hyperSpec(<hyperSpec>)` was created (cbeleites/hyperSpec#282).
    - The default value of argument `wl` is now set to `wl = NULL` (cbeleites/hyperSpec#297).
    - `wl = NULL` now means that the default values of wavelengths should be calculated inside the methods of `as.hyperSpec()` (cbeleites/hyperSpec#297).
* Possibility to initialize `hyperSpec` object by providing wavelengths only (cbeleites/hyperSpec#288).
* Function `wl.eval()` is converted into S3 generic. Methods `wl.eval(<hyperSpec>)` and `wl.eval(<numeric>)` for numeric vectors were added (cbeleites/hyperSpec#287).
* New function `new_hyperSpec()` that initializes `hyperSpec` object in a similar way as `new("hyperSpec")` does but has autocompletion possibilities in RStudio (cbeleites/hyperSpec#283).
* Functions `show()` and `print()` give more concise default output now (@GegznaV, cbeleites/hyperSpec#211).
* The default output of function `summary()` was changed (@GegznaV, cbeleites/hyperSpec#211).
* New color palette `palette_colorblind` introduced (@bryanhanson).
* Function `sample()` gains new argument `index`; `sample(..., index = TRUE)` replaced function `isample()` (@GegznaV, #17).


## Non-User-Facing Changes from 0.99 Series

* Note: this listing is for the benefit of developers, and should summarize significant infrastructure changes.
* Vignettes converted to `.Rmd` and formatted consistently, thanks to @GegznaV.
* Package **hySpc.testthat** is now used for unit testing (cbeleites/hyperSpec#228).
