<!-- ---------------------------------------------------------------------- -->

<!-- badges: start -->
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![R-CMD-check](https://github.com/r-hyperspec/hyperSpec/workflows/R-CMD-check/badge.svg?branch=develop)](https://github.com/r-hyperspec/hyperSpec/actions)
[![Codecov](https://codecov.io/gh/r-hyperspec/hyperSpec/branch/develop/graph/badge.svg)](https://codecov.io/gh/r-hyperspec/hyperSpec?branch=develop)
![Website (pkgdown)](https://github.com/r-hyperspec/hyperSpec/workflows/Website%20(pkgdown)/badge.svg)
<!-- [![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) -->
<!-- badges: end -->



<!-- ---------------------------------------------------------------------- -->
# R Package **hyperSpec**
<!-- ---------------------------------------------------------------------- -->
<br>
<center>
<font color="red" size=4>
<b>Package `hyperSpec` is under overhaul now.</b>  
So this website is still under construction and the contents as well as resources are not fully updated yet.  
The documentation of version `0.100.0` is not present here too.
</font>
</center>
<br>
<!-- ---------------------------------------------------------------------- -->


[**R**](https://www.r-project.org/) package **hyperSpec** is the main package in the [**`r-hyperspec`**](https://r-hyperspec.github.io/) family of packages.
The goal of **hyperSpec** (and whole **`r-hyperspec`**) is to make the work with hyperspectral data sets, (i.e. spatially or time-resolved spectra, or spectra with any other kind of information associated with each of the spectra) more comfortable.
The spectra can be data as obtained during 
[XRF](https://en.wikipedia.org/wiki/X-ray_fluorescence),
[UV/VIS](https://en.wikipedia.org/wiki/Ultraviolet%E2%80%93visible_spectroscopy), 
[Fluorescence](https://en.wikipedia.org/wiki/Fluorescence_spectroscopy),
[AES](https://en.wikipedia.org/wiki/Auger_electron_spectroscopy),
[NIR](https://en.wikipedia.org/wiki/Near-infrared_spectroscopy),
[IR](https://en.wikipedia.org/wiki/Infrared_spectroscopy), 
[Raman](https://en.wikipedia.org/wiki/Raman_spectroscopy), 
[NMR](https://en.wikipedia.org/wiki/Nuclear_magnetic_resonance_spectroscopy), 
[MS](https://en.wikipedia.org/wiki/Mass_spectrometry),
etc. spectroscopy measurements.
More generally, any data that is recorded over a discretized variable, e.g. `absorbance = f(wavelength)`, stored as a vector of absorbance values for discrete wavelengths is suitable.


<!-- ---------------------------------------------------------------------- -->

## Documentation

There are two versions of **hyperSpec** online documentation:

a. for the [released version](https://r-hyperspec.github.io/hyperSpec/) and
b. for the [in-development version](https://r-hyperspec.github.io/hyperSpec/dev/) of **hyperSpec**.

The documentation of the other **`r-hyperspec`** family packages can be found at [r-hyperspec.github.io](https://r-hyperspec.github.io/).

<!-- ---------------------------------------------------------------------- -->

## Issues, Bug Reports and Feature Requests

Issues, bug reports and feature requests should go [here](https://github.com/r-hyperspec/hyperSpec/issues)!
<!-- ---------------------------------------------------------------------- -->


## Installation

### Install from CRAN

You can install the **released** version of **hyperSpec** from [CRAN](https://cran.r-project.org/package=hyperSpec) with:

```r
install.packages("hyperSpec")
```


### Install from CRAN-like Repository

The **recommended** way to install the in-development version:

```r
repos <- c("https://r-hyperspec.github.io/pkg-repo/", getOption("repos"))
install.packages("hyperSpec", repos = repos)
```

### Install from GitHub

<details>
<summary>Install from GitHub (details)</summary>

You can install the in-development version of the package from [GitHub](https://github.com/r-hyperspec/hyperSpec) too:

```r
if (!require(remotes)) {install.packages("remotes")}
remotes::install_github("r-hyperspec/hyperSpec")
```

**NOTE 1:**
Usually, "Windows" users need to download, install and properly configure **Rtools** (see [these instructions](https://cran.r-project.org/bin/windows/Rtools/)) to make the code above work.

**NOTE 2:**
This method will **not** install package's documentation (help pages and vignettes) into your computer.
So you can either use the [online documentation](https://r-hyperspec.github.io/) or build the package from source (see the next section).

</details>



### Install from Source

<details>
<summary>Install from Source (details)</summary>


1. From the **hyperSpec**'s GitHub [repository](https://github.com/r-hyperspec/hyperSpec):
    - If you use Git, `git clone` the branch of interest.
      You may need to fork it before cloning.
    - Or just chose the branch of interest (1 in Figure below), download a ZIP archive with the code (2, 3) and unzip it on your computer.  
![image](https://user-images.githubusercontent.com/12725868/89338263-ffa1dd00-d6a4-11ea-94c2-fa36ee026691.png)

2. Open the downloaded directory in RStudio (preferably, as an RStudio project).
    - The code below works correctly only if your current working directory coincides with the root of the repository, i.e., if it is in the directory that contains file `README.md`.
    - If you open RStudio project correctly (e.g., by clicking `project.Rproj` icon ![image](https://user-images.githubusercontent.com/12725868/89340903-26621280-d6a9-11ea-8299-0ec5e9cf7e3e.png) in the directory), then the working directory is set correctly by default.

3. In RStudio 'Console' window, run the code (provided below) to:
    a. Install packages **remotes** and **devtools**.
    b. Install **hyperSpec**'s dependencies.
    c. Create **hyperSpec**'s documentation.
    d. Install package **hyperSpec**.

```r
# Do not abort installation even if some packages are not available
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

# Install packages remotes and devtools
install.packages(c("remotes", "devtools"))

# Install hyperSpec's dependencies
remotes::install_deps(dependencies = TRUE)

# Create hyperSpec's documentation
devtools::document()

# Install package hyperSpec
devtools::install(build_vignettes = TRUE)
```

**NOTE 1:**
Usually, "Windows" users need to download, install and properly configure **Rtools** (see [these instructions](https://cran.r-project.org/bin/windows/Rtools/)) to make the code above work.

</details>


## For Developers

Developers can find information about automatic deployment from this repo to `pkg-repo` [here](https://github.com/r-hyperspec/pkg-repo) in `CONTRIBUTING.md`.
