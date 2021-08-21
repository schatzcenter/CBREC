**California Biomass Residue Emissions Characterization (C-BREC) Model**
===================================================================

The [California Biomass Residue Emissions Characterization (C-BREC) Model](https://schatzcenter.org/cbrec) is a life cycle assessment (LCA) framework specific to the use of California forest residues for electricity generation. It enables robust and transparent accounting of greenhouse gases and air pollutant emissions associated with residual woody biomass energy systems in California. The model allows specification of the following key characteristics at the individual project level:

* Location of residue generation
* Type of forest treatment or harvest activity being conducted
* Baseline residue disposition (piled or scattered)
* Location of residue utilization
* Reference fate of unremoved biomass (prescribed burn or left in place)
* Supply chain characteristics

Additional methodological documentation can be found on the C-BREC Model website. The C-BREC model was originally developed as part of the [California Biopower Impact Project](https://schatzcenter.org/cbip/) and maintained by the [Schatz Energy Research Center](https://schatzcenter.org/cbrec). The remote repository for this model can be found at [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC).

## Installation

Download or fork the repository from [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC). 

## File Structure

The file structure consists of the following: 

```
CBREC              # Main model directory
+-- CBREC-Fire     # Directory containing the [C-BREC Fire Module](https://github.com/schatzcenter/CBREC/tree/master/CBREC-Fire)
+-- CBREC-LCA      # Directory containing the [C-BREC LCA Module](https://github.com/schatzcenter/CBREC/tree/master/CBREC-LCA)
+-- common-inputs  # Directory containing .csv files of case definitions and cases to run used by both modules
+-- LICENSE        # Software license file (variant of the LBNL BSD-3 license)
+-- README.md      # This readme
```

See documentation for each module for additional information.

## Prerequisites

This project was written in R using the Rstudio project framework.

### Software requirements

To run the main scenario_emissions function, the following is required:

* A current version of R
* A current version of Rstudio
* The following R packages and their dependencies:
  - data.table
  - future.apply
  - optparse
  - raster
  - sf
  - tictoc
* The CBREC Rstudio project folder, including CBREC-Fire, CBREC-LCA, and associated inputs to each.
* The following minimum recommended resources for flexibility in spatial resolution and extent of the intended run. Note that required resources are dictated by the spatial resolution (the size of the individual project(s) being run) and the spatial extent (the fraction of the area of the State of California) chosen by the user. The following are based on running a large number of projects (10^4 or more) or a statewide run at low spatial resolution (50+ regions).
  * At least 24 logical CPU cores at >2GHz each. This results in a roughly 1 month runtime.
  * At least 160GB RAM, more is required for higher CPU core count.
  * At least 4TB storage to support both the total size of input data and total size of output data. This is not thoroughly tested. More storage is better.
        
Packages can be installed as follows:

```
install.packages("data.table")
```

### Input Data

A substantial amount of input data is necessary to run the C-BREC Model. Find a snapshot of this release at ??? which includes the input data necessary.

All static input data required to run both the C-BREC Fire Module and the C-BREC LCA Module can be downloaded via the Zenodo link provided above. The download preserves the required directory structure. It also includes the code associated with the release correlated to it. If downloading the input data for use in a later release, do not use the R scripts included in the data download.

## Summary Overview

The C-BREC Model is designed as two separate modules: C-BREC Fire Module and C-BREC LCA Module.
* C-BREC Fire: this module calculates emissions from in-field combustion of forest residues via prescribed burn and wildfire. The outputs from this module are used as inputs to the C-BREC LCA Module. This module is designed to only be run once, regardless of the number of times the C-BREC LCA module is run.
* C-BREC LCA: this module calculates gross emissions for a variety of cases as specified by the user, and net emissions for a variety of scenarios defined by combinations of individual cases. This module calculates both mass of greenhouse gas and criteria pollutants emitted, and normalized carbon dioxide equivalent (CO2e) using global warming potential and global temperature potential.

## Usage

First run the C-BREC Fire Module, then run the C-BREC LCA Module. See documentation for each module for additional details.

## Output description

See documentation for each module for details on outputs.

## Versioning

We use [git](https://git-scm.com/) for version control on this project. For a complete history, see the [this repository](https://github.com/SchatzCenter/CBREC).

Code releases are manually archived and assigned a DOI using [Zenodo](https://zenodo.org) in order to enable uploading of the large input data sets that cannot be stored on GitHub. The latest DOI badge is included on the repository page.

Release versions (on GitHub and Zenodo) will always correlate. Furthermore, the release version will always correlate with the framework documentation version this is included in this repository.

## Authors

* Andy Harris - [Schatz Energy Research Center](https://schatzcenter.org)
* [Micah Wright](https://github.com/wrightmicahc) - [Humboldt State University Department of Forestry & Wildland Resource](https://fwr.humboldt.edu/) and [Schatz Energy Research Center](https://schatzcenter.org)
* [Max Blasdel](https://github.com/mxblsdl) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Jerome Qiriazi](https://github.com/jqiriazi) (Project Manager) - [Schatz Energy Research Center](https://schatzcenter.org)
* Chih-Wei Hsu - [Schatz Energy Research Center](https://schatzcenter.org)
* [Jeff Kane](https://fwr.humboldt.edu/people/jeffrey-kane) (Corresponding Author) - [Humboldt State University Department of Forestry & Wildland Resource](https://fwr.humboldt.edu/)

## Acknowledgments

* The authors would like to thank the California Energy Commission's (CEC) Electric Program Investment Charge (EPIC) program for its support of this research under contract EPC-16-047, and in particular Commission Agreement Managers Katharina Gerber and David Stoms for their indispensable assistance. The C-BREC Model would not have been possible without the contributions and commitment of the following members and organizations:
  - Schatz Energy Research Center: Cassidy Barrientos, Carisse Geronimo, Sabrinna Rios-Romero, and Mark Severy
  - Natural Resource Spatial Informatics Group: Luke Rogers and Jeff Comnick
  - Consortium for Research on Renewable Industrial Materials: Elaine Oneil and Maureen Puettmann

* This research was much improved by the ongoing input and support of the members of our Technical Advisory Committee and in particular of its chair, Andrea Tuttle.

* The R consume scripts were originally translated directly into R from the original python code from [Consume 4.2](https://www.fs.fed.us/pnw/fera/fft/consumemodule.shtml), a component of [Fuel and Fire Tools](https://www.fs.fed.us/pnw/fera/fft/index.shtml).

* DISCLAIMER: This source code was prepared as the result of work sponsored by the California Energy Commission (CEC). It does not necessarily represent the views of the CEC, its employees, or the State of California. The CEC, the State of California, its employees, contractors, and subcontractors make no warrant, express or implied, and assume no legal liability for the information in this report; nor does any party represent that the uses of this information will not infringe upon privately owned rights. This report has not been approved or disapproved by the California Energy Commission, nor has the California Energy Commission passed upon the accuracy or adequacy of the information in this report.
