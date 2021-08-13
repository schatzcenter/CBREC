**California Biomass Residue Emissions Characterization (C-BREC) Model**
===================================================================

## Life Cycle Assessment Module

This module is part of the [California Biomass Residue Emissions Characterization (C-BREC) Model](https://schatzcenter.org/cbrec). Additional methodological documentation can be found on the C-BREC Model website. The C-BREC model was originally developed as part of the [California Biopower Impact Project](https://schatzcenter.org/cbip/). The remote repository for the model can be found at [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC).

This module calculates life cycle gross and net:

* Greenhouse gas emissions
* Climate impact metrics: global warming potential and global temperature potential
* Criteria air pollutants

under different silvacultural treatments and biomass utilization scenarios. This module allows specification of the following key characteristics at the individual project level:

* Location of residue generation
* Type of forest treatment or harvest activity being conducted
* Baseline residue disposition (piled or scattered)
* Location of residue utilization
* Reference fate of unremoved biomass (prescribed burn or left in place)
* Supply chain characteristics

### Installation

Download or fork the repository from [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC). 

### File Structure

The file structure is shown in the following tree. 

```
File structure to be filled in
```
All input data required for running v1.2.1 can be downloaded via [![DOI](https://sandbox.zenodo.org/badge/DOI/10.5072/zenodo.899028.svg)](https://doi.org/10.5072/zenodo.899028)

### Prerequisites

This project was written in R using the Rstudio project framework.

#### Software requirements

To run the main scenario_emissions function, the following is required:

* A current version of R
* A current version of Rstudio
* The following R packages and their dependencies:
        - future.apply 
        - data.table
        - sf 
* The CBREC Rstudio project folder, including CBREC-Fire, CBREC-LCA, and associated inputs to each.
* At least 24 logical CPU cores at >2GHz each for a barely tolerable run time (month-scale), more is better.
* At least 160GB RAM, more is required for higher CPU core count.
* At least 2TB storage. This is not thoroughly tested. More storage is better.
        
Packages can be installed as follows:

```
install.packages("data.table")
```

Other packages and software are required to reproduce the entire project. Packages are loaded at the beginning of every script where possible. All scripts have a description header.

### Summary Overview

Text

### Usage

Text

### What it does

Text

### Output description

Text

### Versioning

We use [git](https://git-scm.com/) for version control on this project. For a complete history, see the [this repository](https://github.com/SchatzCenter/CBREC).

Code releases are automatically archived and assigned a DOI using [Zenodo](https://zenodo.org). The latest DOI badge is included on the repository page.

Release versions (on GitHub and Zenodo) will always correlate. Furthermore, the release version will always correlate with the framework documentation version this is included in this repository.

### Authors

* Andy Harris (Lead Author) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Jerome Qiriazi](https://github.com/jqiriazi) (Project Manager) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Max Blasdel](https://github.com/mxblsdl) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Micah Wright](https://github.com/wrightmicahc) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Chih-Wei Hsu](https://www.linkedin.com/in/chihweihsu/) - [Schatz Energy Research Center](https://schatzcenter.org)

### Acknowledgments

* <list acknowledgments>

* This project was funded by the California Energy Commission's (CEC) Electric Program Investment Charge (EPIC) program under contract agreement EPC-16-047. DISCLAIMER: This source code was prepared as the result of work sponsored by the California Energy Commission. It does not necessarily represent the views of the CEC, its employees, or the State of California. The CEC, the State of California, its employees, contractors, and subcontractors make no warrant, express or implied, and assume no legal liability for the information in this report; nor does any party represent that the uses of this information will not infringe upon privately owned rights. This report has not been approved or disapproved by the California Energy Commission, nor has the California Energy Commission passed upon the accuracy or adequacy of the information in this report.