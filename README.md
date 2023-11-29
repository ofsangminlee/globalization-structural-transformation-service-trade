# Globalization and Structural Transformation: The Role of Tradable Services

This repository contains the source code and data for replicating all
results in the paper:

  Lee, Sang Min (2023): “Globalization and Structural Transformation: The Role of Tradable Services”

## Structure

- `data` contains data used for the paper. 
	- `icio`: OECD Inter-Country Input-Output Database 2018 (`icio` data are uploaded with Git Large File Storage.)
	- `prices`: price data from OECD, UN, Groningen Growth and
      Development Centre, National Statistics of Republic of China
      (Taiwan)
	- `pwt`: Penn World Table 10.0
	- `BEA`: BEA/BLS KLEMS

- `src` contains all source code for replication.
    - `src_data`: `R` code for data cleaning; generating trade,
      input-output, and price data; and producing motivating plots 
	- `src_est_cal`: `R` code for estimating model parameters,
       calculating primitives (e.g, productivities and trade
       costs), and analyzing the patterns of globalization
	- `src_model`: `julia` code for 
      model simulation, `R` code for visualization of the results, `R`
      code for the analysis of productivity growth

- `output` contains outputs from running the source code
	- `cleaned_data`: cleaned data used for parameter estimation and inference
      of model primitives
	- `params_est_cal`: estimated parameters and calculated model
      primitives
	- `result_model`: simulated models
	
- `doc`: contains figures, tables, and numbers, generated through the
  source code, that are in the paper.

## Running the code

To clone the repository, at your desired location, type in the
terminal:

```
git clone
https://github.com/ofsangminlee/globalization-structural-transformation-service-trade.git
git lfs pull

```

It is important to run the second line, because ICIO data are
uploaded with Git Large File Storage. (Files for the ICIO data at the
repository are pointers, not actual files.)

The code is in `R` and `Julia`. 

For replication under the same version of open-source packages and
dependencies in `R` and `Julia`, follow the steps below.

Open an `R` prompt at the subfolder `src` so that your working
directory is `src`. Then type:
```
renv::load(".")
renv::restore()
```

Open a `Julia` prompt at the subfolder `src`. Then type:
```
using Pkg 
Pkg.activate(".")
Pkg.instantiate()
```

Now you can automatically generate all results by running Makefile
using `GNU Make`. If you are using a Unix-based OS (e.g., Linux,
macOS), in your terminal, run `make all`. This prompt will erase all
the results in the `output` and `doc` folders and regenerate them.

For Windows users, you can either download `GNU Make for Windows` and
follow the above steps. Alternatively, you can manually
run the code files in the order outlined in `Makefile`. `Makefile` can
be opened with text editors.
