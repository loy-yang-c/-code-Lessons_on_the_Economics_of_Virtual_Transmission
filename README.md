# Lessons_on_the_Economics_of_VT: Data Analysis, Statistics, Modelling and Results

## Requirements
### Data 
Download and unzip study data at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24148227.svg)](https://doi.org/10.5281/zenodo.24148227) 
and [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24148429.svg)](https://doi.org/10.5281/zenodo.24148429).
### Data Analysis
R 4.5.2 (or equivalent)  
R Studio IDE (optional)  
Tidyverse package (run `install.packages("tidyverse")`)  
Arrow package (run `install.packages("arrow")`)
### Counterfactual Simulation
Python 3.11
Gurobipy 13.0 and licence. We use an WLS Compute Server licence and ARDC Nectar Research Cloud virtual machines for simulation.
### Git
GitHub
https://github.com/loy-yang-c/Monash-NEMPY  
https://github.com/loy-yang-c/nempy-backend

## Overview
Download and unpack data → `data_analysis.R` → Alternative Scenario Simulation → `scenario_analysis.R`

## Raw Data
Raw data is scrapped from AEMO's publicly available Electricity Data Model and NEMWeb. For convenience, we provide this raw data through Zenodo.

## Secondary Data Creation
### vbb_sips_constraint_groups.csv
Map VBB VT constraints from ./raw_data/genericconstraintrhs_ALL_VBB_SIPS.csv to groups. Multiple GENCONIDs map to the same constraint depending on system state.
### genconid_VBB_SIPS_mapping.csv
For VBB, VT and non-VT constraints are duplicated. The inactivee constraint is swamped. We map the VT and non-VT versions in this file, by finding the equivalent 
non-VT constraint in ./raw_data/genericconstraintrhs_SPDID_CONTAINS_SIP.csv for each GENCONID in ./raw_data/genericconstraintrhs_ALL_VBB_SIPS.csv
### Simulation Re-Dispatch Intervals
historical_coi_XXXX.csv: superset of all scenario intervals being re-dispatched.  
vni_contract_binding_XXXX.csv: set of historic intervals when a v.1 constraint was binding.  
arbe_elml_binding_XXXX.csv: set of historic intervals when a v.2 constraint was binding.  
vni_voltage_binding_XXXX.csv: set of historic intervals when a v.3 constraint was binding.  
wsb_thermal_binding_XXXX.csv: set of historic intervals when a w.1 and w.2 constraint was binding.  
XXXX partitions the set by period: SIPS1 (VBB: November 2021-March 2022), SIPS2 (VBB: November 2022-March 2023), SIPS3 (VBB: November 2023-March 2024),
SIPS4 (VBB: November 2024-March 2025), SIPS5 (VBB: November 2025-January 2026, WSB: July 2025-January 2026).  
./data_analysis.R
### dudetail_summary_technology.csv
We create a technology mapping for each DUID in ./raw_data/dudetailsummary.csv.

## Table 2
### Constraint formulas:
See right-hand-side constraint formulations in, WSB: ./raw_data/genericconstraintrhs_wtah.csv, VBB: ./raw_data/genericconstraintrhs_vbb.xlsx and 
./raw_data/genericequationrhs_VIC.csv
### Observed values:
./data_analysis.R

## Figures 2, 3, 5, 9
./data_analysis.R

## Quoted Statistics: Lessons 2, 3
./data_analysis.R

## Table 6
See ./raw_data/genericconstraintrhs_V_NIL_ELML_BAML2.csv.

## Table 7
See right-hand-side constraint formulations in, WSB: ./raw_data/genericconstraintrhs_wtah.csv, VBB: ./raw_data/genericconstraintrhs_vbb.xlsx and 
./raw_data/genericequationrhs_VIC.csv.

## Table 8
See mapping in ./secondary_data/genconid_VBB_SIPS_mapping.csv.

## Alternative Scenario Simulation
### Step 1
Generate Simulation Re-Dispatch Intervals in ./data_analysis.R
### Step 2
Create and load a virtual environment for Python 3.11. Windows Command Prompt:  
`python -m venv .venv` 
`.venv\Scripts\activate.bat`
### Step 3
Clone Monash-NEMPY and nempy-backend repositories. Windows Command Prompt:  
`pip install <repo filepath>`
### Step 4
From step 1, we have generated intervals for each scenario in which the market requires re-dispatch. Copy the 16 vni_contract_binding_XXXX.csv, 
arbe_elml_binding_XXXX.csv, vni_voltage_binding_XXXX.csv and wsb_thermal_binding_XXXX.csv from ./secondary_data/ to /Monash-NEMPY/input/.
### Step 5 
NEMPY simulation requires a scraped NEMWeb data files (partitioned into daily .loaded files) and an SQLite .db file. 
The data is scenario-agnostic and need only be scraped once for each period. /Monash-NEMPY/input_data.py
scrapes and stores the required data for simulation. Set `START_DATE` and `END_DATE`, and `DATA_PATH` and `MMS_DB_WRITE_PATH` in /Monash-NEMPY/vars.py and then 
run the script (Windows Command Prompt: `python input_data.py`). We also provide a `MMS_DB_WRITE_APNDX` which allows the user to save multiple .db objects for 
each simulation period. We maintain different .db objects in a cloud environment for better workflow.   

Run each of the scenario-period simulations. Copy the relevant period .db object to the `MMS_DB_PATH` and rename the object to remove the appendix.
Edit /Monash-NEMPY/vars.py and save based on the following table. Execute /Monash-NEMPY/run_model.py 
(Windows Command Prompt: `python run_model.py`). Iterate for each row in the following table:  
| Scenario | CUSTOM_INTERVALS  | NEMPY_MODEL | <PERIODS> |
|----------|-------------------|-------------|-----------|
| v.1      |  "./input/vni_contract_binding_<PERIODS>.csv" | "vbb_sips_on-p.py" | SIPS1-SIPS5 |
| v.2      |  "./input/arbe_elml_binding_<PERIODS>.csv" | "vni_arbe_elml_relaxation-p.py" | SIPS1-SIPS5 |
| v.3      |  "./input/vni_voltage_binding_<PERIODS>.csv" | "vni_voltage_100-p.py" | SIPS1-SIPS5 |
| w.1      |  "./input/wsb_thermal_binding_<PERIODS>.csv" | "wtah_sips_on-p.py" | SIPS5 |
| w.2      |  "./input/wsb_thermal_binding_<PERIODS>.csv" | "wtah_350_flat-p.py" | SIPS5 |
| -        |  "./input/historical_coi_<PERIODS>.csv" | "historical-p_re-run.py" | SIPS5 |

Provide a `RESULTS_PATH` to save simulation .parquet files. The program will not identify which period is being solved, so ensure that the path provided in
`RESULTS_PATH` is appended by the period, for example, ".../results/SIPS1/". With both input and simulation output data, we recommend storing this locally and outside of the 
repository given the volume of data. `N_JOBS` is the number of concurrent intervals that are solved in parallel, which we set to the number of cpu cores. 
Fields `START_DATE` and `END_DATE` are ignored when `CUSTOM_INTERVALS` are provided. The applicable market price cap and floor (`MPC` and `MPF`) need to be 
provided for each simulation period. Refer to the following table:  
| Scenarios | Period | MPC  | MPF | START_DATE | END_DATE |
|-----------|--------|------|-----|------------|----------|
| v.1-3 | SIPS1  | 15100 | -1000 | "2021-11-29" | "2022-04-02" |
| v.1-3 | SIPS2  | 15500 | -1000 | "2022-11-01" | "2023-04-02" |
| v.1-3 | SIPS3  | 16600 | -1000 | "2023-11-01" | "2024-04-02" |
| v.1-3 | SIPS4  | 17500 | -1000 | "2024-11-01" | "2025-04-02" |
| v.1-3 | SIPS5  | 20300 | -1000 | "2025-11-01" | "2026-02-02" |
| w.1-2 | SIPS5  | 20300 | -1000 | "2025-07-31" | "2026-02-02" |

### Step 6
Lastly, we require historical constraint violations. Set `START_DATE`, `END_DATE` and `DATA_PATH` and `MMS_DB_WRITE_PATH`, 
and using the same `RESULTS_PATH` paths as simulation, run /Monash-NEMPY/scrape_CVP.py (Windows Command Prompt: `python scrape_CVP.py`).
### Step 7
Set up a results folder structure for .parquet outputs. This should follow ./results/<PERIOD>/<SCENARIO>/. Copy Step 5 .parquet files into these folders by the
appropriate period and scenario. Copy CVP .parquet files into ./results/<PERIOD>/cvp/. ./results/ is now your `parquet_path`.
### Step 8
Results are stored as .parquet files and grouped into energy prices, FCAS prices, constraint marginal values, unit dispatch, interconnector flows
and linear objective function value. In this step we use ./convert_simulation_parquet.R to convert results to an .RDS file for data analysis. Ensure that results are
saved in the following format ".../<PERIOD>/<SCENARIO>/..." and set the `parquet_path` value at the top of the R script. The script will output 
./secondary_data/results.rds.

## Figures 4, 10
./simulation_analysis.R

## Tables 3, 4, 5, 9, 10
./simulation_analysis.R

## Quoted Statistics: Counterfactual Virtual Transmission Scenarios, Lesson 5, Methods
./simulation_analysis.R

