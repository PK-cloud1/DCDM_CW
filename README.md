# Group 11 DCDM

### Overview 
* This repository hosts the R and MySQL scripts, databases, and R Shiny visualisations created by Group 11 for the DCDM coursework  
* All data processing, including analysis and cleaning, was performed within the King’s College London CREATE HPC environment to ensure reproducibility

### Requirements
* R ≥ 4.3.0 with packages: tidyverse, shiny
* MySQL ≥ 8.0
* Access to King's College London CREATE HPC environment

### Usage
1. Navigate to the 'Data Cleaning' folder and run scripts in R/Python.
2. Load the cleaned data into MySQL using scripts in 'Database'.
3. Launch R Shiny apps in 'Visualisation' for interactive exploration.

### Project Structure
```text
- Data
- Data Cleaning
- Database
- Visualisation
```
### [Data Cleaning](./Data_Cleaning)
* Software used: R and Python  
* The 'Scripts' folder contains code for data cleaning and organisation.  
* The 'Data' folder contains the cleaned raw datasets.  

### [Database Creation](./Database) 
* Software used: MySQL  
* The 'Database' folder contains MySQL scripts created for database management.  

### [Data Visualisation](./Visualisation)
* Software used: R (R Shiny)  
* Three interactive visualisations were developed in response to collaborator requests.  

### [Supporting Data](./data)
* **IMPC_parameter_description.csv** – metadata about the experimental parameters used in IMPC studies.  
* **Disease_information.csv** – mappings between phenotypic traits and known human diseases.  
* **IMPC_procedure.csv** – documentation of the experimental procedures carried out in IMPC pipelines.


#### Contributors
* Samara Banday
* Chia-Yu (Samantha) Tu
* Radhika Shaunak
* Phoebe Kusi-Yeboah
* Michael Tuft
