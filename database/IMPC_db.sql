DROP DATABASE IF EXISTS impc_db;
CREATE DATABASE impc_db CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE impc_db;


#Phase 1: Create all tables (without foreign keys first) 



# Table 1: genes - Gene information

CREATE TABLE genes (
    gene_accession_id VARCHAR(20) PRIMARY KEY, 
    gene_symbol VARCHAR(50) NOT NULL,
    INDEX idx_gene_symbol (gene_symbol)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 2: parameters - Parameter information

CREATE TABLE parameters (
    parameter_id VARCHAR(30) PRIMARY KEY, 
    parameter_name VARCHAR(150) NOT NULL,
    INDEX idx_parameter_name (parameter_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 3: parameter_descriptions - Parameter descriptions
CREATE TABLE parameter_descriptions (
    impc_parameter_orig_id VARCHAR(30),
    name VARCHAR(255), 
    description TEXT, 
    parameter_id VARCHAR(30),
    INDEX idx_impc_orig_id (impc_parameter_orig_id),
    INDEX idx_param_id (parameter_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 4: procedures - Experimental procedures
CREATE TABLE procedures (
    impc_parameter_orig_id VARCHAR(30) PRIMARY KEY,
    procedure_name TEXT,
    description TEXT,
    is_mandatory BOOLEAN,
    INDEX idx_procedure_name (procedure_name(100))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 5: parameter_groups - Parameter classification
CREATE TABLE parameter_groups (
    group_id INT AUTO_INCREMENT PRIMARY KEY, 
    group_name VARCHAR(100) NOT NULL, 
    parameter_id VARCHAR(30),
    INDEX idx_group_name (group_name),
    INDEX idx_param_id (parameter_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 6: results - Experimental results
CREATE TABLE results (
    id INT AUTO_INCREMENT PRIMARY KEY,  
    analysis_id VARCHAR(30), 
    gene_accession_id VARCHAR(20) NOT NULL, 
    mouse_strain VARCHAR(20) NOT NULL, 
    mouse_life_stage VARCHAR(30) NOT NULL, 
    parameter_id VARCHAR(30) NOT NULL, 
    pvalue DOUBLE,
    INDEX idx_gene (gene_accession_id),
    INDEX idx_parameter (parameter_id),
    INDEX idx_pvalue (pvalue)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 7: diseases - Disease information
CREATE TABLE diseases (
    disease_id VARCHAR(50), 
    disease_name VARCHAR(255),
    omim_id VARCHAR(30), 
    gene_accession_id VARCHAR(20),
    INDEX idx_gene_disease (gene_accession_id),
    INDEX idx_disease_id (disease_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

# Table 8: staging_raw - Staging table for data cleaning

CREATE TABLE staging_raw (
    gene_accession_id VARCHAR(30),
    gene_symbol VARCHAR(50),
    mouse_strain VARCHAR(20),
    mouse_life_stage VARCHAR(30),
    parameter_id VARCHAR(30),
    pvalue DOUBLE,
    parameter_name VARCHAR(150),
    analysis_id VARCHAR(30),
    analysis_id_status VARCHAR(10),
    gene_accession_id_status VARCHAR(10),
    gene_symbol_status VARCHAR(10),
    mouse_strain_status VARCHAR(10),
    mouse_life_stage_status VARCHAR(10),
    parameter_id_status VARCHAR(10),
    parameter_name_status VARCHAR(10),
    pvalue_status VARCHAR(10)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


# Step 2: Enable LOAD DATA and load data


SET GLOBAL local_infile = 1;

# A) Load parameter descriptions

LOAD DATA LOCAL INFILE '/scratch/grp/msc_appbio/Group11/data/IMPC_parameter_description_cleaned.csv'
INTO TABLE parameter_descriptions
FIELDS TERMINATED BY "," 
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(impc_parameter_orig_id, name, description, parameter_id);


## B) Load procedure information

LOAD DATA LOCAL INFILE '/scratch/grp/msc_appbio/Group11/data/IMPC_procedure_cleaned.csv'
INTO TABLE procedures
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(procedure_name, description, is_mandatory, impc_parameter_orig_id);


# C) Load raw experimental DATA

LOAD DATA LOCAL INFILE '/scratch/grp/msc_appbio/DCDM/Group11/data/RawData_QC/qc_result_all.csv'
INTO TABLE staging_raw
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;


# D) Clean gene IDs in staging_raw

ALTER TABLE staging_raw
    ADD COLUMN clean_gene_id VARCHAR(50);

UPDATE staging_raw
SET clean_gene_id = UPPER(
    TRIM(
        REPLACE(
            REPLACE(
                REPLACE(gene_accession_id, '\r', ''),
                '\n', ''),
            '\t', '')
    )
);

# E) Insert gene DATA

INSERT IGNORE INTO genes (gene_accession_id, gene_symbol)
SELECT
    clean_gene_id,
    MIN(gene_symbol) AS gene_symbol
FROM staging_raw
WHERE clean_gene_id IS NOT NULL
  AND clean_gene_id <> ''
GROUP BY clean_gene_id;


# F) Clean parameter IDs (enhanced version: clean all special characters)

UPDATE parameter_descriptions
SET parameter_id = UPPER(TRIM(REPLACE(REPLACE(REPLACE(parameter_id, '\r', ''), '\n', ''), '\t', ''))),
    impc_parameter_orig_id = UPPER(TRIM(REPLACE(REPLACE(REPLACE(impc_parameter_orig_id, '\r', ''), '\n', ''), '\t', '')));

## Clean impc_parameter_orig_id in procedures (enhanced version)
UPDATE procedures
SET impc_parameter_orig_id = UPPER(TRIM(REPLACE(REPLACE(REPLACE(impc_parameter_orig_id, '\r', ''), '\n', ''), '\t', '')));

# G) Insert parameter DATA

INSERT INTO parameters (parameter_id, parameter_name)
SELECT parameter_id, parameter_name
FROM (
    SELECT DISTINCT
        UPPER(TRIM(parameter_id)) AS parameter_id,
        name AS parameter_name
    FROM parameter_descriptions
    WHERE parameter_id IS NOT NULL
      AND parameter_id <> ''
    
    UNION
    
    SELECT DISTINCT
        UPPER(TRIM(parameter_id)) AS parameter_id,
        parameter_name
    FROM staging_raw
    WHERE parameter_id IS NOT NULL
      AND parameter_id <> ''
) combined
ON DUPLICATE KEY UPDATE parameter_name = VALUES(parameter_name);


## H) Create parameter_prefix and classify groups

ALTER TABLE parameters
    ADD COLUMN prefix VARCHAR(20);

UPDATE parameters
SET prefix = SUBSTRING_INDEX(parameter_id, '_', 2);

# I) Insert parameter groups

INSERT INTO parameter_groups (group_name, parameter_id)
SELECT 
    CASE
        WHEN parameter_id LIKE '%IMPC_CAL_%' THEN 'Metabolism'
        WHEN parameter_id LIKE '%IMPC_IPG_%' THEN 'Metabolism'
        WHEN parameter_id LIKE '%IMPC_INS_%' THEN 'Metabolism'
        WHEN parameter_id LIKE '%UCDLA_IPG_%' THEN 'Metabolism' 

        WHEN parameter_id LIKE '%CCP_LFO_%' THEN 'Organ Function'
        WHEN parameter_id LIKE '%IMPC_CHL_%' THEN 'Organ Function'
        WHEN parameter_id LIKE '%IMPC_ECG_%' THEN 'Organ Function'
        WHEN parameter_id LIKE '%ESLIM_009_%' THEN 'Organ Function'
        WHEN parameter_id LIKE '%HRWLLA_GRS_%' THEN 'Organ Function'
        WHEN parameter_id LIKE '%IMPC_GRS_%' THEN 'Organ Function'

        WHEN parameter_id LIKE '%IMPC_ELZ_%' THEN 'Gene Expression'
        WHEN parameter_id LIKE '%IMPC_ALZ_%' THEN 'Gene Expression'

        WHEN parameter_id LIKE '%IMPC_HOU_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_WEL_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_EVP_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_VIA_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_EVL_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_EVM_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_EVO_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_FER_%' THEN 'Colony Management and Welfare'
        WHEN parameter_id LIKE '%IMPC_EXD_%' THEN 'Colony Management and Welfare'

        WHEN parameter_id LIKE '%IMPC_CBC_%' THEN 'Blood'
        WHEN parameter_id LIKE '%IMPC_HEM_%' THEN 'Blood'
        WHEN parameter_id LIKE '%ESLIM_021_%' THEN 'Blood'
        WHEN parameter_id LIKE '%ESLIM_015_%' THEN 'Blood'
        WHEN parameter_id LIKE '%RBRCLA_HEM_%' THEN 'Blood'
        WHEN parameter_id LIKE '%M-G-P_016_%' THEN 'Blood'
        WHEN parameter_id LIKE '%ESLIM_016_%' THEN 'Blood'
        WHEN parameter_id LIKE '%KMPCLA_HEM_%' THEN 'Blood'
        WHEN parameter_id LIKE '%MGP_PBI_%' THEN 'Blood'

        WHEN parameter_id LIKE '%IMPC_IMM_%' THEN 'Pathology'
        WHEN parameter_id LIKE '%IMPC_PAT_%' THEN 'Pathology'
        WHEN parameter_id LIKE '%IMPC_HIS_%' THEN 'Pathology'
        WHEN parameter_id LIKE '%IMPC_BLK_%' THEN 'Pathology'

        WHEN parameter_id LIKE '%IMPC_GEO_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GEP_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GEM_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GPL_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_EYE_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GPM_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GPO_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GPP_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_EOL_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%IMPC_GEL_%' THEN 'Morphology'
        WHEN parameter_id LIKE '%M-G-P_013_%' THEN 'Morphology'

        WHEN parameter_id LIKE '%IMPC_OWT_%' THEN 'Weight'
        WHEN parameter_id LIKE '%IMPC_HWT_%' THEN 'Weight'
        WHEN parameter_id LIKE '%IMPC_BWT_%' THEN 'Weight'
        WHEN parameter_id LIKE '%ESLIM_022_%' THEN 'Weight'
        
        WHEN parameter_id LIKE '%IMPC_ACS_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_ABR_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_CSD_%' THEN 'Brain'
        WHEN parameter_id LIKE '%KMPCLA_CSD_%' THEN 'Brain'
        WHEN parameter_id LIKE '%MGP_CSD_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_FOR_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_FEA_%' THEN 'Brain'
        WHEN parameter_id LIKE '%ICS_FEA_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_YMZ_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_LDT_%' THEN 'Brain'
        WHEN parameter_id LIKE '%IMPC_OFD_%' THEN 'Brain'
        WHEN parameter_id LIKE '%JAX_SLW_%' THEN 'Brain'
        WHEN parameter_id LIKE '%JAX_LDT_%' THEN 'Brain'
        WHEN parameter_id LIKE '%KMPCLA_OFD_%' THEN 'Brain'
        WHEN parameter_id LIKE '%JAX_OFD_%' THEN 'Brain'
        WHEN parameter_id LIKE '%ESLIM_012_%' THEN 'Brain' 
        
        WHEN parameter_id LIKE '%IMPC_MAA_%' THEN 'Image'
        WHEN parameter_id LIKE '%IMPC_EMA_%' THEN 'Image'
        WHEN parameter_id LIKE '%IMPC_ECH_%' THEN 'Image'
        WHEN parameter_id LIKE '%IMPC_DXA_%' THEN 'Image'
        WHEN parameter_id LIKE '%ESLIM_005_%' THEN 'Image'
        WHEN parameter_id LIKE '%M-G-P_005_%' THEN 'Image'
        WHEN parameter_id LIKE '%IMPC_XRY_%' THEN 'Image'
        WHEN parameter_id LIKE '%CCP_XRY_%' THEN 'Image'
        WHEN parameter_id LIKE '%IMPC_EML_%' THEN 'Image'
        WHEN parameter_id LIKE '%IMPC_EMO_%' THEN 'Image'

        ELSE 'Other'
    END AS group_name,
    parameter_id
FROM parameters;

# J) Load disease information

LOAD DATA LOCAL INFILE '/scratch/grp/msc_appbio/DCDM/Group11/data/cleaned_disease_table.csv'
INTO TABLE diseases
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(disease_id, disease_name, omim_id, gene_accession_id);


## Modification 1: Keep all disease data even if gene is not in genes TABLE

DELETE d
FROM diseases d
LEFT JOIN genes g ON d.gene_accession_id = g.gene_accession_id
WHERE d.gene_accession_id IS NOT NULL
  AND g.gene_accession_id IS NULL;


K)  Insert experimental results
INSERT INTO results (
    analysis_id,
    gene_accession_id,
    mouse_strain,
    mouse_life_stage,
    parameter_id,
    pvalue
)
SELECT
    analysis_id,
    UPPER(
        TRIM(
            REPLACE(
                REPLACE(
                    REPLACE(gene_accession_id, '\r', ''),
                    '\n', ''),
                '\t', '')
        )
    ) AS clean_gene_id,
    mouse_strain,
    mouse_life_stage,
    UPPER(TRIM(parameter_id)) AS clean_param_id,
    pvalue
FROM staging_raw
WHERE analysis_id IS NOT NULL
  AND pvalue > 0;


# STEP 3: Complete data validation and diagnostics



SELECT 'genes' as table_name, COUNT(*) as count FROM genes
UNION ALL
SELECT 'parameters', COUNT(*) FROM parameters
UNION ALL
SELECT 'parameter_descriptions', COUNT(*) FROM parameter_descriptions
UNION ALL
SELECT 'procedures', COUNT(*) FROM procedures
UNION ALL
SELECT 'results', COUNT(*) FROM results
UNION ALL
SELECT 'diseases', COUNT(*) FROM diseases
UNION ALL
SELECT 'parameter_groups', COUNT(*) FROM parameter_groups;


SELECT 
    'procedures total' as type,
    COUNT(DISTINCT impc_parameter_orig_id) as count
FROM procedures
UNION ALL
SELECT 
    'parameter_descriptions total' as type,
    COUNT(DISTINCT impc_parameter_orig_id) as count
FROM parameter_descriptions
UNION ALL
SELECT 
    'Match (can connect)' as type,
    COUNT(DISTINCT pr.impc_parameter_orig_id) as count
FROM procedures pr
INNER JOIN parameter_descriptions pd ON pr.impc_parameter_orig_id = pd.impc_parameter_orig_id;


# STEP 4: Add foreign keys (modified for LEFT JOIN compatibility)


ALTER TABLE results
ADD CONSTRAINT fk_results_genes FOREIGN KEY (gene_accession_id) REFERENCES genes(gene_accession_id),
ADD CONSTRAINT fk_results_parameters FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id);

ALTER TABLE diseases
ADD CONSTRAINT fk_diseases_genes FOREIGN KEY (gene_accession_id) REFERENCES genes(gene_accession_id);

ALTER TABLE parameter_descriptions
ADD CONSTRAINT fk_pd_parameters FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id) ON DELETE SET NULL;

ALTER TABLE parameter_groups
ADD CONSTRAINT fk_pg_parameters FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id);

ALTER TABLE procedures
ADD CONSTRAINT fk_procedures_pd FOREIGN KEY (impc_parameter_orig_id) 
REFERENCES parameter_descriptions(impc_parameter_orig_id) ON DELETE CASCADE;


# STEP 5: Query Section - Analysis of Target Genes

# Query 1: Gene accession IDs of the 4 target genes
# Outputs the gene accession ID associated with each query gene
SELECT *
FROM genes
WHERE gene_symbol IN ('Plk5', 'Col28a1', 'Dok7', 'Slfn5');

## Query 2: Gene-phenotype associations with p-values
# Retrieves the 4 target genes and shows how significant each gene-phenotype association IS

SELECT
    g.gene_symbol,
    r.parameter_id,
    p.parameter_name,
    r.pvalue
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
WHERE g.gene_symbol IN ('Plk5', 'Col28a1', 'Dok7', 'Slfn5')
ORDER BY g.gene_symbol, r.pvalue ASC;

# Query 3: Significant phenotype-gene associations (p-value < 0.05)
# Shows the most significant (p-value < 0.05) phenotype-gene associations with each query gene
# Slfn5 and Plk5 do not have any significant associations with any phenotype

SELECT 
    g.gene_symbol,
    p.parameter_name,
    r.pvalue
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
WHERE g.gene_symbol IN ('Plk5','Col28a1','Dok7','Slfn5')
  AND r.pvalue < 0.05
ORDER BY g.gene_symbol, r.pvalue ASC;

# Query 4: Significant associations with parameter groupings
# Shows the most significant (p-value < 0.05) phenotype-gene associations as well as parameter group classification

SELECT 
    g.gene_symbol,
    pg.group_name,
    p.parameter_name,
    r.pvalue
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
LEFT JOIN parameter_groups pg USING (parameter_id)
WHERE g.gene_symbol IN ('Plk5','Col28a1','Dok7','Slfn5')
  AND r.pvalue < 0.05
ORDER BY g.gene_symbol, pg.group_name, r.pvalue;

# Query 5: Genes in specific parameter groupings
# Shows which of the query genes occur in required parameter groupings: Brain, Image and Weight
# Note: Only Plk5 and Slfn5 have these parameter groupings

SELECT 
    g.gene_symbol,
    pg.group_name,
    COUNT(*) AS n_parameters
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameter_groups pg USING (parameter_id)
WHERE g.gene_symbol IN ('Plk5','Col28a1','Dok7','Slfn5')
  AND pg.group_name IN ('Brain', 'Weight', 'Image')
GROUP BY g.gene_symbol, pg.group_name
ORDER BY g.gene_symbol, pg.group_name;


# STEP 6: Full Metadata Queries for Target Genes


# QUERY  6: Full metadata of gene Plk5

SELECT 
    g.gene_symbol,
    r.gene_accession_id,
    r.parameter_id,
    p.parameter_name,
    pd.description AS parameter_description,
    pg.group_name AS parameter_group,
    r.pvalue,
    pr.procedure_name,
    pr.description AS procedure_description,
    COALESCE(d.disease_name, 'No association') AS disease_name,
    COALESCE(d.omim_id, 'None') AS omim_id
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
LEFT JOIN parameter_groups pg USING (parameter_id)
LEFT JOIN parameter_descriptions pd USING (parameter_id)
LEFT JOIN procedures pr USING (impc_parameter_orig_id)
LEFT JOIN diseases d USING (gene_accession_id)
WHERE g.gene_symbol = 'Plk5'
ORDER BY r.pvalue ASC;

# QUERY 7: Full metadata of gene Col28a1

SELECT 
    g.gene_symbol,
    r.gene_accession_id,
    r.parameter_id,
    p.parameter_name,
    pd.description AS parameter_description,
    pg.group_name AS parameter_group,
    r.pvalue,
    pr.procedure_name,
    pr.description AS procedure_description,
    COALESCE(d.disease_name, 'No association') AS disease_name,
    COALESCE(d.omim_id, 'None') AS omim_id
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
LEFT JOIN parameter_groups pg USING (parameter_id)
LEFT JOIN parameter_descriptions pd USING (parameter_id)
LEFT JOIN procedures pr USING (impc_parameter_orig_id)
LEFT JOIN diseases d USING (gene_accession_id)
WHERE g.gene_symbol = 'Col28a1'
ORDER BY r.pvalue ASC;

# Query 8: Full metadata of gene Dok7

SELECT 
    g.gene_symbol,
    r.gene_accession_id,
    r.parameter_id,
    p.parameter_name,
    pd.description AS parameter_description,
    pg.group_name AS parameter_group,
    r.pvalue,
    pr.procedure_name,
    pr.description AS procedure_description,
    COALESCE(d.disease_name, 'No association') AS disease_name,
    COALESCE(d.omim_id, 'None') AS omim_id
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
LEFT JOIN parameter_groups pg USING (parameter_id)
LEFT JOIN parameter_descriptions pd USING (parameter_id)
LEFT JOIN procedures pr USING (impc_parameter_orig_id)
LEFT JOIN diseases d USING (gene_accession_id)
WHERE g.gene_symbol = 'Dok7'
ORDER BY r.pvalue ASC;

# Query 9: Full metadata of gene Slfn5
SELECT 
    g.gene_symbol,
    r.gene_accession_id,
    r.parameter_id,
    p.parameter_name,
    pd.description AS parameter_description,
    pg.group_name AS parameter_group,
    r.pvalue,
    pr.procedure_name,
    pr.description AS procedure_description,
    pr.impc_parameter_orig_id,
    COALESCE(d.disease_name, 'No association') AS disease_name,
    COALESCE(d.omim_id, 'None') AS omim_id
FROM results r
JOIN genes g USING (gene_accession_id)
JOIN parameters p USING (parameter_id)
LEFT JOIN parameter_groups pg USING (parameter_id)
LEFT JOIN parameter_descriptions pd USING (parameter_id)
LEFT JOIN procedures pr USING (impc_parameter_orig_id)
LEFT JOIN diseases d USING (gene_accession_id)
WHERE g.gene_symbol = 'Slfn5'
ORDER BY r.pvalue ASC;

