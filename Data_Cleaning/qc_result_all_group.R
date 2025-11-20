library(dplyr)
library(readr)
library(stringr)
library(tidyr)

DATA_PATH <- "/scratch/grp/msc_appbio/DCDM/Group11/data/QC/qc_result_all.csv"
OUT_STD   <- "/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group.csv"
OUT_MAP   <- "/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_mapping.csv"

df <- read_csv(DATA_PATH, show_col_types = FALSE)

# 簡單拆 IMPC 三碼（不區分大小寫，向量化版本）
parse_code3 <- function(pid) {
  sapply(pid, function(p) {
    p <- as.character(p)
    p_upper <- toupper(p)
    if (str_detect(p_upper, "^IMPC_")) {
      code <- str_match(p_upper, "^IMPC_(\\w{3})_")[,2]
      return(ifelse(is.na(code), NA_character_, code))
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)
}

# 提取非 IMPC 的「中心_模組」前綴（統一轉大寫，向量化版本）
extract_prefix <- function(pid) {
  sapply(pid, function(p) {
    p_upper <- toupper(as.character(p))
    # 匹配各種格式：M-G-P_XXX, HRWLLA_XXX, 等
    m <- str_match(p_upper, "^([A-Z\\-]+_[A-Z0-9]{3})_")[,2]
    return(ifelse(is.na(m), NA_character_, m))
  }, USE.NAMES = FALSE)
}

# 創建基礎欄位並進行解析
df1 <- df %>% mutate(
  parameter_id_raw = parameter_id,
  code3 = parse_code3(parameter_id),
  param_prefix = extract_prefix(parameter_id)
)

# IMPC 三碼字典（新增 FOR, GEP, GPO, HWT, IMM）
code_map <- c(
  # Behavior/Neuro
  FEA = "Behavior/Fear conditioning",
  LDT = "Behavior/Anxiety (Light–Dark)",
  SLW = "Behavior/Sleep",
  OFD = "Behavior/Open-field",
  CSD = "Behavior/Startle",
  PPI = "Behavior/Pre-pulse inhibition",
  ACS = "Behavior/Neuro",
  # Hearing
  ABR = "Hearing/Auditory",
  # Metabolic
  IPG = "Metabolic/Glucose tolerance",
  # Hematology
  HEM = "Hematology",
  CBC = "Hematology",
  # Eye/Vision
  EYE = "Eye/Vision",
  # Neuromuscular
  GRS = "Neuromuscular/Grip strength",
  # Body composition
  DXA = "Growth/Body composition",
  LFO = "Growth/Weight",
  HWT = "Growth/Weight",              # Heart weight
  FO_ = "Growth/Weight",
  # Cardio
  ECG = "Cardio/ECG",
  ECH = "Cardio/Echo",
  # Skeletal
  XRY = "Skeletal/X-ray",
  RY_ = "Skeletal/X-ray",
  # Morphology/Imaging
  GEO = "Gross morphology",
  GEL = "Embryo/Imaging",
  # Immunology
  IMM = "Immunology",
  # Gene expression / Pathology
  GEP = "Gene expression/Pathology",
  GPO = "Gross pathology/Organ",
  FOR = "Behavior/Neuro"              # 假設是 forced swim 或類似行為測試
)

# 非 IMPC 前綴對應表（新增所有剩餘代碼）
prefix_map <- c(
  "ICS_FEA" = "Behavior/Fear conditioning",
  "CCP_LFO" = "Growth/Weight",
  "CCP_XRY" = "Skeletal/X-ray",
  # ESLIM 數字系列
  "ESLIM_005" = "Growth/Body composition",
  "ESLIM_009" = "Neuromuscular/Grip strength",
  "ESLIM_012" = "Behavior/Neuro",
  "ESLIM_015" = "Metabolic/Enzyme",
  "ESLIM_016" = "Hematology/Erythrocyte",
  "ESLIM_021" = "Metabolic/Lipid",
  "ESLIM_022" = "Growth/Body composition",
  # M-G-P 系列
  "M-G-P_005" = "Skeletal/Bone",
  "M-G-P_013" = "Eye/Vision",
  "M-G-P_016" = "Hematology/Platelets",
  # JAX 系列
  "JAX_LDT" = "Behavior/Anxiety (Light–Dark)",
  "JAX_OFD" = "Behavior/Open-field",
  "JAX_SLW" = "Behavior/Sleep",
  # KMPCLA 系列
  "KMPCLA_CSD" = "Behavior/Startle",
  "KMPCLA_HEM" = "Hematology",
  "KMPCLA_OFD" = "Behavior/Open-field",
  # MGP 系列
  "MGP_CSD" = "Behavior/Startle",
  "MGP_PBI" = "Behavior/Pre-pulse inhibition",
  # RBRCLA 系列
  "RBRCLA_HEM" = "Hematology",
  # UCDLA 系列
  "UCDLA_IPG" = "Metabolic/Glucose tolerance",
  # HRWLLA 系列
  "HRWLLA_GRS" = "Neuromuscular/Grip strength"
)

# Parameter 名稱自動分類
param_name_to_group <- function(name) {
  name <- tolower(name)
  case_when(
    str_detect(name, "cholesterol") ~ "Metabolic/Lipid",
    str_detect(name, "retina") ~ "Eye/Vision",
    str_detect(name, "lactate dehydrogenase") ~ "Metabolic/Enzyme",
    str_detect(name, "mean cell volume") ~ "Hematology/Erythrocyte",
    str_detect(name, "weight|lean mass|bmc") ~ "Growth/Body composition",
    str_detect(name, "grip strength|forelimb grip|hindlimb grip") ~ "Neuromuscular/Grip strength",
    str_detect(name, "platelet") ~ "Hematology/Platelets",
    str_detect(name, "bone mineral density|bmd") ~ "Skeletal/Bone",
    str_detect(name, "pre-pulse inhibition|ppi") ~ "Behavior/Neuro",
    str_detect(name, "first response|time of") ~ "Behavior/Neuro",
    str_detect(name, "^eye$|vision") ~ "Eye/Vision",
    str_detect(name, "bone area") ~ "Skeletal/Bone",
    TRUE ~ NA_character_
  )
}

# 進行分類
df1 <- df1 %>%
  mutate(
    group_from_prefix = unname(prefix_map[param_prefix]),
    group_from_code3 = unname(code_map[code3]),
    group_from_name = ifelse("parameter_name" %in% names(.), param_name_to_group(parameter_name), NA),
    final_group = coalesce(group_from_prefix, group_from_name, group_from_code3, "Other/Unknown")
  )

param_mapping <- df1 %>%
  distinct(parameter_id = parameter_id_raw, param_prefix, code3, 
           group_from_prefix, group_from_code3, group_from_name, final_group) %>%
  arrange(parameter_id)

write_csv(df1, OUT_STD)
write_csv(param_mapping, OUT_MAP)
message("Saved: ", OUT_STD)
message("Saved: ", OUT_MAP)


center <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group.csv")
center_all <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_mapping.csv")
