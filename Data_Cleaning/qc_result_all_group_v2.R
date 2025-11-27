library(dplyr)
library(readr)
library(stringr)
library(tidyr)

DATA_PATH <- "/scratch/grp/msc_appbio/DCDM/Group11/QC/qc_result_all.csv"
OUT_STD   <- "/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_v2.csv"
OUT_MAP   <- "/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_mapping_v2.csv"

df <- read_csv(DATA_PATH, show_col_types = FALSE)

# 簡單拆 IMPC 三碼（不區分大小寫，向量化版本）
# 定義完整的 parse_code3 function
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

# IMPC 三碼 → 新分組大類
code_map <- c(
  # ---------- IMAGE ----------
  MAA = "IMAGE",
  EMA = "IMAGE",
  ECH = "IMAGE",
  DXA = "IMAGE",
  XRY = "IMAGE",
  EML = "IMAGE",
  EMO = "IMAGE",
  
  # ---------- BRAIN ----------
  ACS = "BRAIN",
  ABR = "BRAIN",
  CSD = "BRAIN",
  FOR = "BRAIN",
  FEA = "BRAIN",
  YMZ = "BRAIN",
  LDT = "BRAIN",
  OFD = "BRAIN",
  SLW = "BRAIN",   # 若有 IMPC_SLW 也會歸到 BRAIN
  
  # ---------- WEIGHT ----------
  OWT = "WEIGHT",
  HWT = "WEIGHT",
  BWT = "WEIGHT",
  
  # ---------- MORPHOLOGY ----------
  GEO = "MORPHOLOGY",
  GEP = "MORPHOLOGY",
  GEM = "MORPHOLOGY",
  GPL = "MORPHOLOGY",
  GPM = "MORPHOLOGY",
  GPO = "MORPHOLOGY",
  GPP = "MORPHOLOGY",
  EYE = "MORPHOLOGY",
  EOL = "MORPHOLOGY",
  GEL = "MORPHOLOGY",
  
  # ---------- PATHOLOGY ----------
  PAT = "PATHOLOGY",
  HIS = "PATHOLOGY",
  BLK = "PATHOLOGY",
  IMM = "PATHOLOGY",
  
  # ---------- BLOOD ----------
  CBC = "BLOOD",
  HEM = "BLOOD",
  
  # ---------- COLONY MANAGEMENT & WELFARE ----------
  HOU = "COLONY MANAGEMENT & WELFARE",
  WEL = "COLONY MANAGEMENT & WELFARE",
  EVP = "COLONY MANAGEMENT & WELFARE",
  VIA = "COLONY MANAGEMENT & WELFARE",
  EVL = "COLONY MANAGEMENT & WELFARE",
  EVM = "COLONY MANAGEMENT & WELFARE",
  EVO = "COLONY MANAGEMENT & WELFARE",
  FER = "COLONY MANAGEMENT & WELFARE",
  EXD = "COLONY MANAGEMENT & WELFARE",
  
  # ---------- METABOLISM ----------
  CAL = "METABOLISM",
  IPG = "METABOLISM",
  INS = "METABOLISM",
  
  # ---------- ORGAN FUNCTION ----------
  CHL = "ORGAN FUNCTION",
  ECG = "ORGAN FUNCTION",
  GRS = "ORGAN FUNCTION",  # IMPC_GRS
  
  # ---------- GENE EXPRESSION ----------
  ELZ = "GENE EXPRESSION",
  ALZ = "GENE EXPRESSION"
)

# 非 IMPC 「中心_模組」前綴 → 新分組大類
prefix_map <- c(
  # ---------- IMAGE ----------
  "ESLIM_005" = "IMAGE",
  "M-G-P_005" = "IMAGE",
  "CCP_XRY"   = "IMAGE",
  
  # ---------- BRAIN ----------
  "ICS_FEA"    = "BRAIN",
  "JAX_LDT"    = "BRAIN",
  "JAX_SLW"    = "BRAIN",
  "JAX_OFD"    = "BRAIN",
  "KMPCLA_CSD" = "BRAIN",
  "MGP_CSD"    = "BRAIN",
  "KMPCLA_OFD" = "BRAIN",
  "ESLIM_012"  = "BRAIN",
  
  # ---------- WEIGHT ----------
  "ESLIM_022" = "WEIGHT",
  
  # ---------- MORPHOLOGY ----------
  "M-G-P_013" = "MORPHOLOGY",
  
  # ---------- BLOOD ----------
  "ESLIM_021"  = "BLOOD",
  "ESLIM_015"  = "BLOOD",
  "RBRCLA_HEM" = "BLOOD",
  "M-G-P_016"  = "BLOOD",
  "ESLIM_016"  = "BLOOD",
  "KMPCLA_HEM" = "BLOOD",
  "MGP_PBI"    = "BLOOD",
  
  # ---------- METABOLISM ----------
  "UCDLA_IPG" = "METABOLISM",
  
  # ---------- ORGAN FUNCTION ----------
  "CCP_LFO"    = "ORGAN FUNCTION",
  "HRWLLA_GRS" = "ORGAN FUNCTION",
  "ESLIM_009"  = "ORGAN FUNCTION"
)

# Parameter 名稱自動分類
param_name_to_group <- function(name) {
  name <- tolower(name)
  case_when(
    # 代謝相關
    str_detect(name, "cholesterol") ~ "METABOLISM",
    str_detect(name, "lactate dehydrogenase") ~ "METABOLISM",
    
    # 血液相關
    str_detect(name, "mean cell volume|platelet|haemoglobin|hemoglobin") ~ "BLOOD",
    
    # 體重 / 體組成
    str_detect(name, "weight|lean mass|bmc") ~ "WEIGHT",
    
    # 骨密度 / 骨區域（常來自 DXA 或 X-ray）
    str_detect(name, "bone mineral density|bmd|bone area") ~ "IMAGE",
    
    # 握力（肌肉 / 器官功能）
    str_detect(name, "grip strength|forelimb grip|hindlimb grip") ~ "ORGAN FUNCTION",
    
    # 視覺 / 眼睛
    str_detect(name, "retina|^eye$|vision") ~ "MORPHOLOGY",
    
    # 行為 / 腦功能
    str_detect(name, "pre-pulse inhibition|ppi|first response|time of") ~ "BRAIN",
    
    TRUE ~ NA_character_
  )
}

# 進行分類
df1 <- df %>%
  mutate(
    parameter_id_raw = parameter_id,
    code3        = parse_code3(parameter_id),
    param_prefix = extract_prefix(parameter_id),
    
    group_from_prefix = unname(prefix_map[param_prefix]),
    group_from_code3  = unname(code_map[code3]),
    group_from_name   = if ("parameter_name" %in% names(.)) {
      param_name_to_group(parameter_name)
    } else {
      NA_character_
    },
    
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


center <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_v2.csv")
center_all <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_mapping_v2.csv")
