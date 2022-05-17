# ==========================================================================
# Objective:
#' + Calculate profit-deficits and RMSE of EONR 
#' + Calculate RMSE of yield predictions for RF, BRF and CNN
#'      + For CNN results, estimate  EONR in this codes
#' + Finally, put together those results into one data set
# ==========================================================================

#/*----------------------------------*/
#' ## Preparation 
#/*----------------------------------*/
library(here)
library(data.table)
library(RColorBrewer)
library(patchwork)
library(ggplot2)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggpubr)
library(here)

# === Source Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))
source(here("Codes/0_2_functions_main_sim_ALF.R"))

# === Prices ===#
pCorn <- price_table[2, pCorn]
pN <- price_table[2, pN]


# === Function for RMSE Calculation === #
rmse_general <-function(preds,actual){ 
  sqrt(mean((actual - preds)^2))
}

# === Load Training and Testing Data Sets for Evaluation === #
reg_data_all <- readRDS(here("Data/reg_data.rds"))
test_data_all <- readRDS(here("Data/test_data.rds"))

source_dt <- 
    bind_rows(reg_data_all, test_data_all, .id = "type") %>%
    .[padding==1, .(sim, type, unique_subplot_id, alpha, beta, ymax, rate, yield, opt_N)] %>%
    .[,type := case_when(type == "1" ~ "train", type == "2" ~ "test")]

# === Unique_subplot_id in a field === #
subplots_infiled <- source_dt[,unique_subplot_id] %>% unique()


# ==========================================================================
# 1. Results of GAM, RF, BRF and CF
# ==========================================================================

# /*=================================================*/
#' # Load and organize the Forest results
# /*=================================================*/
# === Load Forest Results === #
ls_res_forest <- 
    list.files(
        path = here("Data/Forest_rawRes"), 
        full.names = TRUE
    ) %>%
    gtools::mixedsort(decreasing = TRUE)


forest_simRes_all <- 
    lapply(ls_res_forest, readRDS) %>%
    rbindlist(., idcol = "Model") %>%
    .[,Model := case_when(
        Model == 1 ~ "aby",
        Model == 2 ~ "abytt",
        Model == 3 ~ "aabbyy",
        Model == 4 ~ "aabbyytt"
        )] %>%
    .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]%>%
    .[, Method := factor(Method, levels = c("RF_ranger","RF_split", "XGBRF","RF", "BRF", "XCF_base", "CF_base"))] %>%
    .[, .(type, sim, Model, Method, unique_subplot_id, pred_yield, opt_N_hat, yield, opt_N)]


# /*=================================================*/
#' # RMSE of EONRs and Profit-deficits calculation
# /*=================================================*/
forest_optN_piLoss <- 
    source_dt[, !c("rate", "yield", "opt_N")] %>%
    forest_simRes_all[, on=c("sim", "type", "unique_subplot_id")] %>%
    .[,`:=`(
        max_pi = pCorn*gen_yield_MB(ymax, alpha, beta, opt_N) - pN*opt_N,
        pi =  pCorn*gen_yield_MB(ymax, alpha, beta, opt_N_hat) - pN*opt_N_hat)] %>%
    .[, pi_loss := max_pi - pi]

# === Summarize by Simulation Round === #
forest_summary_bySim <- 
    forest_optN_piLoss %>%
    .[,.(
        rmse_optN = rmse_general(opt_N_hat, opt_N),
        pi_loss = mean(pi_loss),
        rmse_y = rmse_general(pred_yield, yield)
    ), by= .(sim, type, Method, Model)]

# === Summarize by Method and Model === #
forest_summary_bySim %>%
    .[, .(
        mean_rmse_optN = mean(rmse_optN),
        mean_pi_loss = mean(pi_loss),
        mean_rmse_y = mean(rmse_y)
        ), by=.(type, Method, Model)] %>%
    .[order(type, Method)]


# ==========================================================================
# Merge Forest results and CNN results
# ==========================================================================
# === Merge === #
allML_summary_bySim <-
  forest_summary_bySim %>%
  .[, Method := factor(Method, levels = c("RF_ranger", "RF_split","XGBRF", "RF", "BRF", "CF_base"))] %>%
  .[, Model := factor(Model, levels = c("aby", "abytt", "aabbyy", "aabbyytt"))]

saveRDS(allML_summary_bySim, here("Data/allML_summary_bySim.rds"))



