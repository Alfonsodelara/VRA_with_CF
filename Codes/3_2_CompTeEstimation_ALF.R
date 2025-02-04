# Description: This code file estimates treatment effect by treatment type and ML method under the "aabbyytt" scenario in one of the one 
# thousand simulations rounds. The output data is used to create Figure 6: "True treatment effects vs estimated treatment 
# effects (scenario: aabbyytt)"
# Final output: dt_TEcomparison.rds

#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/
library(here)
library(ggplot2)
library(grf)
library(sf)
library(here)
library(data.table)
library(tidyverse)
library(ggthemes)
library(future.apply)
library(xgboost)
library(ranger)

# === Load Functions === #
source(here("Codes/0_1_functions_gen_analysis_data.R"))
source(here("Codes/0_2_functions_main_sim_ALF.R"))

# === Load Data Sets === # 
reg_data_all <- readRDS(here("Data/reg_data.rds"))
test_data_all <- readRDS(here("Data/test_data.rds"))

# === Pick a single simulation round === #
x = 1
reg_data_sample <- reg_data_all[sim==x & padding==1,]
test_data_sample <- test_data_all[sim==x & padding==1,]
N_levels <- reg_data_sample$rate%>%unique()%>%sort()

subplots_infiled <- reg_data_sample[,unique_subplot_id] %>% unique()

# --- true treatment effect data set --- #
true_te_dt <- 
	test_data_sample %>%
	.[,.(sim, unique_subplot_id, alpha, beta, ymax)]%>%
	.[rep(1:nrow(.), each = length(N_levels)), ] %>%
  	.[, rate := rep(N_levels, nrow(.) / length(N_levels))] %>%
  	.[, det_yield := gen_yield_MB(ymax, alpha, beta, rate)] %>%
	.[, yield_base := .SD[rate==min(rate), det_yield], by = .(unique_subplot_id)] %>%
  	.[, true_te_base := det_yield - yield_base] %>%
  	.[, .(sim, unique_subplot_id, rate, true_te_base)]


# /*================================================================*/
#' # (1) Treatment Effect Calculation by CF-base, RF, BRF
# /*================================================================*/
# === all the cases to be considered === #
var_ls_variations <- list(
    c("alpha1", "alpha2", "beta1", "beta2", "ymax1", "ymax2", "theta_1", "theta_2")
    )

te_case_data <- expand.grid(
    var_ls = var_ls_variations,
    Method = c("CF_base", "XCF_base")#, "RF_split", "XGBRF","BRF", "RF")    
  ) %>%
  tibble()


# === set up for parallel computations === #
#plan(multicore, workers = availableCores()-2)    multicore not supported on windows
plan(multisession)
options(future.globals.maxSize= 850*1024^2)
set.seed(1378)


# === treatment effect calculation === #
forest_te_dt <- 
	te_case_data %>%
	mutate(
		te_data = future_lapply(
			seq_len(nrow(.)),
			function(x) {
				get_te_dt(
				reg_data = reg_data_sample,
            	test_data = test_data_sample,
            	var_ls = .$var_ls[[x]],
            	rates_ls = N_levels,
            	Method = .$Method[[x]]
            	)
			}, future.seed = TRUE
    	)
  	)%>%
  	unnest(., cols= "te_data")%>%
  	data.table()%>%
  	.[, Method := factor(Method, levels = c("RF_split", "XGBRF","RF", "BRF", "XCF_base", "CF_base"))] %>%
  	.[,!c("var_ls")]


# === Merge the results with forest_te_dt === #
te_comp_dt <- 
	forest_te_dt %>%
	true_te_dt[., on = c("unique_subplot_id", "rate")] %>%
	.[rate != N_levels[1],]%>%
	.[, Treatment := case_when(
		rate == N_levels[2] ~ "N1-N2",
		rate == N_levels[3] ~ "N1-N3",
		rate == N_levels[4] ~ "N1-N4",
		rate == N_levels[5] ~ "N1-N5"
	)]
	


saveRDS(te_comp_dt, here("Data/dt_TEcomparison.rds"))


#/*----------------------------------*/
#' ## Visualization
#/*----------------------------------*/

ggplot(te_comp_dt)+
	geom_point(aes(x=true_te_base, y=te_base), size=0.5)+
	facet_grid(Treatment ~ Method) +
	geom_abline(slope=1, intercept=0, color="red") +
	labs(y = "Estimated Treatment Effect")+
    labs(x = "True Treatment Effect")+
    theme_few()


