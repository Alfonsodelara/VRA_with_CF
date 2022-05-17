library(sf)
library(data.table)
library(tidyverse)

# --- figure making --- #
# library(extrafont)
library(ggbrace)
library(RColorBrewer)
library(patchwork)
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggpubr)
library(gridExtra)
library(DiagrammeR)

# --- table making --- #
library(flextable)
library(ftExtra)
library(officer)
library(officedown)
library(modelsummary)



res_y_test <-
  allML_summary_bySim %>%
  .[type == "test" & Method %in% c("RF_ranger","RF_split","XGBRF","RF", "BRF", "CNN")]



# --- on testing data sets--- #
#report_table_y <-
res_y_test %>%
  .[, .(rmse_y = mean(rmse_y)), by = .(Method, Model)] %>%
  .[, rmse_y := format(round(rmse_y, 1), nsmall = 1)] %>%
  dcast(Model ~ Method, value.var = "rmse_y") %>%
  .[, CF_base := "-"] %>%
  mutate(
    across(
      everything(),
      as.character
    )
  ) %>%
  flextable(.) %>%
  set_header_labels(values = list(
    Model = "Model",
    RF_spit = "RF_split",
    RF_ranger = "RF_ranger",
    XGBRF = "XGBRF", 
    RF = "RF",
    BRF = "BRF",
    CNN = "CNN",
    XCF_base= "XCF_base",
    CF_base = "CF-base"
  )) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  #- change the borders just for consistency with other figures -#
  hline_bottom(part = "all") %>%
  hline_top(part = "header") %>%
  autofit()







# TABLE
allML_summary_bySim %>%
  .[type=="test",] %>%
  .[Method %in% c("RF_ranger","RF_split","XGBRF","RF", "BRF", "XCF_base", "CF_base"), ] %>%
  .[Method == "CF_base", Method := "CF-base"] %>%
  .[, Model := case_when(
    Model == "aby" ~ "Scenario: aby",
    Model == "abytt" ~ "Scenario: abytt",
    Model == "aabbyy" ~ "Scenario: aabbyy",
    Model == "aabbyytt" ~ "Scenario: aabbyytt"
  )] %>%
  .[, Model := factor(Model,
                      levels = c("Scenario: aby", "Scenario: abytt", "Scenario: aabbyy", "Scenario: aabbyytt")
  )] %>% 
  group_by(Method,Model)%>% summarise(rmse_optN=mean(rmse_optN), pi_loss=mean(pi_loss), rmse_y=mean(rmse_y))

# PLOT
  allML_summary_bySim %>%
    .[type=="test",] %>%
    .[Method %in% c("RF_ranger","RF_split","XGBRF","RF", "BRF", "CF_base"), ] %>%
    .[Method == "CF_base", Method := "CF-base"] %>%
    .[, Model := case_when(
      Model == "aby" ~ "Scenario: aby",
      Model == "abytt" ~ "Scenario: abytt",
      Model == "aabbyy" ~ "Scenario: aabbyy",
      Model == "aabbyytt" ~ "Scenario: aabbyytt"
    )] %>%
    .[, Model := factor(Model,
                        levels = c("Scenario: aby", "Scenario: abytt", "Scenario: aabbyy", "Scenario: aabbyytt")
    )] %>% 
  ggplot() +
    geom_density(aes(x = rmse_optN, fill = Method), alpha = 0.7) +
    facet_wrap(~Model, ncol = 1) +
    labs(x = "RMSE (kg/ha)")
  
  
  
  allML_summary_bySim %>%
    .[type=="test",] %>%
    .[Method %in% c("RF_ranger","RF_split","XGBRF","RF", "BRF", "CF_base"), ] %>%
    .[Method == "CF_base", Method := "CF-base"] %>%
    .[, Model := case_when(

      Model == "aby" ~ "Scenario: aby",
      Model == "abytt" ~ "Scenario: abytt",
      Model == "aabbyy" ~ "Scenario: aabbyy",
      Model == "aabbyytt" ~ "Scenario: aabbyytt"
    )] %>%
    .[, Model := factor(Model,
                        levels = c("Scenario: aby", "Scenario: abytt", "Scenario: aabbyy", "Scenario: aabbyytt")
    )] %>% 
    ggplot() +
    geom_boxplot(aes(y = rmse_optN, x=Method, fill=Method), alpha = 0.7) +
    facet_wrap(~Model) +
    labs(x = "RMSE (kg/ha)")
  

  
  
#################################################################################
# 
# TEST  BCF   https://github.com/jaredsmurray/bcf/blob/master/examples/simple_example.R

  set.seed(1)
  
  p <- 3 # two control variables and one effect moderator
  n <- 250
  n_burn <- 2000
  n_sim <- 2000
  
  x <- matrix(rnorm(n*p), nrow=n)
  
  
  # create targeted selection, whereby a practice's likelihood of joining the intervention (pi) is related to their expected outcome (mu)
  q = -1*(x[,1]>(x[,2])) + 1*(x[,1]<(x[,2]))
  
  # generate treatment variable
  pi <- pnorm(q)
  z <- rbinom(n,1,pi)
  
  # tau is the true treatment effect. It varies across practices as a function of
  # X3, the effect moderator
  tau = (0.5*(x[,3] > -3/4) + 0.25*(x[,3] > 0) + 0.25*(x[,3]>3/4))
  
  # generate the response using q, tau and z
  mu = (q + tau*z)
  
  # set the noise level relative to the expected mean function of Y
  sigma = diff(range(q + tau*pi))/8
  
  # draw the response variable with additive error
  y = mu + sigma*rnorm(n)
  
  bcf_out <- bcf::bcf(y                = y, #response variable
                      z                = z, #trt variable
                      x_control        = x, #design matrix for the "prognostic" function mu(x)
                      x_moderate       = x, #Design matrix for the covariate-dependent treatment effects tau(x)
                      pihat            = pi, #Length n estimates of
                      nburn            = n_burn, #Number of burn-in MCMC iterations
                      nsim             = n_sim)  #Number of MCMC iterations to save after burn-in

summary(bcf_out)
# Get posterior of treatment effects
tau_post = bcf_out$tau
tauhat = colMeans(tau_post)
plot(tau, tauhat); abline(0,1)
#################################################################################

data <- readRDS("./Data/reg_data.rds") %>% filter(sim==1)

rates_ls <- data[, rate] %>%
  unique() %>%
  sort()
rates = rates_ls[c(1, 1 + 1)]

data_temp_dt <- data %>%
  .[rate %in% rates, ] %>%
  .[, trt := ifelse(rate == rates[1], 0, 1)]

var_ls= c("alpha", "beta", "ymax")
x <- data_temp_dt[, ..var_ls]%>% as.matrix()
y <- data_temp_dt[, yield]
z <- data_temp_dt[, trt]

Y_forest <- regression_forest(x, y)
pi <- predict(Y_forest)$predictions




bcf_out <- bcf::bcf(y                = y, #response variable
                    z                = z, #trt variable
                    x_control        = x, #design matrix for the "prognostic" function mu(x)
                    x_moderate       = x, #Design matrix for the covariate-dependent treatment effects tau(x)
                    pihat            = pi, #Length n estimates of
                    nburn            = n_burn, #Number of burn-in MCMC iterations
                    nsim             = n_sim) 

tau_post = bcf_out$tau
tauhat = colMeans(tau_post)
tau= data_temp_dt[,opt_N]
plot(tau, tauhat); abline(0,1)


tau_data <- predict(object= bcf_out,  
                    x_predict_control        = x, #design matrix for the "prognostic" function mu(x)
                    x_predict_moderate       = x, #Design matrix for the covariate-dependent treatment effects tau(x)
                    z_pred                   = z, #trt variable
                    pi_pred            = pi)



