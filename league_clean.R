library(tidyverse)
library(readxl)
library(lazyeval)
library(stringr)
# Functions to clean and tidy the league table spreadsheet into an R data set for use in leaguetable hosted online ------------------

## TIDY THIS CODE UP ========================================================

league_spreadsheet <- read_excel("G:/League table/League Table Master Spreadsheet_0905.xlsx", skip = 1)%>%
                        mutate(discount_rate = if_else(is.na(discount_rate), 0.03, discount_rate))%>%
                        filter(discount_rate == 0.03,
                               icer_ind == "Y")

# Remove white space at end of colnames ### this needs putting into a function

#league_cols <- colnames(league_spreadsheet)
#league_cols <- str_trim(league_cols, side = "right")
#league_spreadsheet <- setNames(league_spreadsheet, league_cols)


# Function to rename the league table columns - this is hard coded. TAKE CARE ---------------------------------------------------

lt_cols <- function(dat){
 
  dat %>%
    mutate_at(c("domain_a", "domain_b", "domain_c", "domain_d", 
                "intervention_main", "het_main", "intervention_plot",  
                "time_horizon", "target_pop", "evidence_strength"), factor)#%>%
    #mutate(links=sprintf('<a href="links" target="_blank" class="btn btn-primary">Info</a>'))
  
}




# Splits the data by target, total and equity, and then relabels the variables so they are consistent --------

lt_split <- function(dat){
 
 tar_dat <- dat %>%
                select(domain_a_tab:heterogeneity, links, contains("tar"), -contains("eq_"))%>%
                mutate(qalys_uncer = paste0("(", qalys_tar_low, ", ", qalys_tar_upp, ")"),
                       hs_costs_uncer = paste0("(", hs_costs_tar_low, ", ", hs_costs_tar_upp, ")"),
                       int_costs_uncer = paste0("(", int_costs_tar_low, ", ", int_costs_tar_upp, ")"),
                       icer_uncer = paste0("(", icer_tar_low, ", ", icer_tar_upp, ")"),
                       icer_tar = as.numeric(icer_tar),
                       icer_tar = ifelse (is.na(icer_tar), 0, icer_tar),
                       icer_label = ifelse (icer_tar == 0, "Cost-saving", NA),##########
                  df = "tar_dat")



names(tar_dat) <- tar_dat %>%
                      names(.)%>%
                      str_replace("_tar", "")
  
  tot_dat <- dat %>%
              select(domain_a_tab:heterogeneity, links, contains("tot"), -contains("eq_"))%>%
                mutate(qalys_uncer = paste0("(", qalys_tot_low, ", ", qalys_tot_upp, ")"),
                       hs_costs_uncer = paste0("(", hs_costs_tot_low, ", ", hs_costs_tot_upp, ")"),
                       int_costs_uncer = paste0("(", int_costs_tot_low, ", ", int_costs_tot_upp, ")"),
                       icer_uncer = paste0("(", icer_tot_low, ", ", icer_tot_upp, ")"),
                       icer_tot = as.numeric(icer_tot),
                       icer_tot = ifelse (is.na(icer_tot), 0, icer_tot),
                       icer_label = ifelse (icer_tot == 0, "Cost-saving", NA),
                df = "tot_dat")
  
  names(tot_dat) <- tot_dat %>%
                      names(.)%>%
                      str_replace("_tot", "")
  
  
  eq_tar_dat <- dat %>%
                  select(domain_a_tab:heterogeneity, links, contains("eq"))%>%
                  select(domain_a_tab:heterogeneity, links, contains("tar"))%>%
           mutate(qalys_uncer = paste0("(", eq_qalys_tar_low, ", ", eq_qalys_tar_upp, ")"),
           hs_costs_uncer = paste0("(", eq_hs_costs_tar_low, ", ", eq_hs_costs_tar_upp, ")"),
           int_costs_uncer = paste0("(", eq_int_costs_tar_low, ", ", eq_int_costs_tar_upp, ")"),
           icer_uncer = paste0("(", eq_icer_tar_low, ", ", eq_icer_tar_upp, ")"),
           eq_icer_tar = as.numeric(eq_icer_tar),
           eq_icer_tar = ifelse (is.na(eq_icer_tar), 0, eq_icer_tar),
           icer_label = ifelse (eq_icer_tar == 0, "Cost-saving", NA),
           df = "eq_tar_dat")
  
  names(eq_tar_dat) <- eq_tar_dat %>%
                        names(.)%>%
                        str_replace("_tar", "")
  
  eq_tot_dat <- dat %>%
                  select(domain_a_tab:heterogeneity, links, contains("eq"))%>%
                  select(domain_a_tab:heterogeneity, links, contains("tot"))%>%
    mutate(qalys_uncer = paste0("(", eq_qalys_tot_low, ", ", eq_qalys_tot_upp, ")"),
           hs_costs_uncer = paste0("(", eq_hs_costs_tot_low, ", ", eq_hs_costs_tot_upp, ")"),
           int_costs_uncer = paste0("(", eq_int_costs_tot_low, ", ", eq_int_costs_tot_upp, ")"),
           icer_uncer = paste0("(", eq_icer_tot_low, ", ", eq_icer_tot_upp, ")"),
           eq_icer_tot = as.numeric(eq_icer_tot),
           eq_icer_tot = ifelse (is.na(eq_icer_tot), 0, eq_icer_tot),
           icer_label = ifelse (eq_icer_tot == 0, "Cost-saving", NA),
           df = "eq_tot_dat")
  
  names(eq_tot_dat) <- eq_tot_dat %>%
                        names(.)%>%
                        str_replace("_tot", "")
  
  dat_list <- list(tar_dat = tar_dat, tot_dat = tot_dat, eq_tar_dat = eq_tar_dat, eq_tot_dat = eq_tot_dat)

  lapply(1:length(dat_list), function(i) saveRDS(dat_list[[i]],
                                                 file = paste0("G:/League table/Dev/league_tab/data/", names(dat_list)[i], ".rds")))
  
  }

### ================================================================================

# Code below calls the functions above ---------------------------------------------

clean_data <- lt_cols(league_spreadsheet)
lt_split(dat = clean_data)





