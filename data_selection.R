## Create a character string to use in filtering of main interventions ------------------------------------
# note if no main interventions are selected, all the main interventions for the selected domains
# will be returned

main_char_str_server <- function(input, output, session, dat){
  
  char_str <- reactive({dat()})
  
  main_char_str <-  reactive({
    if (is.null(input$main_select)) {
      dput(levels(char_str()$intervention_main)) 
    } else {
      input$main_select
    }
  })  
}

## Data selection for the league table -------------------------------------------------------------------

league_data_select <- function(input, output, session, dat, main_char_str, het_ind){
  
  standard <- "c(domain_a_tab, intervention_tab, comparator,  
                 qalys, hs_costs, int_costs, icer,     
                 time_horizon, dur_and_freq, perspective, links)"

  #### NEED TO REPLACE 'EXPECTED VALUE COLUMNS' WITH MEAN VALUES FROM MONTE CARLO SIMULATION ####
    
  uncertainty <- "c(domain_a_tab, intervention_tab, comparator,
                    qalys, qalys_uncer, 
                    hs_costs, hs_costs_uncer, 
                    int_costs, int_costs_uncer, 
                    icer, icer_uncer,
                    time_horizon, dur_and_freq, perspective, links)"
  
  main_str <- reactive({main_char_str()})
  
  col_str <- reactive({
    if(input$uncer_select == TRUE){
      uncertainty
    } else if (input$uncer_select == FALSE) {
    standard
  }
  })
  
  reactive({if(het_ind() == FALSE){ 
    
    dat %>%
      filter(intervention_main %in% main_str(),
             het_main %in% c("Main"))%>%
      select_(.dots = col_str())%>% #look up standard and non-standard evaluation for more info
      droplevels.data.frame(.)

      } else if (het_ind() == TRUE) {
    
    dat %>%
      filter(intervention_main %in% main_str(),
             het_main %in% c("Main", "Het"))%>%
      select_(.dots = col_str())%>%
      droplevels.data.frame(.)
    }
  })  
}


# To select data for the plots ------------------------------------------------


plot_data_select <- function(input, output, session, dat, main_char_str, plot_outcome, het_ind){
    
  main_str <- reactive({main_char_str()})
  
  dat1 <- dat %>%
            select(domain_a:het_main, intervention_plot, qalys, hs_costs, icer, icer_label, int_costs)%>% #add in other outcomes
            gather(key, intervention_plot, -domain_a, -domain_b, -domain_c, -domain_d,
                   -intervention_main, -qalys, -hs_costs, -icer, -icer_label, -int_costs,
                   -het_main)%>%  
            unite(dom_het, domain_a, het_main, sep = "_", remove = FALSE)%>% #for graph colouring
            mutate(dom_het = as_factor(dom_het))%>%
            ungroup()%>%
            droplevels.data.frame(.)
  
  reactive({if(het_ind() == FALSE & plot_outcome() == "qalys"){
    
  dat2 <- dat1 %>%
            filter(intervention_main %in% main_str(),
                   het_main %in% c("Main"))%>%
            mutate_(max_outcome = interp(~max(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
            mutate(max_outcome = ifelse(max_outcome < 0, 0, max_outcome))%>%
            mutate(min_outcome = 0)%>% #reactively select variable for max_outcome
            droplevels.data.frame(.)
    
  } else if (het_ind() == FALSE & plot_outcome() == "hs_costs") {
    
  dat2 <- dat1 %>%
            filter(intervention_main %in% main_str(),
                   het_main %in% c("Main"))%>%
    mutate_(max_outcome = interp(~max(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
    mutate_(min_outcome = interp(~min(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
    mutate(max_outcome = if_else(max_outcome < 0, 0, max_outcome))%>%
    mutate(min_outcome = if_else(min_outcome > 0, 0, min_outcome))%>%
    #mutate(max_outcome = max(qalys_cap, na.rm = TRUE))%>%
            droplevels.data.frame(.)
    }
    
    
    else if (het_ind() == FALSE & plot_outcome() == "icer") {
      
      dat2 <- dat1 %>%
        filter(intervention_main %in% main_str(),
               het_main %in% c("Main"))%>%
        mutate_(max_outcome = interp(~max(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
       # mutate_(min_outcome = interp(~min(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
        mutate(max_outcome = if_else(max_outcome == 0, 1, max_outcome))%>%
        #mutate(min_outcome = if_else(min_outcome > 0, 0, min_outcome))%>%
        mutate(min_outcome = 0)%>%
        #mutate(max_outcome = max(qalys_cap, na.rm = TRUE))%>%
        droplevels.data.frame(.)
    }
    
    
    
    
    
    
    
    else if(het_ind() == TRUE & plot_outcome() == "qalys"){
      
      dat2 <- dat1 %>%
        filter(intervention_main %in% main_str(),
               het_main %in% c("Main", "Het"))%>%
        mutate_(max_outcome = interp(~max(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
        mutate(max_outcome = ifelse(max_outcome < 0, 0, max_outcome))%>%
        mutate(min_outcome = 0)%>% #reactively select variable for max_outcome
        droplevels.data.frame(.)
   
     } else if (het_ind() == TRUE & plot_outcome() == "hs_costs") {
      
      
      dat2 <- dat1 %>%
        filter(intervention_main %in% main_str(),
               het_main %in% c("Main", "Het"))%>%
        mutate_(max_outcome = interp(~max(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
        mutate_(min_outcome = interp(~min(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
        mutate(max_outcome = if_else(max_outcome < 0, 0, max_outcome))%>%
        #mutate(min_outcome = if_else(min_outcome > 0, 0, min_outcome))%>%
        mutate(min_outcome = if_else(min_outcome > 0, 0, min_outcome))%>%
        #mutate(max_outcome = max(qalys_cap, na.rm = TRUE))%>%
        droplevels.data.frame(.)
    }

    else if (het_ind() == TRUE & plot_outcome() == "icer") {
      
      
      dat2 <- dat1 %>%
        filter(intervention_main %in% main_str(),
               het_main %in% c("Main", "Het"))%>%
        mutate_(max_outcome = interp(~max(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
        mutate_(min_outcome = interp(~min(outcome, na.rm = TRUE), outcome = as.name(plot_outcome())))%>% #reactively select variable for max_outcome
        mutate(max_outcome = if_else(max_outcome == 0, 1, max_outcome))%>%
        mutate(min_outcome = 0)%>%
        #mutate(max_outcome = max(qalys_cap, na.rm = TRUE))%>%
        droplevels.data.frame(.)
    }
    
    
  })

}  
  







  
  
  
  
  
  
  
  







