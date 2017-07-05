### For controlling the bootstrap appearance of the table - this can make it more compact or add cell borders etc.
# https://rstudio.github.io/DT/005-bootstrap.html



# Table UI

table_ui <- function(id){
  
  ns <- NS(id)
  tagList(dataTableOutput(ns("table_tar")),
          dataTableOutput(ns("table_tot"))
  )
  
}

#Table server

table_server <- function(input, output, session, dat, uncer_ind){
  

#  output$my_table <-  renderDataTable({
  
expected_cols <- c("Domain" = "domain_a_tab", "Intervention" = "intervention_tab", "Comparator" = "comparator", 
                   "QALYs" = "qalys", "Health System Costs" = "hs_costs", "Intervention Costs" = "int_costs", "ICER" = "icer", 
                   "Time Horizon" = "time_horizon", "Duration and Frequency" = "dur_and_freq", "Perspective" = "perspective", "Links" = "links")
  
uncertainty_cols <- c("Domain" = "domain_a_tab", "Intervention" = "intervention_tab", "Comparator"="comparator", 
                      "QALYs" = "qalys","QALYs CI" = "qalys_uncer", 
                      "Health System Costs"= "hs_costs", "Health System Costs CI" = "hs_costs_uncer",
                      "Intervention Costs" = "int_costs","Intervention Costs CI" = "int_costs_uncer", "ICER" = "icer", "ICER CI" = "icer_uncer",
                      "Time Horizon" = "time_horizon", "Duration and Frequency" = "dur_and_freq", "Perspective" = "perspective", "Links" = "links")


tab_dat <- reactive({dat()%>%
                      mutate(qalys = format(qalys, big.mark = ","))})

#uncertainty_cols <- 
#  uncertainty <- "c(domain_a, intervention_tab, comparator,
#qalys, qalys_low, qalys_upp, 
#hs_costs, hs_costs_low, hs_costs_upp, 
#int_costs, int_costs_low, int_costs_upp, 
#icer, icer_low, icer_upp,
# time_horizon, dur_and_freq, perspective, links)"      
  
  reactive({    
    if(uncer_ind() == FALSE) 
    {
      
      #if (is.null(reactive({input$my_table_rows_selected}))){
      
      datatable(tab_dat(),
                style = 'bootstrap', class = 'table-bordered compact table-hover',
                #,
                #filter = list(position = 'top'),
                rownames = TRUE,
                colnames = expected_cols,
                #extensions = 'Buttons', 
                #selection = list(mode = "multiple", target = "row")
                options = list(
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '450px', targets = c(2)),
                                    list(width = '50px', targets = c(1)),
                                    list(width = '100px', targets = c(5)),
                                    list(width = '80px', targets = c(7)),
                                    list(width = '100px', targets = c(9))),
                                    #list(width = '1000px', targets = 3)),
                  scrollX = TRUE
                  #  dom = 't',
                  #  ordering = FALSE,
                 #   rowCallback = JS("function(row,data) {$(row).attr('height', '100px')}")
                ))%>%
        formatStyle(0, target = 'row', lineHeight='80%')%>%
        formatStyle('Time Horizon', target = 'row', fontWeight = styleEqual(c("Lifetime"), c('bold')))%>%
        formatCurrency('Health System Costs', currency = "$", digits = 0)%>%
        formatCurrency('Intervention Costs', currency = "$")#%>%
       # formatCurrency('QALYs', currency = "", interval = 3, mark = ",", digits=ifelse(tar_dat$qalys<10,3,0))
        
                 # dom = 'Bfrtip',
                #  buttons = list(list(extend = 'colvis', columns = seq(ncol(dat())))),
                #  pagingType = "simple",
                 # pageLength = 10000
               # )%>% formatStyle(names(dat()),#'intervetion_long',
                  #                'time_horizon',
                   #               target = 'row',
                    #              "qalys_cap", color = 'black',
                     #             #backgroundColor = styleInterval(c(0, 10000), c('red', 'orange', 'lightgreen')),
                      #            fontWeight = styleEqual(c("Lifetime"), c('bold'))) 
      
    }
    
    #  }
    else {

    
      
            
      datatable(tab_dat(),
                #filter = list(position = 'top'),
                rownames = TRUE,
                colnames = uncertainty_cols,
                extensions = 'Buttons', 
                selection = list(mode = "multiple", target = "row"),
                options = list(
                  dom = 'Bfrtip',
                  buttons = list(list(extend = 'colvis', columns = seq(ncol(dat())))),
                  pagingType = "simple",
                  pageLength = 10000
                )) #%>% formatStyle(names(dat()),#'intervetion_long',
                                 # 'time_horizon',
                                 # target = 'row',
                                 #"qalys_cap", color = 'black',
                                 # #backgroundColor = styleInterval(c(0, 10000), c('red', 'orange', 'lightgreen')),
                                  #fontWeight = styleEqual(c("Lifetime"), c('bold'))) 
    }
    
  })
  
  
  
  #return(reactive(input$my_table_rows_selected))
  #observeEvent(row_ind(), { 
  #return(reactive(input$my_table_rows_selected))
  #})
  
}


table_render <- function(input, output, session, league_table){
  
  output$table_tar <- DT::renderDataTable({league_table()})
  
  
}
