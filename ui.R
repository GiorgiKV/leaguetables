

shinyUI(
  
    
  

  
  navbarPage(
    
    
    
    
    title = "", position = "static-top", theme = "bode.css", fluid = TRUE,
             inverse = T,
             tabPanel("League Table",
                      
                  
                      titlePanel (tags$div(
                                    HTML(paste0("BODE", tags$sup(3), " League Table"))
                                    )
                                  ),
                     # br(),
                      p("This interactive league table allows researchers and policy-makers to rank health ‘interventions’ 
                        by health gains, costs, or cost-effectiveness. You can use the section on the left to customize your search." ),
                   
                        column(3,
                      
                      
                        sidebarPanel(
                          
                          domain_ui(id = "domain", dat = tar_dat),
                          
                          main_select_ui("main_menu"),
                          br(),
                         
                         div(style="display: inline-block;vertical-align:top; width: 150px;", het_select_ui("het_select")),
                         div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                         div(style="display: inline-block;vertical-align:top; width: 150px;", uncer_select_ui("uncer_select")),
                         
                         # het_dashboard_ui("het_dashboard"),
                          
                                   
                          h4(strong("Plotting Options")),
                          plot_outcome_ui("p_outcome"),        
                          width = 12
                          )),
                        
                        column(9, #align = "left",
                        
                          tabsetPanel(
                          tabPanel("Target Population",
                        
                        
#                        h5("Select/Hide Columns"),
                                  table_ui("table_tar"),
                                  
                        
                                 # render_text_ui("text_out"),
                                  plot_tar_ui("plot_tar"),
                                  br(),
                                  br(),
                                  br(),
                                  br()), 
                      
                      tabPanel("Total Population",
 #                       h5("Select/Hide Columns"),
                                           table_ui("table_tot"),
                                           plot_tot_ui("plot_tot")
                      ),
                      tabPanel("Cost Effectiveness Plane",
                          plot_cep_ui("plot_cep"))
                      )
                     ) ),
                      
                      
                      
             tabPanel ("Further Details",
                       titlePanel("Further Details on the League Table and Research Team"),
                       mainPanel(
                         p("Insert some text and explanation here. This page can include information on
                           BODE, how the research was funded, links to our website etc etc")
                         
                         )),
             tabPanel("Help",
                      mainPanel(
                        p("If people agree we can insert some text helping people to use the league tables")
                      )))

    
  )


