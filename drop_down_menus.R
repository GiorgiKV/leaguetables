## Domain UI -----------------------------------------------------------------------------

domain_ui <- function(id, dat){
  
  ns <- NS(id)
  
  pickerInput(ns("domain"),
              label = "Select Domain",
              choices = levels(dat$domain_a),
              selected = "Salt reduction",
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              width = "100%")
}


## Domain selection server ---------------------------------------------------------------

# This provides the names of each main intervention according to the domain selected
# Required for the "Select Main Intervention" drop down menu

domain_server <- function(input, output, session, dat){
  
  dom_select <- reactive({input$domain
    })
  
  tab_dat <- reactive({
    dat %>%
      filter(domain_a %in% dom_select(),
             het_main == "Main") %>%
      droplevels.data.frame(.)
  })
}


# Select Main intervention drop down menu ui -----------------------------------------------


main_select_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("main_menu"))
}


## Main intervention drop down menu server side ---------------------------------------------

# This uses the output from domain_server so the user is restricted to interventions that
# that correspond with the domain(s) selected above

main_select_server <- function(input, output, session, dat){
  
  ns <- session$ns
  output$main_menu <- renderUI({
    
    pickerInput(ns("main_select"),
                label = "Select Main Intervention",
                choices = c(levels(dat()$intervention_main)),
                selected = c("Salt substitution intervention where 25% of the sodium in processed foods was replaced by potassium and magnesium salts ",                            
                             "Salt substitution intervention where most (59%) of the sodium in processed foods was replaced by potassium and magnesium salts "),#"All",
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = "count > 1"),
                multiple = TRUE
    )
  })
}


## Heterogeneity switch ui and server --------------------------------------------------------

het_select_ui <- function(id){
  
  ns <- NS(id)
  switchInput(ns("het_select"),
              "Heterogeneity",
              onStatus = "success", offStatus = "danger")
}


het_server <- function(input, output, session){
  
  reactive({input$het_select})
}


## Uncertainty switch ui and server ----------------------------------------------------------
uncer_select_ui <- function(id){
  
  ns <- NS(id)
  switchInput(ns("uncer_select"), "Uncertainty",
              onStatus = "success", offStatus = "danger")
}


uncer_server <- function(input, output, session){
  
  reactive({input$uncer_select})
}


## Heterogeneity dashboard (placeholder) -------------------------------------------------------

het_dashboard_ui <- function(id){
  
  ns <- NS(id)
  checkboxGroupButtons(inputId = "het_dashboard", 
                       label = strong("Het Choices"), choices = c("Ethnicity", "Age", "Sex"), 
                       status = "primary",
                       individual = TRUE,
                       #justified = TRUE,
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                        no = icon("remove", lib = "glyphicon")))
}


## Plot outcome ui and server ------------------------------------------------------------------

plot_outcome_ui <- function(id){
  
  ns <- NS(id)
  selectInput(ns("p_outcome"),
              label = "Select Outcome Variable",
              choices = c("QALYs" = "qalys",
                          "Health System Costs" = "hs_costs",
                          "ICER" = "icer"
              ),
              selected = "qalys")
}


plot_outcome_server <- function(input, output, session){
  
  reactive({input$p_outcome})
}










