# Plot functions ------------------------------------------------------------------  

# Plotting ui ---------------------------------------------------------------------
plot_tar_ui <- function(id){
  
  ns <- NS(id)
  plotOutput(ns("plot_tar"))
          
}

plot_tot_ui <- function(id){
  
  ns <- NS(id)
  plotOutput(ns("plot_tot"))
}


plot_cep_ui <- function(id){
  
  ns <- NS(id)
  plotOutput(ns("plot_cep"), height = 400, width = 800)
}


# Plotting template -------------------------------------------------------------

theme_league <- function (base_size = 14, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = "")+
    theme (
      plot.title = element_text(size = base_size*1.2, vjust = 2, face = "bold"),
      panel.border = element_rect(fill=NA, colour="white"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour="gray80", size = 0.3),
      panel.margin = unit(1.3, "lines"),
      strip.background = element_rect(fill = "white", colour = "white"),
      strip.text = element_text(size = base_size*1.2, face = "bold"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "right"
    )
}



# Plot server

plot_height_server <- function(input, output, session, het_ind, plot_dat){
  reactive({  
    
    if(het_ind() == FALSE){
      50*length(plot_dat()$intervention_main)
    } else if (het_ind() == TRUE & nlevels(plot_dat()$intervention_main) == length(plot_dat()$intervention_main)){
      50*length(plot_dat()$intervention_main)
    } else {
      30*length(plot_dat()$intervention_main)
    }
  })
  
}


## Non shiny functions to be used in plot_nest_server -------------------------------
# For plotting ----------------------------------------


plot_nest <- function(dat){  
  
  domain_colours <- c("Cancer_Main" = "#de2d26", "Cancer_Het" = "#fc9272", 
                      "Housing_Main" = "#31a354", "Housing_Het" = "#e5f5e0", 
                      "Salt reduction_Main" = "#756bb1", "Salt reduction_Het" = "#bcbddc", 
                      "Tobacco control_Main" = "#636363", "Tobacco control_Het" = "#bdbdbd", "NA_Het" = "#bdbdbd")
  
  y_limit_max <- max(dat$max_outcome, na.rm = TRUE)
  y_limit_min <- min(dat$min_outcome, na.rm = TRUE)
  
  ggplot(dat, aes(x = intervention_plot, y = y_outcome, fill = dom_het))+
    #intervention_main))+
    geom_bar(width = 0.9, stat = "identity")+
    scale_x_discrete(labels = function(x)str_wrap(x, width = 60), name = "")+
    scale_y_continuous(limits = c(y_limit_min, y_limit_max), expand=c(0.05,0), labels = comma)+
    scale_fill_manual(values = domain_colours)+
    theme(legend.position="none")+
    theme(axis.title.x=element_blank())+
    background_grid(major = "xy", minor = "none")+
    coord_flip()
  
}


plot_nest_icer <- function(dat){  
  
  domain_colours <- c("Cancer_Main" = "#de2d26", "Cancer_Het" = "#fc9272", 
                      "Housing_Main" = "#31a354", "Housing_Het" = "#e5f5e0", 
                      "Salt reduction_Main" = "#756bb1", "Salt reduction_Het" = "#bcbddc", 
                      "Tobacco control_Main" = "#636363", "Tobacco control_Het" = "#bdbdbd", "NA_Het" = "#bdbdbd")
  
  y_limit_max <- max(dat$max_outcome, na.rm = TRUE)
  y_limit_min <- min(dat$min_outcome, na.rm = TRUE)
  
  
  ggplot(dat, aes(x = intervention_plot, y = y_outcome, fill = dom_het))+
    geom_text(stat = "identity", aes(label = icer_label))+#, nudge_y = 10000)+ #x = intervention_plot, y = icer_label, 
    #intervention_main))+
    geom_bar(width = 0.9, stat = "identity")+
    scale_x_discrete(labels = function(x)str_wrap(x, width = 60), name = "")+
    scale_y_continuous(limits = c(y_limit_min, y_limit_max), labels = comma)+#, expand=c(0.05,0))+
    # scale_y_discrete()+
    scale_fill_manual(values = domain_colours)+
    theme(legend.position="none")+
    theme(axis.title.x=element_blank())+
    background_grid(major = "xy", minor = "none")+
    coord_flip()
  
}

plot_nest_icer_single <- function(dat){  
  
  domain_colours <- c("Cancer_Main" = "#de2d26", "Cancer_Het" = "#fc9272", 
                      "Housing_Main" = "#31a354", "Housing_Het" = "#e5f5e0", 
                      "Salt reduction_Main" = "#756bb1", "Salt reduction_Het" = "#bcbddc", 
                      "Tobacco control_Main" = "#636363", "Tobacco control_Het" = "#bdbdbd", "NA_Het" = "#bdbdbd")
  
  y_limit_max <- max(dat$max_outcome, na.rm = TRUE)
  y_limit_min <- min(dat$min_outcome, na.rm = TRUE)
  
  
  ggplot(dat, aes(x = intervention_plot, y = y_outcome, fill = dom_het))+
    geom_text(stat = "identity", aes(vjust = "middle", hjust = "bottom", label = icer_label), nudge_y = 0.05)+  #### for aligning cost-saving label 
    #intervention_main))+
    geom_bar(width = 0.9, stat = "identity")+
    scale_x_discrete(labels = function(x)str_wrap(x, width = 60), name = "")+
    scale_y_continuous(limits = c(y_limit_min, y_limit_max), labels = comma)+#, expand=c(0.05,0))+
    # scale_y_discrete()+
    scale_fill_manual(values = domain_colours)+
    theme(legend.position="none")+
    theme(axis.title.x=element_blank())+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    background_grid(major = "xy", minor = "none")+
    coord_flip()
  
}



# Functions to order cases from good to bad ------------------------------ 

nest_dat_order_qalys <- function(dat){
  
  dat %>%
    arrange(y_outcome)%>%
    mutate(intervention_plot = as_factor(intervention_plot))
  
}


nest_dat_order_hs_costs <- function(dat){
  
  dat %>%
    arrange(desc(y_outcome))%>%
    mutate(intervention_plot = as_factor(intervention_plot))
  
}


# Function to count number of cases within intervention ---------------------

nest_count <- function(dat){
  nrow(dat)
}    


nest_pos_icer <- function(dat){
  max(dat$icer, na.rm = TRUE)
  
}    


## Plot nest server ----------------------------------------------------------------------------  


plot_nest_server <- function(input, output, session, plot_dat, plot_outcome, het_ind){
  
  main_value <- reactive({plot_dat() %>%
      group_by(intervention_main)%>%
      mutate(n = n())%>%
      ungroup()%>%
      select(-intervention_main)%>%
      mutate(weight = if_else(n == 1, (n/sum(n, na.rm = T))*2, n/sum(n, na.rm =T)))%>% #1.7
      filter(het_main == "Main")%>%
      
      ungroup()
  })
  

    order_value <- reactive({if (plot_outcome() == "qalys"){
      
      paste0("desc(", plot_outcome(), ")")
    } else {
      paste0(plot_outcome())
    }
  
  })
  

  plot_dat_nest <- reactive({ 
    
    plot_dat()%>%
      mutate_(y_outcome = plot_outcome())%>%
      group_by(intervention_main)%>%
      nest()%>%
      bind_cols(main_value())%>%
      arrange_(.dots = order_value())%>%
      mutate(outcome = plot_outcome(),
             int_rows = map(data, nest_count),
             positive_icer = map(data, nest_pos_icer))%>%
      mutate(data = ifelse(outcome == "qalys", map(data, nest_dat_order_qalys), map(data, nest_dat_order_hs_costs)))%>%
      mutate(plots = ifelse(outcome == "icer" & positive_icer == 0, map(data, plot_nest_icer_single),
                            ifelse(outcome == "icer" & positive_icer > 0, map(data, plot_nest_icer), 
                            ifelse(outcome == "qalys", map(data, plot_nest),
                            ifelse(outcome == "hs_costs", map(data, plot_nest), map(data, plot_nest))))))
    
  })

}    



## Rendering the target plot -----------------------------------------------------------------

plot_tar_render <- function(input, output, session, plot_height, dat){

  x_scales <- reactive({as.vector(dat()$weight)})     #plot_dat_nest
  
#  p_title <- reactive({ggdraw() + draw_label(paste0(plot_outcome(), " (per capita)"), fontface = "bold")}) # needs scaling
  
  output$plot_tar <- renderPlot({
    plot_grid(plotlist = dat()$plots, nrow = length(dat()$intervention_main),
              align = "v", rel_heights = x_scales()) 
    
  }, height = plot_height, width = 850) #plot_height
  
}


## Rendering the total plot ------------------------------------------------------------------

plot_tot_render <- function(input, output, session, plot_height, dat){
  
  x_scales <- reactive({as.vector(dat()$weight)})     #plot_dat_nest
  
 # p_title <- reactive({ggdraw() + draw_label(paste0(plot_outcome(), " "))})
  
  output$plot_tot <- renderPlot({
    plot_grid(plotlist = dat()$plots, nrow = length(dat()$intervention_main),
              align = "v", rel_heights = x_scales()) 
    
  }, height = plot_height, width = 850) #plot_height
  
}



# ==========================================================================


plot_scatter_server <- function(input, output, session, plot_dat){
  
  output$plot_cep <- renderPlot({
   
#    y_axis <- list(
#      zeroline = TRUE,
#      showline = TRUE,
#      rangemode = "tozero",
#      overlaying = "y",
#      side = "right"
#      #mirror = "ticks",
#      #gridcolor = toRGB("gray50"),
#      #gridwidth = 2,
#      #zerolinecolor = toRGB("red"),
#      #zerolinewidth = 4,
#      #linecolor = toRGB("black"),
#      #linewidth = 6
#    )
#    
#    x_axis <- list(
#      showline = TRUE
#    )
#    
#    plot_ly(data = plot_dat(), x = ~qalys, y = ~hs_costs) %>%
#      layout(#xaxis = y_axis,
#             yaxis2 = y_axis
#  #      yaxis = y_axis)
#      )
    
     
      ggplot(plot_dat(), aes(x = qalys, y = hs_costs, colour = domain_a))+
       # theme_set(theme_gray())+
        geom_point(aes(shape = intervention_main))+
        geom_vline(xintercept = 0, alpha = 0.5, size = 0.3)+ 
        geom_hline(yintercept = 0, alpha = 0.5, size = 0.3)+
   #     scale_x_continuous(alpha = 0.1)+
      background_grid(major = "xy", minor = "none")+  
     theme_league()+ 
     theme(legend.position = "none")#+
        #facet_grid(~domain_a)
    
  })
  
}


# hover mouse to show intervention_short, qalys and costs. Min information. 



