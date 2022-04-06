#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 

### PACKAGES ######

library(shiny)
library(tidyverse)
library(rvest) # scrape data
library(mclust) # for clustering
library(janitor) # clean up variable names
library(scales) # resvale variables
library(DT) # create pretty table
### Read in data ####

# custom scraping function
scrape_table <- function(url){
  read_html(url) %>%
    html_nodes("table.tablesorter") %>%
    .[1] %>% # keep the third of these tables
    .[[1]] %>% # keep the first element of this list
    html_table(fill=T)  %>%
    clean_names() %>%
    mutate_at(vars(contains("_")), str_replace, pattern = "%", replacement = "") %>%
    mutate_at(vars(contains("_")), as.numeric)
  
}


# path to all files
# last 52 weeks
rally <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_rally_women_last52.html")
return <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_return_women_last52.html")
tactics <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_tactics_women_last52.html")
serve <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_serve_women_last52.html")


# career files. 
# rally <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_rally_women_career.html")
# return <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_return_women_career.html")
# tactics <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_tactics_women_career.html")
# serve <- scrape_table("https://tennisabstract.com/reports/mcp_leaders_serve_women_career.html")

# merge all data into all_tables object
all_tables <- 
  serve %>%
  full_join(return, by = c("player", "matches"), suffix = c("_serve","_return")) %>%
  full_join(tactics, by = c("player", "matches")) %>%
  full_join(rally, by = c("player", "matches"))  %>%
  select(player,matches,  rally_len, rdi, bh_slice_percent, fhp_100, 
         bhp_100 , sn_v_freq, net_freq, rally_agg,
         drop_freq, return_agg, unret_percent, ri_p_w_percent_serve, 
         ri_p_w_percent_return, ri_p_w_percent_2_return, ri_p_w_percent_2_serve, x2nd_agg) %>%
  rename(`Rally Length` = rally_len, 
        `1st serve points won (serve)` = ri_p_w_percent_serve, 
        `2nd serve points wov (serve)` = ri_p_w_percent_2_serve,
        `1st serve points won (return)` = ri_p_w_percent_return, 
        `2nd serve points won (return)` = ri_p_w_percent_2_return,
        `2nd serve aggression` = x2nd_agg,
        `Backhand slice %` = bh_slice_percent, 
        `Forehand potency` = fhp_100, 
        `Backhand potency` = bhp_100, 
        `Serve and volley freq` = sn_v_freq, 
        `Net freq` = net_freq, 
        `Unreturned serve % (serve)` = unret_percent, 
        `Return aggression` = return_agg, 
        `Rally aggression` = rally_agg, 
        `Drop shot frequency` = drop_freq, 
        `Return Depth Index` = rdi)

  
# pulling out names and matches. 
metadata <-   all_tables %>%
  select(player, matches)







##### UI ######


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
   # shinythemes::themeSelector(),
    
    # Application title
    titlePanel( "WTA Playstyles Gaussian Mixture Model"),

    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # choose features to include in model
        checkboxGroupInput(inputId = "Features", 
                           label = "Possible Features", 
                           choices = colnames(all_tables)[c(-1, -2)],
                           selected = colnames(all_tables)[c(-1, -2)]), 
      
        
        # choose how many cluster to use with gmm
        numericInput(inputId = 'clusters', label = 'Clusters used', 6, min = 2, max = 20), 
        
        # choose how many cluster to use with gmm
        sliderInput(inputId = 'Uncertainty', label =  'Uncertainty cutoff',  min = 0, max = 0.8, value = 0.25), 
        actionButton("goButton", "Start/Update!"),
          
      ), 
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Diagnostic",  plotOutput("plot_diagnostic")), 
                    tabPanel("Uncertainty",  plotOutput("plot_uncertainty")), 
                    tabPanel("Heat Map",  plotOutput("plot_heat_map")), 
                    tabPanel("Boxplot Summary",  plotOutput("plot_boxplot")), 
                    tabPanel("Classification and data",  DT::dataTableOutput("table_classification"), 
                             width = 12)
        )
        
      )
      
    

    )
)



#### Server #####

# Define server logic required to draw a histogram
server <- function(input, output) {


  
 
  # filter data down to selected
  data_for_gmm <- 
    eventReactive(input$goButton, {
      all_tables %>%
      select(any_of(input$Features)) #%>%
     # filter(player %in% intput$Players)
      })
  
  # to get an idea on what to set as the number of clusters
  general_model <- 
    eventReactive(input$goButton, {
      Mclust(data_for_gmm()) 
      })
  
  
  output$plot_diagnostic <- 
    renderPlot({
      plot(general_model(), what = "BIC")
    })
    
  
  # choosing the better parameters. 
  gmm_model_final <- 
    eventReactive(input$goButton, {
      Mclust(data = data_for_gmm(), G = input$clusters)
  })
  
  


  
  output$plot_uncertainty <- 
    renderPlot({
      # plot the uncertainty for each player in each cluster. 
      tibble(uncertainty = gmm_model_final()$uncertainty, 
             cluster = gmm_model_final()$classification, 
             player = metadata$player) %>%
        ggplot(aes(x = uncertainty, y = reorder(player, uncertainty))) + 
        geom_point() + facet_wrap(~ cluster, scales = "free_y") + ylab("Players")
    })
      
  
  heat_map_data <- 
    reactive({ 
      data_for_gmm() %>%
        mutate_all(scales::rescale) %>%
        rename_all( ~ paste0("feature_", .x)) %>% # so i can pivot easily
        add_column(classification = gmm_model_final()$classification) %>%
        add_column(uncertainty = gmm_model_final()$uncertainty) %>%
        add_column(player = metadata$player) %>%
        filter(uncertainty < input$Uncertainty) # drop ones that didn't fit well 
      
    })
  
  

  
  
  output$plot_heat_map <- 
    renderPlot({
      heat_map_data() %>%
        pivot_longer(cols = starts_with("feature"), names_to = "Feature", values_to = "Value") %>%
        mutate(Feature = str_replace(string = Feature, pattern = "feature_", replacement = "")) %>% # remove feature in name
        ggplot(aes(x = Feature, y = player,  fill = Value)) + 
        geom_tile() + facet_wrap(~classification, scales = "free_y") + 
        scale_fill_gradient(name = "Relative Value",
                            low = "#FFFFFF",
                            high = "#012345")  + 
        theme(axis.text.x = element_text(angle = 90))
    })                            
  
  
  output$plot_boxplot <- 
    renderPlot({
      heat_map_data() %>%
        pivot_longer(cols = starts_with("feature"), names_to = "Feature", values_to = "Value") %>%        
        mutate(Feature = str_replace(string = Feature, pattern = "feature_", replacement = "")) %>% # remove feature in name
        mutate(classification = as.factor(classification)) %>%
        ggplot(aes(x = classification, y = Value, color = classification, fill = classification)) + geom_boxplot() + facet_wrap(~Feature)
    })
  
  
  
  output$table_classification <- 
    DT::renderDataTable({
      
      finished_table <- 
        heat_map_data()  %>%
        relocate(c(player, classification, uncertainty), .before = where(is.numeric)) %>% # pull names to the front
        rename_with(~str_replace(., pattern = "feature_", replacement = "")) %>%
        arrange((classification)) %>%
        mutate_if(is.numeric, ~round(., digits = 2)) 
      
      DT::datatable(finished_table, options = list(lengthMenu = c(10, 50, 100, 200), pageLength = 100))
    })
  
}
  
  
  
  
  
  
  
  
  
  
# Run the application 
shinyApp(ui = ui, server = server)


