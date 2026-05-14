library(shiny)
library(tidyverse)
library(maps)
library(scales)
library(viridis)

print(getwd())
print(list.files())

if (!file.exists("honeyproduction.csv")) {
  stop("honeyproduction.csv was not found in the app directory. Files available are: ",
       paste(list.files(), collapse = ", "))
}

honey <- readr::read_csv("honeyproduction.csv", show_col_types = FALSE)
honey_clean <- honey %>%
  mutate(
    year = as.integer(year),
    state = str_to_upper(state),
    totalprod_mil = totalprod / 1000000,
    prodvalue_mil = prodvalue / 1000000,
    stocks_mil = stocks / 1000000
  )

state_lookup <- tibble(
  state = state.abb,
  region = str_to_lower(state.name)
)

us_states <- map_data("state")

honey_map_data <- honey_clean %>%
  left_join(state_lookup, by = "state")

ui <- fluidPage(
  
  titlePanel("Interactive U.S. Honey Production Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Customize the Map"),
      
      sliderInput(
        inputId = "selected_year",
        label = "Choose a year:",
        min = min(honey_clean$year, na.rm = TRUE),
        max = max(honey_clean$year, na.rm = TRUE),
        value = min(honey_clean$year, na.rm = TRUE),
        step = 1,
        sep = ""
      ),
      
      selectInput(
        inputId = "selected_variable",
        label = "Choose a variable:",
        choices = c(
          "Total production, million lb" = "totalprod_mil",
          "Number of colonies" = "numcol",
          "Yield per colony" = "yieldpercol",
          "Price per pound" = "priceperlb",
          "Production value, million dollars" = "prodvalue_mil",
          "Stocks, million lb" = "stocks_mil"
        ),
        selected = "totalprod_mil"
      ),
      
      hr(),
      
      p("Use the slider to choose a year and the dropdown to choose what the map displays."),
      p("Click on a state to view its honey production information.")
    ),
    
    mainPanel(
      plotOutput(
        outputId = "honey_map",
        height = "575px",
        click = "map_click"
      ),
      
      h4("Clicked State Information"),
      tableOutput("clicked_state_info"),
      
      hr(),
      
      plotOutput("national_trend", height = "325px"),
      
      hr(),
      
      h4("Selected Year Summary"),
      tableOutput("year_summary")
    )
  )
)

server <- function(input, output) {
  
  selected_year_data <- reactive({
    honey_map_data %>%
      filter(year == input$selected_year)
  })
  
  map_for_year <- reactive({
    us_states %>%
      left_join(selected_year_data(), by = "region")
  })
  
  variable_label <- reactive({
    case_when(
      input$selected_variable == "totalprod_mil" ~ "Total production, million lb",
      input$selected_variable == "numcol" ~ "Number of colonies",
      input$selected_variable == "yieldpercol" ~ "Yield per colony",
      input$selected_variable == "priceperlb" ~ "Price per pound",
      input$selected_variable == "prodvalue_mil" ~ "Production value, million dollars",
      input$selected_variable == "stocks_mil" ~ "Stocks, million lb",
      TRUE ~ input$selected_variable
    )
  })
  
  clicked_state <- reactive({
    
    req(input$map_click)
    
    clicked_point <- nearPoints(
      df = map_for_year(),
      coordinfo = input$map_click,
      xvar = "long",
      yvar = "lat",
      maxpoints = 1,
      threshold = 10
    )
    
    if (nrow(clicked_point) == 0) {
      return(NULL)
    }
    
    clicked_region <- clicked_point$region[1]
    
    selected_year_data() %>%
      filter(region == clicked_region) %>%
      distinct(
        state,
        region,
        year,
        totalprod_mil,
        numcol,
        yieldpercol,
        priceperlb,
        prodvalue_mil,
        stocks_mil
      )
  })
  
  output$honey_map <- renderPlot({
    
    ggplot(
      data = map_for_year(),
      mapping = aes(
        x = long,
        y = lat,
        group = group,
        fill = .data[[input$selected_variable]]
      )
    ) +
      geom_polygon(color = "white", linewidth = 0.2) +
      coord_fixed(1.3) +
      scale_fill_viridis_c(
        option = "C",
        na.value = "gray90",
        labels = label_number(),
        name = variable_label()
      ) +
      labs(
        title = paste("U.S. Honey", variable_label(), "by State"),
        subtitle = paste("Selected year:", input$selected_year),
        x = NULL,
        y = NULL,
        caption = "Click on a state to view its values. Data: honeyproduction.csv"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "right"
      )
  })
  
  output$clicked_state_info <- renderTable({
    
    state_data <- clicked_state()
    
    if (is.null(state_data) || nrow(state_data) == 0) {
      return(
        tibble(
          Message = "Click on a state to see its honey production information."
        )
      )
    }
    
    state_data %>%
      transmute(
        State = str_to_title(region),
        Year = year,
        `Total production, million lb` = round(totalprod_mil, 2),
        `Number of colonies` = numcol,
        `Yield per colony` = yieldpercol,
        `Price per pound` = priceperlb,
        `Production value, million dollars` = round(prodvalue_mil, 2),
        `Stocks, million lb` = round(stocks_mil, 2)
      )
  })
  
  output$national_trend <- renderPlot({
    
    honey_yearly <- honey_clean %>%
      group_by(year) %>%
      summarize(
        totalprod_mil = sum(totalprod_mil, na.rm = TRUE),
        numcol = sum(numcol, na.rm = TRUE),
        yieldpercol = mean(yieldpercol, na.rm = TRUE),
        priceperlb = mean(priceperlb, na.rm = TRUE),
        prodvalue_mil = sum(prodvalue_mil, na.rm = TRUE),
        stocks_mil = sum(stocks_mil, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(
      data = honey_yearly,
      mapping = aes(
        x = year,
        y = .data[[input$selected_variable]]
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      geom_vline(
        xintercept = input$selected_year,
        linetype = "dashed"
      ) +
      scale_x_continuous(
        breaks = sort(unique(honey_yearly$year))
      ) +
      scale_y_continuous(
        labels = label_number()
      ) +
      labs(
        title = paste("National Trend:", variable_label()),
        subtitle = paste("Dashed line shows selected year:", input$selected_year),
        x = "Year",
        y = variable_label()
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$year_summary <- renderTable({
    
    selected_year_data() %>%
      summarize(
        Year = input$selected_year,
        `Total production, million lb` = round(sum(totalprod_mil, na.rm = TRUE), 1),
        `Total colonies` = sum(numcol, na.rm = TRUE),
        `Average yield per colony` = round(mean(yieldpercol, na.rm = TRUE), 1),
        `Average price per lb` = round(mean(priceperlb, na.rm = TRUE), 2),
        `Total production value, million dollars` = round(sum(prodvalue_mil, na.rm = TRUE), 1)
      )
  })
}

shinyApp(ui = ui, server = server)




