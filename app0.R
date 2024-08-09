library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(foreign)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(scales)
library(ggpubr)
library(stringr)
library(tidyverse)
library(cowplot)

experiment5_info_box <- function() {
  box(
    title = "Experiment 5 Information",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    HTML(
      paste(
        "<b>Day 1:</b> 1/13/2023<br>",
        "<b>Last Day:</b> 3/7/2023 (Day 55)<br>",
        "<b>Stress Day ON:</b> NONE<br>",
        "<b>Stress Day OFF:</b> NONE<br>",
        "<b>Stress Mice:</b> NONE<br>",
        "<b>Lights ON:</b> 5:00 UTC (12:00 AM EST)<br>",
        "<b>Light Swap:</b> 2/10/2023 (Day 28)<br>",
        "<b>Light Swap Mice:</b> BB01, BB03, BB06, BB09, BB11, BB12, BB14, and BB15"
      )
    )
  )
}

experiment6_info_box <- function() {
  box(
    title = "Experiment 6 Information",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    HTML(
      paste(
        "<b>Day 1:</b> 7/21/2023<br>",
        "<b>Last Day:</b> 9/9/2023<br>",
        "<b>Stress Day ON:</b> 8/12/2023<br>",
        "<b>Stress Day OFF:</b> 8/26/2023<br>",
        "<b>Stress Mice:</b> BB01, BB03, BB06, BB09, BB11, BB12, BB14, and BB15<br>",
        "<b>Lights ON:</b> 11:00 UTC (7:00 AM EST)<br>",
        "<b>Light Swap:</b> NONE<br>",
        "<b>Light Swap Mice:</b> NONE"
      )
    )
  )
}

create_arrow_plot <- function(mouse_data, var_name, date_range, circadian, flag, exp_select, selected_mode) {
  days <- unique(mouse_data$date)
  num_days <- length(days)
  
  mouse_data <- mouse_data %>%
    arrange(date) %>%
    mutate(label = row_number()) %>%
    filter(label >= date_range[1] & label <= date_range[2])
  
  mouse_label <- unique(mouse_data$Mouse)
  mode_label <- selected_mode
  
  if (exp_select == "Exp5") {
    light_swap_mice <- c("BB01", "BB03", "BB06", "BB09", "BB11", "BB12", "BB14", "BB15")
    if (mouse_label %in% light_swap_mice) {
      status_label <- "Light Swap"
    } else {
      status_label <- "No Light Swap"
    }
  } else if (exp_select == "Exp6") {
    stressed_mice <- c("BB01", "BB03", "BB06", "BB09", "BB11", "BB12", "BB14", "BB15")
    if (mouse_label %in% stressed_mice) {
      status_label <- "Stressed"
    } else {
      status_label <- "Not Stressed"
    }
  }
  
  title <- paste(mode_label, "-", mouse_label, "(", status_label, ")[Arrow]")
  subtitle <- paste("Circadian:", paste(circadian, collapse = ", "), "Flag:", paste(flag, collapse = ", "), "Days:", date_range[1], "-", date_range[2])
  
  ggplot(mouse_data) +
    geom_segment(aes(x = 0, y = 0, xend = 10, yend = !!sym(var_name), color = label),
                 arrow = arrow(length = unit(0.3, "cm")), show.legend = TRUE) +
    geom_text(aes(x = 10.5, y = !!sym(var_name), label = label, color = label),
              vjust = -0.5, hjust = 0, size = 3, show.legend = FALSE) +
    scale_color_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = median(mouse_data$label)) +
    theme_minimal() +
    labs(title = title,
         subtitle = subtitle,
         x = "", y = "", color = "Date") +
    theme(plot.title = element_text(hjust = 0.5, size = 15), # Adjust the size as needed
          plot.subtitle = element_text(hjust = 0.5, size = 10), # Adjust the size as needed
          legend.key.size = unit(3, 'cm'),
          legend.title = element_text(size = 15))
}

create_bar_plot <- function(mouse_data, var_name, date_range, circadian, flag, exp_select, selected_mode) {
  mouse_data <- mouse_data %>%
    arrange(date) %>%
    mutate(label = row_number()) %>%
    filter(label >= date_range[1] & label <= date_range[2])
  
  mouse_label <- unique(mouse_data$Mouse)
  mode_label <- selected_mode
  
  if (exp_select == "Exp5") {
    light_swap_mice <- c("BB01", "BB03", "BB06", "BB09", "BB11", "BB12", "BB14", "BB15")
    if (mouse_label %in% light_swap_mice) {
      status_label <- "Light Swap"
    } else {
      status_label <- "No Light Swap"
    }
  } else if (exp_select == "Exp6") {
    stressed_mice <- c("BB01", "BB03", "BB06", "BB09", "BB11", "BB12", "BB14", "BB15")
    if (mouse_label %in% stressed_mice) {
      status_label <- "Stressed"
    } else {
      status_label <- "Not Stressed"
    }
  }
  
  title <- paste(mode_label, "-", mouse_label, "(", status_label, ")[Bar]")
  subtitle <- paste("Circadian:", paste(circadian, collapse = ", "), "Flag:", paste(flag, collapse = ", "), "Days:", date_range[1], "-", date_range[2])
  
  if (grepl("Water", var_name, ignore.case = TRUE)) {
    interval <- 3
    interval1 <- NULL
    interval2 <- NULL
  } else if (grepl("Food", var_name, ignore.case = TRUE)) {
    interval <- 7
    interval1 <- NULL
    interval2 <- NULL
  } else if (grepl("Wheel", var_name, ignore.case = TRUE)) {
    interval <- NULL
    interval1 <- 3
    interval2 <- 7
  }
  else {
    interval <- NULL
    interval1 <- NULL
    interval2 <- NULL
  }
  
  p <- ggplot(mouse_data, aes(x = label, y = !!sym(var_name), fill = label)) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = median(mouse_data$label)) +
    theme_minimal() +
    labs(title = title,
         subtitle = subtitle,
         x = "", y = "", color = "Date") +
    theme(plot.title = element_text(hjust = 0.5, size = 15), # Adjust the size as needed
          plot.subtitle = element_text(hjust = 0.5, size = 10), # Adjust the size as needed
          legend.key.size = unit(3, 'cm'),
          legend.title = element_text(size = 15))
  
  if (!is.null(interval)) {
    p <- p + geom_vline(xintercept = mouse_data$label[mouse_data$label %% interval == 0], color = "green", linetype = "dashed")
  }
  if (!is.null(interval1)) {
    p <- p + geom_vline(xintercept = mouse_data$label[mouse_data$label %% interval1 == 0], color = "blue", linetype = "dotted")
  }
  if (!is.null(interval2)) {
    p <- p + geom_vline(xintercept = mouse_data$label[mouse_data$label %% interval2 == 0], color = "red", linetype = "dashed")
  }
  
  return(p)
}

compute_and_plot_z <- function(data, mode, mouse_id, date_range, retain, circadian, flag, exp_select, selected_mode) {
  mouse_data <- data %>%
    group_by(Mouse) %>%
    mutate(label = row_number()) %>%
    filter(label >= date_range[1] & label <= date_range[2]) %>%
    ungroup()
  
  suffix <- switch(mode,
                   "sum" = "_sum$",
                   "median" = "_median$",
                   "mean" = "_mean$")
  
  selected_vars <- grep(suffix, names(mouse_data), value = TRUE)
  
  combined_data_z <- mouse_data %>%
    group_by(Mouse) %>%
    summarise(across(all_of(selected_vars),
                     ~ mean(., na.rm = TRUE),
                     .names = "mean_{col}")) %>%
    ungroup()
  
  if (nrow(combined_data_z) > 1) {
    combined_data_z <- combined_data_z %>%
      mutate(across(starts_with("mean"),
                    ~ (.-mean(., na.rm = TRUE))/sd(., na.rm = TRUE),
                    .names = "z_{col}"))
  } else {
    combined_data_z <- combined_data_z %>%
      mutate(across(starts_with("mean"),
                    ~ 0,
                    .names = "z_{col}"))
  }
  
  z_score_long_combined <- combined_data_z %>%
    select(Mouse, starts_with("z_")) %>%
    tidyr::pivot_longer(-Mouse, names_to = "variable", values_to = "z_score")
  
  # Trim the variable names
  z_score_long_combined <- z_score_long_combined %>%
    mutate(variable = gsub("z_mean_|_sum$|_median$|_mean$", "", variable))
  
  dataz <- z_score_long_combined %>%
    filter(Mouse == mouse_id)
  
  mouse_label <- mouse_id
  mode_label <- selected_mode
  
  if (exp_select == "Exp5") {
    light_swap_mice <- c("BB01", "BB03", "BB06", "BB09", "BB11", "BB12", "BB14",
                         "BB15")
    if (mouse_label %in% light_swap_mice) {
      status_label <- "Light Swap"
    } else {
      status_label <- "No Light Swap"
    }
  } else if (exp_select == "Exp6") {
    stressed_mice <- c("BB01", "BB03", "BB06", "BB09", "BB11", "BB12", "BB14", 
                       "BB15")
    if (mouse_label %in% stressed_mice) {
      status_label <- "Stressed"
    } else {
      status_label <- "Not Stressed"
    }
  }
  
  title <- paste(mode_label, "-", mouse_label, "(", status_label, ")[", mode, "]")
  subtitle <- paste("Circadian:", paste(circadian, collapse = ", "), "Flag:", paste(flag, collapse = ", "), "Days:", date_range[1], "-", date_range[2])
  
  if (retain == "false") {
    ggplot(dataz, aes(x = variable,
                      y = z_score,
                      fill = z_score)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               na.rm = TRUE) +
      coord_flip() +
      theme_minimal() +
      ylim(-3, 3) +
      labs(title = title,
           subtitle = subtitle,
           x = "Variable",
           y = "Z-Score") +
      scale_fill_gradientn(colors = c("blue", "purple", "yellow", "red")) +
      theme(plot.title = element_text(hjust = 0.5, size = 15), # Adjust the size as needed
            plot.subtitle = element_text(hjust = 0.5, size = 10)) # Adjust the size as needed
  } else {
    ggplot(dataz, aes(x = variable,
                      y = z_score,
                      fill = z_score)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               na.rm = TRUE) +
      coord_flip() +
      theme_minimal() +
      ylim(-3, 3) +
      labs(title = title,
           subtitle = subtitle,
           x = "Variable",
           y = "Z-Score") +
      scale_fill_gradientn(colors = c("blue", "purple", "yellow", "red")) +
      theme(plot.title = element_text(hjust = 0.5, size = 15), # Adjust the size as needed
            plot.subtitle = element_text(hjust = 0.5, size = 10), # Adjust the size as needed
            axis.text.y = element_blank(),
            axis.title.y = element_blank())
  }
}

adjust_plot_for_grid <- function(plot, is_first_column) {
  if (is_first_column) {
    plot + theme(
      plot.margin = unit(c(1, 0.5, 1, 0.1), "cm"),  # Increased margin for the first column
      axis.text.y = element_text(size = 6),
      axis.title.y = element_text(size = 7),
      plot.title = element_text(size = 15, hjust = 0.5), # Adjusted the size
      plot.subtitle = element_text(size = 10, hjust = 0.5) # Adjusted the size
    )
  } else {
    plot + theme(
      plot.margin = unit(c(1, 0.5, 1, 5), "cm"),  # Standard margin for other columns
      axis.text.y = element_blank(),  # Remove y-axis text
      axis.title.y = element_blank(),  # Remove y-axis title
      plot.title = element_text(size = 15, hjust = 0.5), # Adjusted the size
      plot.subtitle = element_text(size = 10, hjust = 0.5) # Adjusted the size
    )
  }
}

create_multi_mouse_plot <- function(unfiltered_data, data, mode, var_stem, plot_type,
                                    z_mode_select, date_range, retain, circadian, flag, exp_select, selected_mode) {
  mouse_ids <- unique(data$Mouse)
  ncols <- 4  # Number of columns in the grid
  
  plots <- lapply(seq_along(mouse_ids), function(i) {
    mouse_id <- mouse_ids[i]
    is_first_column <- (i - 1) %% ncols == 0  # Check if the plot is in the first column
    
    plot <- NULL
    if (mode == "z") {
      plot <- compute_and_plot_z(unfiltered_data, z_mode_select, mouse_id, 
                                 date_range, retain, circadian, flag, 
                                 exp_select, selected_mode)
    } else {
      mouse_data <- data %>% filter(Mouse == mouse_id)
      if (plot_type == "arrow") {
        plot <- create_arrow_plot(mouse_data, var_stem, date_range, circadian, 
                                  flag, exp_select, selected_mode)
      } else {
        plot <- create_bar_plot(mouse_data, var_stem, date_range, circadian, 
                                flag, exp_select, selected_mode)
      }
    }
    
    adjust_plot_for_grid(plot, is_first_column)
  })
  
  plot_grid <- ggpubr::ggarrange(plotlist = plots, ncol = 4, nrow = 4,
                                 common.legend = TRUE, legend = "top")
  
  return(plot_grid)
}











#############################UI##########################################

ui <- fluidPage(
  titlePanel("Mouse Data Plot"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      selectInput("exp_select", "Exp Select", 
                  choices = c("Exp5", "Exp6")),
      
      selectInput("mode_select", "Mode Select", 
                  choices = c("Show Z" = "z",
                              "Display Mouse" = "mouse"
                  )),
      conditionalPanel(
        condition = "input.mode_select == 'z'",
        selectInput("z_mode_select", "Select Z Mode", 
                    choices = c("Sum" = "sum", "Median" = "median", "Mean" = "mean"))
      ),
      conditionalPanel(
        condition = "input.mode_select != 'z'",
        selectInput("plot_type", "Select Plot Type", 
                    choices = c("Arrow Plot" = "arrow", "Bar Plot" = "bar")),
        uiOutput("var_select_ui")
      ),
      
      uiOutput("mouse_select_ui"),
      selectInput(
        inputId = "circadian",
        label = "Select Circadian Period:",
        choices = c("Day", "Night"),
        selected = c("Day", "Night"),
        multiple = TRUE
      ),
      selectInput(
        inputId = "flag",
        label = "Select Flag:",
        choices = c("Zero", "Non-Zero"),
        selected = c("Zero", "Non-Zero"),
        multiple = TRUE
      ),
      
      tags$style(type = "text/css", 
                 ".irs-grid-text:nth-of-type(n+18):nth-of-type(-n+32) {color: red; font-weight: bold; font-size: 12px;}",
                 ".irs-grid-pol:nth-of-type(n+18):nth-of-type(-n+32) {background: red;}"
      ),
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      
      conditionalPanel(
        condition = "input.exp_select == 'Exp6'",
        sliderInput("date_range_6", "Select Date Range",
                    min = 1, max = 50, value = c(22, 36), step = 1,
                    width = '100%')
      ),
      conditionalPanel(
        condition = "input.exp_select == 'Exp5'",
        sliderInput("date_range_5", "Select Date Range",
                    min = 1, max = 55, value = c(28, 55), step = 1,
                    width = '100%')
      ),
      
      actionButton("retain_plot_button", "Retain Plot"),
      actionButton("clear_retained_plots_button", "Clear Retained Plots"),
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton("downloadData", "Download Data"),
      uiOutput("experiment_info")
    ),
    
    mainPanel(
      
      fluidRow(
        column(12,
               plotOutput("plot", width = "1400", height = "1400")
        )
      ),
      absolutePanel(
        id = "retained_plot_panel",
        top = 850,
        left = 0,
        draggable = TRUE,
        plotOutput("retained_plot", width = "1600px", height = "1600px")
      )
    ),
    position = "left" 
  )
)


















#############################SE##########################################
server <- function(input, output, session) {
  output$experiment_info <- renderUI({
    exp_select <- input$exp_select
    if (exp_select == "Exp5") {
      experiment5_info_box()
    } else if (exp_select == "Exp6") {
      experiment6_info_box()
    }
  })
  
  exp_reactive <- reactive({
    input$exp_select
  })
  
  analog <- reactive({
    exp <- exp_reactive()
    if (exp == "Exp6") {
      read.csv("Exp6_analog_all_HR_new.csv")
    } else if (exp == "Exp5") {
      read.csv("Exp5_analog_all_HR_new.csv")
    }
  })
  
 
  
  digital <- reactive({
    exp <- exp_reactive()
    if (exp == "Exp6") {
      read.csv("Exp6_digital_all_HR_new.csv")
    } else if (exp == "Exp5") {
      data <- read.csv("Exp5_digital_all_HR_new.csv")
      data$Mouse <- ifelse(grepl("^BB0[1-9]$", data$Mouse), 
                           data$Mouse, 
                           gsub("^BB0([1-9][0-9])$", "BB\\1", data$Mouse))
      return(data)
    }
  })
  
  circadian_reactive <- reactive({
    input$circadian
  })
  
  flag_reactive <- reactive({
    input$flag
  })
  
  ana <- reactive({
    circadian <- circadian_reactive()
    flag <- flag_reactive()
    analog_data <- analog()
    
    if (all(c("Day", "Night") %in% circadian) & 
        all(c("Zero", "Non-Zero") %in% flag)) {
      filtered_data <- analog_data
    } else if ("Day" %in% circadian & 
               "Zero" %in% flag) {
      filtered_data <- analog_data %>%
        filter(entry_flag == 0, circadian == 0, !is.na(day))
    } else if ("Night" %in% circadian &
               "Zero" %in% flag) {
      filtered_data <- analog_data %>%
        filter(entry_flag == 0, circadian == 1, !is.na(day))
    } else if ("Day" %in% circadian & 
               "Non-Zero" %in% flag) {
      filtered_data <- analog_data %>%
        filter(entry_flag != 0, circadian == 0, !is.na(day))
    } else if ("Night" %in% circadian &
               "Non-Zero" %in% flag) {
      filtered_data <-  analog_data %>%
        filter(entry_flag != 0, circadian == 1, !is.na(day))
    }
    
    filtered_data %>%
      group_by(Mouse, day) %>%
      mutate(Wheel_Beambreak_CM_sum = sum(Wheel_Beambreak_CM, na.rm = TRUE),
             Wheel_Beambreak_CM_mean = mean(Wheel_Beambreak_CM, na.rm = TRUE),
             Wheel_Beambreak_CM_median = median(Wheel_Beambreak_CM, na.rm = TRUE)) %>%
      select(Mouse, starts_with("Wheel_Beambreak_CM_")) %>%
      ungroup() %>%
      distinct(Wheel_Beambreak_CM_sum, .keep_all = TRUE)
  })
  
  variables <- c("Regular_Water_Beambreak", "Sucrose_Water_Beambreak", "Regular_Food_Beambreak", 
                 "Fatty_Food_Beambreak", "Regular_Water_Dispense", "Sucrose_Water_Dispense", 
                 "Regular_Food_Dispense", "Fatty_Food_Dispense")
  
  process_variable <- function(data, var) {
    data %>%
      group_by(Mouse, day) %>%
      mutate(!!paste0(var, "_sum") := sum(.data[[var]], na.rm = TRUE),
             !!paste0(var, "_mean") := mean(.data[[var]], na.rm = TRUE),
             !!paste0(var, "_median") := median(.data[[var]], na.rm = TRUE)) %>%
      ungroup()
  }
  
  digi <- reactive({
    circadian <- circadian_reactive()
    flag <- flag_reactive()
    digital_data <- digital()
    
    if (all(c("Day", "Night") %in% circadian) & 
        all(c("Zero", "Non-Zero") %in% flag)) {
      filtered_data <- digital_data
    }
    else if ("Day" %in% circadian & 
             "Zero" %in% flag) {
      filtered_data <- digital_data %>%
        filter(entry_flag == 0, circadian == 0, !is.na(day))
    } else if ("Night" %in% circadian &
               "Zero" %in% flag) {
      filtered_data <- digital_data %>%
        filter(entry_flag == 0, circadian == 1, !is.na(day))
    } else if ("Day" %in% circadian & 
               "Non-Zero" %in% flag) {
      filtered_data <- digital_data %>%
        filter(entry_flag != 0, circadian == 0, !is.na(day))
    } else if ("Night" %in% circadian &
               "Non-Zero" %in% flag) {
      filtered_data <-  digital_data %>%
        filter(entry_flag != 0, circadian == 1, !is.na(day))
    }
    
    for (var in variables) {
      filtered_data <- process_variable(filtered_data, var)
    }
    
    filtered_data %>%
      select(Mouse, day, starts_with("Regular_Water_Beambreak_"), 
             starts_with("Sucrose_Water_Beambreak_"), starts_with("Regular_Food_Beambreak_"), 
             starts_with("Fatty_Food_Beambreak_"), starts_with("Regular_Water_Dispense_"), 
             starts_with("Sucrose_Water_Dispense_"), starts_with("Regular_Food_Dispense_"), 
             starts_with("Fatty_Food_Dispense_")) %>%
      distinct(Regular_Water_Beambreak_sum, .keep_all = TRUE)
  })
  
  data <- reactive({
    ana_data <- ana()
    digi_data <- digi()
    
    ana_data$day <- as.numeric(gsub("D", "", as.character(ana_data$day)))
    digi_data$day <- as.numeric(gsub("D", "", as.character(digi_data$day)))
    
    data <- right_join(ana_data, digi_data, by = c("Mouse", "day"))
    all_days <- union(ana_data$day, digi_data$day)
    data %>%
      group_by(Mouse) %>%
      complete(day = all_days) %>%
      mutate(across(everything(), ~ replace_na(., 0))) %>%
      arrange(day) %>%
      rename(date = day) %>%
      ungroup()
  })
  
  output$var_select_ui <- renderUI({
    variables <- unique(setdiff(names(data()), c("Mouse", "date")))
    selectInput("var_stem", "Select Variable", choices = variables)
  })
  
  output$mouse_select_ui <- renderUI({
    mouse_ids <- unique(data()$Mouse)
    exp <- exp_reactive()
    stressed_mouse_ids <- c(1, 3, 6, 9, 11, 12, 14, 15)
    mouse_labels <- if ("Exp5" %in% exp) {
      ifelse(mouse_ids %in% paste0("BB", sprintf("%02d", stressed_mouse_ids)),
             paste(mouse_ids, "(light swap)"),
             mouse_ids)
    } else {
      ifelse(mouse_ids %in% paste0("BB", sprintf("%02d", stressed_mouse_ids)),
             paste(mouse_ids, "(stressed)"),
             mouse_ids)
    }
    
    pickerInput("mouse_id", "Select Mouse ID",
                choices = setNames(mouse_ids, mouse_labels),
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  selectedTextFormat = "count > 10",
                  deselectAllText = "Deselect All",
                  selectAllText = "Select All"
                ))
  })
  
  observeEvent(data(), {
    selected_mouse_ids <- isolate(input$mouse_id)
    mouse_ids <- unique(data()$Mouse)
    exp <- exp_reactive()
    stressed_mouse_ids <- c(1, 3, 6, 9, 11, 12, 14, 15)
    mouse_labels <- if ("Exp5" %in% exp) {
      ifelse(mouse_ids %in% paste0("BB", sprintf("%02d", stressed_mouse_ids)),
             paste(mouse_ids, "(light swap)"),
             mouse_ids)
    } else {
      ifelse(mouse_ids %in% paste0("BB", sprintf("%02d", stressed_mouse_ids)),
             paste(mouse_ids, "(stressed)"),
             mouse_ids)
    }
    
    updatePickerInput(session, "mouse_id", choices = setNames(mouse_ids, mouse_labels), 
                      selected = selected_mouse_ids)
  })
  
  selected_date_range <- reactive({
    if (input$exp_select == "Exp6") {
      return(input$date_range_6)
    } else if (input$exp_select == "Exp5") {
      return(input$date_range_5)
    }
  })
  
  filtered_data <- reactive({
    req(input$mouse_id)
    data() %>%
      filter(Mouse %in% input$mouse_id)
  })
  
  retained_plots <- reactiveVal(list())
  
  output$plot <- renderPlot({
    req(input$var_stem, input$plot_type)
    
    plot_with_legend <- create_multi_mouse_plot(data(),
                                                filtered_data(),
                                                input$mode_select,
                                                input$var_stem,
                                                input$plot_type,
                                                input$z_mode_select,
                                                selected_date_range(),
                                                "false",
                                                input$circadian,
                                                input$flag,
                                                input$exp_select,
                                                input$mode_select)
    plot_with_legend
  })
  
  observeEvent(input$retain_plot_button, {
    new_plot <- create_multi_mouse_plot(data(),
                                        filtered_data(),
                                        input$mode_select,
                                        input$var_stem,
                                        input$plot_type,
                                        input$z_mode_select,
                                        selected_date_range(),
                                        "true",
                                        input$circadian,
                                        input$flag,
                                        input$exp_select,
                                        input$mode_select)
    current_plots <- retained_plots()
    retained_plots(c(current_plots, list(new_plot)))
  })
  
  observeEvent(input$clear_retained_plots_button, {
    retained_plots(list())
  })
  
  adjust_plot_for_grid <- function(plot, is_first_column) {
    if (is_first_column) {
      plot + theme(
        plot.margin = unit(c(1, 0.5, 1, 0.1), "cm"),  # Increased margin for the first column
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 20, hjust = 0.5), # Adjusted the size
        plot.subtitle = element_text(size = 10, hjust = 0.5) # Adjusted the size
      )
    } else {
      plot + theme(
        plot.margin = unit(c(1, 0.5, 1, 5), "cm"),  # Standard margin for other columns
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.y = element_blank(),  # Remove y-axis title
        plot.title = element_text(size = 20, hjust = 0.5), # Adjusted the size
        plot.subtitle = element_text(size = 10, hjust = 0.5) # Adjusted the size
      )
    }
  }
  
  output$retained_plot <- renderPlot({
    if (length(retained_plots()) > 0) {
      # Apply margin adjustments to each plot
      adjusted_plots <- lapply(retained_plots(), function(plot) {
        plot + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
      })
      
      # Arrange the plots with spacing
      plot_grid <- ggpubr::ggarrange(
        plotlist = adjusted_plots,
        ncol = 1,  # Each row will contain only 1 plot
        nrow = length(adjusted_plots),  # Number of rows is equal to the number of plots
        heights = rep(1, length(adjusted_plots)),  # Adjust heights if needed
        common.legend = TRUE,
        legend = "top"
      )
      
      # Adjust grid spacing
      grid.draw(plot_grid)
    }
  })
  
  #############################download####################################
 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("combined_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      filename <- paste0(input$var_stem, "_", input$plot_type, ".png")
      return(filename)
    },
    content = function(file) {
      png(file, width = 980, height = 900)
      plot <- create_multi_mouse_plot(data(),
                                      filtered_data(), 
                                      input$mode_select,
                                      input$var_stem,
                                      input$plot_type,
                                      input$z_mode_select,
                                      selected_date_range(),
                                      "false",
                                      input$circadian,
                                      input$flag,
                                      input$exp_select,
                                      input$mode_select)
      print(plot)
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)