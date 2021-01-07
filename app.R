library(shiny)
library(jsonlite)
library(dplyr)
library(tibble)
library(glue)
library(ggplot2)
library(lubridate)
library(stringr)

gyms_tbl <- fromJSON("gyms.json") %>% as_tibble()

gym_choices <- gyms_tbl %>%
    mutate(gym_description = glue("{gym} ({city}, {state})")) %>%
    select(gym_description, id) %>%
    deframe()

gym_counter_history_tbl <- fromJSON("gym_counter_results.json") %>%
    as_tibble() %>%
    inner_join(gyms_tbl, by = c("gym_id" = "id")) %>%
    mutate(accessed_at_utc = as_datetime(accessed_at)) %>%
    rowwise() %>%
    mutate(accessed_at_local = with_tz(accessed_at_utc, local_tz)) %>%
    ungroup() %>%
    mutate(wday_accessed_at_local = wday(accessed_at_local),
           pct_of_capacity = count / capacity)

ui <- fluidPage(
    titlePanel("When's the safest time to climb?"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                inputId = "gym_id_select",
                label = "Choose Gym:",
                choices = gym_choices,
                selected = NULL
            ),
            dateInput("date_select", label = "View data through...", value = Sys.Date())
        ),

        mainPanel(
            verbatimTextOutput("selected_gym_id"),
            verbatimTextOutput("selected_date_wday"),
            plotOutput("occupancy_over_time"),
            plotOutput("average_occupancy_on_day")
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$selected_gym_id <- renderText({
        input$gym_id_select
    })

    selected_gym_counter_history <- reactive({
        gym_counter_history_tbl %>%
            filter(gym_id == input$gym_id_select)
    })

    selected_date_wday <- reactive({
        wday(input$date_select)
    })

    output$selected_date_wday <- renderText({
        selected_date_wday()
    })

    selected_gym_counter_history_on_wday <- reactive({
        selected_gym_counter_history() %>%
            filter(wday_accessed_at_local == selected_date_wday())
    })

    output$occupancy_over_time <- renderPlot({
        selected_gym_counter_history() %>%
            ggplot(aes(x = accessed_at_local, y = pct_of_capacity)) +
            geom_area(fill = "#07bab1") +
            xlab('Datetime') +
            scale_x_datetime(breaks = '6 hours') +
            ylab('Percent of Capacity') +
            scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
            theme_minimal()
    })

    output$average_occupancy_on_day <- renderPlot({
        selected_gym_counter_history_on_wday() %>%
            group_by(hour_of_day = hour(accessed_at_local), minute_of_hour = minute(accessed_at_local)) %>%
            summarize(mean_pct_of_capacity = mean(pct_of_capacity),
                      .groups = 'drop') %>%
            mutate(time_of_day_str = glue("{str_pad(hour_of_day, 2, pad = 0)}:{str_pad(minute_of_hour, 2, pad = 0)}"),
                   time_of_day = ymd_hm(glue("1993-12-07 {time_of_day_str}"))
                   )%>%
            ggplot(aes(x = time_of_day, y = mean_pct_of_capacity)) +
            geom_area(fill = "#07bab1") +
            xlab('Time') +
            scale_x_datetime(date_labels = "%H:%M", breaks = '30 min') +
            ylab('Percent of Capacity') +
            scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
            theme_minimal()
    })
}

# Run the application
shinyApp(ui = ui, server = server)

# shiny::reactlogShow()
