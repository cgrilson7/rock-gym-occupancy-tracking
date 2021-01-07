library(shiny)
library(jsonlite)
library(dplyr)
library(tibble)
library(glue)
library(ggplot2)
library(lubridate)
library(stringr)
library(shinyWidgets)
library(curl)

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
    mutate(
        wday_accessed_at_local = wday(accessed_at_local),
        pct_of_capacity = count / capacity
    )

ui <- fluidPage(
    titlePanel("When's the safest time to climb?"),
            selectizeInput(
                inputId = "gym_id_select",
                label = "Choose Gym:",
                choices = gym_choices,
                selected = NULL),
            # verbatimTextOutput("selected_gym_id"),
            # verbatimTextOutput("selected_wday"),
            h1("Average Headcount by Hour, by Day of Week:"),
            radioGroupButtons(
                "new_wday_select",
                label = "",
                choices = c(
                    "Sunday" = 1,
                    "Monday" = 2,
                    "Tuesday" = 3,
                    "Wednesday" = 4,
                    "Thursday" = 5,
                    "Friday" = 6,
                    "Saturday" = 7
                ),
                status = "success",
                selected = wday(Sys.Date())
            ),
            plotOutput("average_occupancy_on_day"),
            h1("Recent Headcounts"),
            radioGroupButtons("n_days_back", label = "", choices = c("3D" = 3, "7D" = 7, "14D" = 14, "30D" = 30), selected = 3),
            plotOutput("occupancy_over_time")
)


server <- function(input, output) {
    selected_wday <- reactiveVal(value = wday(Sys.Date()))

    observeEvent(input$new_wday_select,
                 {
                     selected_wday(input$new_wday_select)
                 })

    output$selected_gym_id <- renderText({
        input$gym_id_select
    })

    selected_gym_counter_history <- reactive({
        gym_counter_history_tbl %>%
            filter(gym_id == input$gym_id_select)
    })

    output$selected_wday <- renderText({
        selected_wday()
    })

    selected_gym_counter_history_on_wday <- reactive({
        selected_gym_counter_history() %>%
            filter(wday_accessed_at_local == selected_wday())
    })

    output$occupancy_over_time <- renderPlot({

        latest_accessed_at_local = max(selected_gym_counter_history() %>% pull(accessed_at_local))

        n_days_back <- input$n_days_back

        selected_gym_counter_history() %>%
            ggplot(aes(x = accessed_at_local, y = pct_of_capacity)) +
            # geom_line(size = 5, lineend="round") +
            # scale_color_viridis_c(option = 'inferno') +
            geom_area(fill = "#07bab1") +
            xlab('Date') +
            scale_x_datetime(limits = c(latest_accessed_at_local - days(n_days_back), latest_accessed_at_local), date_labels = '%b %d, %H%p') +
            ylab('Percent of Capacity') +
            scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
            theme_minimal() +
            theme(axis.text.x = element_text(face = 'bold'))
    })

    output$average_occupancy_on_day <- renderPlot({
        selected_gym_counter_history_on_wday() %>%
            group_by(
                hour_of_day = hour(accessed_at_local),
                minute_of_hour = minute(accessed_at_local)
            ) %>%
            summarize(mean_pct_of_capacity = mean(pct_of_capacity),
                      .groups = 'drop') %>%
            mutate(
                time_of_day_str = glue(
                    "{str_pad(hour_of_day, 2, pad = 0)}:{str_pad(minute_of_hour, 2, pad = 0)}"
                ),
                time_of_day = ymd_hm(glue("1993-12-07 {time_of_day_str}"))
            ) %>%
            ggplot(aes(x = time_of_day, y = mean_pct_of_capacity)) +
            # geom_line(size = 5, lineend="round") +
            # scale_color_viridis_c(option = 'inferno') +
            # scale_color_brewer(palette = "RdYlGn") +
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

