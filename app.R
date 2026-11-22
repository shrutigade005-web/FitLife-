# FitLife - Smart Daily Calorie & Exercise Manager
# Run with: shiny::runApp("FitLife")
# All app logic is implemented in R Shiny.

required_packages <- c(
  "shiny", "bslib", "ggplot2", "plotly", "dplyr", "tidyr",
  "DT", "readr", "DBI", "RSQLite"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))."
  )
}

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(DBI)
library(RSQLite)

APP_DIR <- getwd()
DB_DIR <- file.path(APP_DIR, "database")
if (!dir.exists(DB_DIR)) dir.create(DB_DIR, recursive = TRUE)
DB_PATH <- file.path(DB_DIR, "fitlife.sqlite")

con <- dbConnect(RSQLite::SQLite(), DB_PATH)
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS food_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    entry_date TEXT NOT NULL,
    meal TEXT NOT NULL,
    food TEXT NOT NULL,
    quantity REAL NOT NULL,
    calories REAL NOT NULL,
    protein REAL DEFAULT 0,
    carbs REAL DEFAULT 0,
    fat REAL DEFAULT 0
  )
")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS exercise_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    entry_date TEXT NOT NULL,
    activity TEXT NOT NULL,
    duration REAL NOT NULL,
    intensity TEXT NOT NULL,
    met REAL NOT NULL,
    calories_burned REAL NOT NULL
  )
")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS weight_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    entry_date TEXT NOT NULL,
    weight REAL NOT NULL
  )
")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS profile (
    id INTEGER PRIMARY KEY CHECK (id = 1),
    nickname TEXT, age INTEGER, sex TEXT, height REAL, weight REAL,
    activity TEXT, goal TEXT
  )
")

onStop(function() {
  try(dbDisconnect(con), silent = TRUE)
})

activity_factors <- c(
  "Sedentary (little exercise)" = 1.2,
  "Light (1-3 days/week)" = 1.375,
  "Moderate (3-5 days/week)" = 1.55,
  "Active (6-7 days/week)" = 1.725,
  "Very active (physical job/training)" = 1.9
)
activity_mets <- c(
  "Walking" = 3.5, "Running" = 8.0, "Cycling" = 6.8,
  "Yoga" = 2.5, "Swimming" = 7.0, "Gym workout" = 5.0,
  "Dancing" = 5.5, "Hiking" = 6.0, "Other" = 3.5
)

read_table <- function(table) {
  dbReadTable(con, table)
}
safe_num <- function(x, default = 0) {
  y <- suppressWarnings(as.numeric(x))
  ifelse(is.na(y) | !is.finite(y), default, y)
}
calc_bmi <- function(weight, height_cm) {
  if (is.na(weight) || is.na(height_cm) || weight <= 0 || height_cm <= 0) return(NA_real_)
  weight / (height_cm / 100)^2
}
calc_bmr <- function(age, sex, weight, height_cm) {
  if (any(!is.finite(c(age, weight, height_cm))) || age <= 0 || weight <= 0 || height_cm <= 0) return(NA_real_)
  # Mifflin-St Jeor; sex selection is used only for the equation constant.
  if (identical(sex, "Male")) {
    10 * weight + 6.25 * height_cm - 5 * age + 5
  } else {
    10 * weight + 6.25 * height_cm - 5 * age - 161
  }
}
calc_tdee <- function(bmr, activity) {
  factor <- unname(activity_factors[activity])
  if (!length(factor) || is.na(factor) || !is.finite(bmr)) return(NA_real_)
  bmr * factor
}
bmi_category <- function(bmi) {
  if (!is.finite(bmi)) return("Not available")
  if (bmi < 18.5) "Below the usual adult range"
  else if (bmi < 25) "Within the usual adult range"
  else if (bmi < 30) "Above the usual adult range"
  else "High BMI range"
}
read_profile <- function() {
  p <- dbReadTable(con, "profile")
  if (nrow(p)) as.list(p[1, , drop = FALSE]) else NULL
}
read_logs <- function() {
  list(food = read_table("food_log"), exercise = read_table("exercise_log"),
       weight = read_table("weight_log"))
}
date_filter <- function(x, date_col = "entry_date", from, to) {
  if (!nrow(x)) return(x)
  x %>% filter(.data[[date_col]] >= as.character(from),
               .data[[date_col]] <= as.character(to))
}

theme <- bs_theme(
  version = 5, bootswatch = "minty",
  primary = "#237A57", secondary = "#64748b",
  base_font = font_google("Inter")
)

ui <- page_navbar(
  title = div(span("Fit", style = "font-weight:800;color:#237A57;"),
              span("Life", style = "font-weight:800;")),
  theme = theme,
  header = tags$head(
    tags$style(HTML("
      body { background:#f6f9f7; }
      .metric-card { border:0; border-radius:16px; box-shadow:0 3px 14px rgba(30,70,50,.07); }
      .metric-value { font-size:1.8rem; font-weight:750; color:#176b4b; }
      .muted { color:#64748b; }
      .section-title { font-weight:750; color:#1f3d31; }
      .navbar { box-shadow:0 2px 10px rgba(0,0,0,.04); }
      .btn-primary { background:#237a57; border-color:#237a57; }
      .btn-primary:hover { background:#185b40; border-color:#185b40; }
    "))
  ),
  nav_panel("Dashboard", icon = icon("gauge-high"),
    layout_column_wrap(
      width = 1/4,
      value_box("Calories consumed", textOutput("dash_intake", inline = TRUE),
                showcase = icon("utensils"), theme = "success"),
      value_box("Calories burned", textOutput("dash_burned", inline = TRUE),
                showcase = icon("fire"), theme = "warning"),
      value_box("Estimated calorie balance", textOutput("dash_balance", inline = TRUE),
                showcase = icon("scale-balanced"), theme = "primary"),
      value_box("Exercise today", textOutput("dash_minutes", inline = TRUE),
                showcase = icon("person-running"), theme = "info")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Today's overview"),
           plotlyOutput("today_chart", height = "300px")),
      card(card_header("Daily guidance"),
           uiOutput("daily_guidance"),
           tags$hr(),
           uiOutput("profile_snapshot"))
    ),
    card(card_header("Recent food entries"),
         DTOutput("recent_food"))
  ),
  nav_panel("My Profile", icon = icon("user"),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Personal information"),
        textInput("nickname", "Name or nickname", value = ""),
        numericInput("age", "Age (years)", value = 25, min = 13, max = 110),
        selectInput("sex", "Sex used for BMR equation",
                    choices = c("Female", "Male"), selected = "Female"),
        numericInput("height", "Height (cm)", value = 165, min = 80, max = 250),
        numericInput("weight", "Current weight (kg)", value = 65, min = 20, max = 350),
        selectInput("activity", "Usual activity level", choices = names(activity_factors)),
        selectInput("goal", "Fitness goal",
                    choices = c("Maintain weight", "Lose weight gradually", "Gain weight gradually")),
        actionButton("save_profile", "Save profile", class = "btn-primary")
      ),
      card(card_header("Your estimated health metrics"),
        uiOutput("health_metrics"),
        p(class = "muted",
          "BMI, BMR and TDEE are estimates for general information, not medical advice. ",
          "The BMR equation offers two sex-based constants and may not fit every individual.")
      )
    )
  ),
  nav_panel("Food Tracker", icon = icon("apple-whole"),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Add a food entry"),
        dateInput("food_date", "Date", value = Sys.Date()),
        selectInput("meal", "Meal", c("Breakfast", "Lunch", "Dinner", "Snacks")),
        textInput("food_name", "Food name", placeholder = "e.g. Idli, rice, apple"),
        numericInput("food_quantity", "Quantity / servings", 1, min = 0.1, max = 100, step = 0.1),
        numericInput("food_calories", "Calories for this entry (kcal)", 0, min = 0, max = 10000),
        fluidRow(
          column(4, numericInput("food_protein", "Protein (g)", 0, min = 0)),
          column(4, numericInput("food_carbs", "Carbs (g)", 0, min = 0)),
          column(4, numericInput("food_fat", "Fat (g)", 0, min = 0))
        ),
        actionButton("add_food", "Add food", class = "btn-primary")
      ),
      card(card_header("Food diary"),
        uiOutput("food_totals"),
        DTOutput("food_table"),
        numericInput("delete_food_id", "Entry ID to delete", value = 0, min = 0, step = 1),
        actionButton("delete_food", "Delete selected ID", class = "btn-outline-danger")
      )
    )
  ),
  nav_panel("Exercise", icon = icon("person-running"),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Log an exercise"),
        dateInput("exercise_date", "Date", value = Sys.Date()),
        selectInput("exercise_type", "Activity", choices = names(activity_mets)),
        numericInput("exercise_duration", "Duration (minutes)", 30, min = 1, max = 1000),
        selectInput("exercise_intensity", "Intensity",
                    choices = c("Light", "Moderate", "Vigorous"), selected = "Moderate"),
        actionButton("add_exercise", "Add exercise", class = "btn-primary"),
        p(class = "muted", "Calories burned are estimates based on MET values, body weight and duration.")
      ),
      card(card_header("Exercise history"),
        uiOutput("exercise_totals"),
        DTOutput("exercise_table"),
        numericInput("delete_exercise_id", "Entry ID to delete", value = 0, min = 0, step = 1),
        actionButton("delete_exercise", "Delete selected ID", class = "btn-outline-danger")
      )
    )
  ),
  nav_panel("Workout Planner", icon = icon("calendar-check"),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Build a simple routine"),
        selectInput("fitness_level", "Fitness level",
                    c("Beginner", "Intermediate", "Advanced")),
        numericInput("available_time", "Available time (minutes)", 30, min = 10, max = 180),
        selectInput("preferred_workout", "Preferred activity",
                    c("Walking", "Cycling", "Yoga", "Gym workout", "Dancing", "Swimming")),
        selectInput("planner_goal", "Goal",
                    c("General fitness", "Cardio fitness", "Flexibility", "Strength")),
        actionButton("make_plan", "Create my routine", class = "btn-primary")
      ),
      card(card_header("Suggested routine"),
        uiOutput("workout_plan"),
        p(class = "muted", "Adjust the routine to your ability. Stop if you feel pain, dizziness or unwell.")
      )
    ),
    card(card_header("Weekly activity checklist"),
      tableOutput("weekly_plan")
    )
  ),
  nav_panel("Analytics", icon = icon("chart-line"),
    card(
      fluidRow(
        column(4, dateRangeInput("analytics_dates", "Date range",
                                 start = Sys.Date() - 6, end = Sys.Date())),
        column(4, downloadButton("download_food", "Download food CSV")),
        column(4, downloadButton("download_exercise", "Download exercise CSV"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Calories consumed by day"), plotlyOutput("intake_plot", height = "280px")),
      card(card_header("Calories burned by day"), plotlyOutput("burn_plot", height = "280px")),
      card(card_header("Food macronutrients"), plotlyOutput("macro_plot", height = "280px")),
      card(card_header("Weight history"), plotlyOutput("weight_plot", height = "280px"))
    )
  ),
  nav_panel("Dataset", icon = icon("file-csv"),
    card(card_header("Upload your dataset"),
      p("Upload a CSV file when it is ready. Your original file is not changed. The app previews its columns and checks missing values and duplicate rows."),
      fileInput("dataset_file", "Choose CSV file", accept = c(".csv", "text/csv")),
      checkboxInput("dataset_header", "First row contains column names", TRUE),
      DTOutput("dataset_preview"),
      uiOutput("dataset_summary"),
      DTOutput("missing_table")
    ),
    card(card_header("How the uploaded dataset is handled"),
      p("The uploaded dataset is used for preview and data-quality inspection. Food and exercise diary entries are separately stored in the local FitLife database."),
      p("Column meanings are not guessed or silently converted. Once your actual dataset is available, its structure can be mapped to the relevant tracker.")
    )
  ),
  footer = div(class = "text-center muted p-3",
               "FitLife • Educational wellness tracker • Estimates only")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    food = read_table("food_log"),
    exercise = read_table("exercise_log"),
    weight = read_table("weight_log"),
    profile = read_profile(),
    dataset = NULL,
    plan_created = FALSE
  )

  observe({
    if (!is.null(rv$profile)) {
      p <- rv$profile
      updateTextInput(session, "nickname", value = ifelse(is.null(p$nickname), "", p$nickname))
      updateNumericInput(session, "age", value = p$age)
      updateSelectInput(session, "sex", selected = p$sex)
      updateNumericInput(session, "height", value = p$height)
      updateNumericInput(session, "weight", value = p$weight)
      updateSelectInput(session, "activity", selected = p$activity)
      updateSelectInput(session, "goal", selected = p$goal)
    }
  })

  profile_values <- reactive({
    list(nickname = input$nickname, age = safe_num(input$age, NA_real_),
         sex = input$sex, height = safe_num(input$height, NA_real_),
         weight = safe_num(input$weight, NA_real_),
         activity = input$activity, goal = input$goal)
  })
  bmr_value <- reactive({
    p <- profile_values()
    calc_bmr(p$age, p$sex, p$weight, p$height)
  })
  tdee_value <- reactive({
    calc_tdee(bmr_value(), profile_values()$activity)
  })
  today_food <- reactive({
    rv$food %>% filter(entry_date == as.character(Sys.Date()))
  })
  today_exercise <- reactive({
    rv$exercise %>% filter(entry_date == as.character(Sys.Date()))
  })
  today_intake <- reactive(sum(today_food()$calories, na.rm = TRUE))
  today_burn <- reactive(sum(today_exercise()$calories_burned, na.rm = TRUE))
  today_minutes <- reactive(sum(today_exercise()$duration, na.rm = TRUE))

  observeEvent(input$save_profile, {
    p <- profile_values()
    if (is.na(p$age) || is.na(p$height) || is.na(p$weight) ||
        p$age < 13 || p$height <= 0 || p$weight <= 0) {
      showNotification("Please enter valid age, height and weight.", type = "error")
      return()
    }
    dbExecute(con, "DELETE FROM profile")
    dbExecute(con, paste(
      "INSERT INTO profile (id,nickname,age,sex,height,weight,activity,goal)",
      "VALUES (1,?,?,?,?,?,?,?)"
    ), params = list(p$nickname, p$age, p$sex, p$height, p$weight, p$activity, p$goal))
    dbExecute(con, "INSERT INTO weight_log (entry_date,weight) VALUES (?,?)",
              params = list(as.character(Sys.Date()), p$weight))
    rv$profile <- read_profile()
    rv$weight <- read_table("weight_log")
    showNotification("Profile saved.", type = "message")
  })

  observeEvent(input$add_food, {
    vals <- c(input$food_quantity, input$food_calories,
              input$food_protein, input$food_carbs, input$food_fat)
    if (!nzchar(trimws(input$food_name)) || any(!is.finite(safe_num(vals, NA_real_))) ||
        input$food_quantity <= 0 || any(safe_num(vals[-1]) < 0)) {
      showNotification("Enter a food name and valid non-negative nutrition values.", type = "error")
      return()
    }
    dbExecute(con, paste(
      "INSERT INTO food_log (entry_date,meal,food,quantity,calories,protein,carbs,fat)",
      "VALUES (?,?,?,?,?,?,?,?)"
    ), params = list(as.character(input$food_date), input$meal, input$food_name,
                     input$food_quantity, input$food_calories, input$food_protein,
                     input$food_carbs, input$food_fat))
    rv$food <- read_table("food_log")
    showNotification("Food entry added.", type = "message")
  })

  observeEvent(input$delete_food, {
    if (input$delete_food_id > 0) {
      dbExecute(con, "DELETE FROM food_log WHERE id = ?", params = list(as.integer(input$delete_food_id)))
      rv$food <- read_table("food_log")
      showNotification("Food entry deleted if the ID existed.", type = "message")
    }
  })

  observeEvent(input$add_exercise, {
    if (!is.finite(input$exercise_duration) || input$exercise_duration <= 0) {
      showNotification("Enter a valid exercise duration.", type = "error")
      return()
    }
    p <- profile_values()
    if (!is.finite(p$weight) || p$weight <= 0) {
      showNotification("Save a valid weight in My Profile before logging exercise.", type = "error")
      return()
    }
    met <- unname(activity_mets[input$exercise_type])
    intensity_factor <- switch(input$exercise_intensity, Light = 0.8, Moderate = 1, Vigorous = 1.2)
    met <- met * intensity_factor
    burned <- met * 3.5 * p$weight / 200 * input$exercise_duration
    dbExecute(con, paste(
      "INSERT INTO exercise_log (entry_date,activity,duration,intensity,met,calories_burned)",
      "VALUES (?,?,?,?,?,?)"
    ), params = list(as.character(input$exercise_date), input$exercise_type,
                     input$exercise_duration, input$exercise_intensity, met, round(burned)))
    rv$exercise <- read_table("exercise_log")
    showNotification(paste("Exercise saved. Estimated burn:", round(burned), "kcal."), type = "message")
  })

  observeEvent(input$delete_exercise, {
    if (input$delete_exercise_id > 0) {
      dbExecute(con, "DELETE FROM exercise_log WHERE id = ?",
                params = list(as.integer(input$delete_exercise_id)))
      rv$exercise <- read_table("exercise_log")
      showNotification("Exercise entry deleted if the ID existed.", type = "message")
    }
  })

  output$dash_intake <- renderText(format(round(today_intake()), big.mark = ","))
  output$dash_burned <- renderText(format(round(today_burn()), big.mark = ","))
  output$dash_balance <- renderText({
    if (!is.finite(tdee_value())) "Set profile"
    else format(round(tdee_value() - today_intake()), big.mark = ",")
  })
  output$dash_minutes <- renderText(paste(round(today_minutes()), "min"))

  output$today_chart <- renderPlotly({
    d <- data.frame(Category = c("Consumed", "Burned"),
                    Calories = c(today_intake(), today_burn()))
    plot_ly(d, x = ~Category, y = ~Calories, type = "bar",
            marker = list(color = c("#237A57", "#E5A33B"))) %>%
      layout(yaxis = list(title = "kcal"), xaxis = list(title = ""))
  })
  output$daily_guidance <- renderUI({
    if (!is.finite(tdee_value())) {
      return(tags$p("Complete and save your profile to see your estimated daily energy needs."))
    }
    diff <- today_intake() - tdee_value()
    msg <- if (today_intake() == 0) {
      "Start by recording your meals so your daily summary can become more useful."
    } else if (abs(diff) < 150) {
      "Your recorded intake is currently close to your estimated maintenance energy level. Daily needs naturally vary."
    } else if (diff > 0) {
      "Your recorded intake is above your estimated maintenance level today. One day does not define your progress; look at longer-term patterns."
    } else {
      "Your recorded intake is below your estimated maintenance level today. Make sure you are eating enough and prioritizing balanced meals."
    }
    tags$p(msg)
  })
  output$profile_snapshot <- renderUI({
    p <- profile_values()
    if (!is.finite(bmr_value())) return(tags$p("No saved profile metrics yet."))
    tags$div(
      tags$p(strong("Estimated BMR: "), round(bmr_value()), " kcal/day"),
      tags$p(strong("Estimated TDEE: "), round(tdee_value()), " kcal/day"),
      tags$p(strong("Goal: "), p$goal)
    )
  })
  output$recent_food <- renderDT({
    x <- rv$food %>% arrange(desc(entry_date), desc(id)) %>% head(5)
    datatable(x, rownames = FALSE, options = list(dom = "tip", pageLength = 5))
  })

  output$health_metrics <- renderUI({
    p <- profile_values()
    bmi <- calc_bmi(p$weight, p$height)
    if (!is.finite(bmi) || !is.finite(bmr_value())) {
      return(tags$p("Enter valid profile information to calculate your estimates."))
    }
    tagList(
      fluidRow(
        column(4, div(class = "metric-card p-3",
                      div(class = "muted", "BMI"), div(class = "metric-value", round(bmi, 1)),
                      tags$small(bmi_category(bmi)))),
        column(4, div(class = "metric-card p-3",
                      div(class = "muted", "BMR"), div(class = "metric-value", round(bmr_value())),
                      tags$small("kcal/day"))),
        column(4, div(class = "metric-card p-3",
                      div(class = "muted", "TDEE"), div(class = "metric-value", round(tdee_value())),
                      tags$small("kcal/day")))
      )
    )
  })

  output$food_totals <- renderUI({
    x <- rv$food
    if (nrow(x)) x <- x %>% filter(entry_date == as.character(input$food_date))
    tagList(
      p(strong("Calories on selected date: "), round(sum(x$calories, na.rm = TRUE)), " kcal"),
      p(strong("Protein: "), round(sum(x$protein, na.rm = TRUE), 1), " g  |  ",
        strong("Carbs: "), round(sum(x$carbs, na.rm = TRUE), 1), " g  |  ",
        strong("Fat: "), round(sum(x$fat, na.rm = TRUE), 1), " g")
    )
  })
  output$food_table <- renderDT({
    x <- rv$food
    if (nrow(x)) x <- x %>% filter(entry_date == as.character(input$food_date))
    datatable(x, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$exercise_totals <- renderUI({
    x <- rv$exercise
    if (nrow(x)) x <- x %>% filter(entry_date == as.character(input$exercise_date))
    tagList(
      p(strong("Exercise time: "), sum(x$duration, na.rm = TRUE), " minutes"),
      p(strong("Estimated calories burned: "), round(sum(x$calories_burned, na.rm = TRUE)), " kcal")
    )
  })
  output$exercise_table <- renderDT({
    x <- rv$exercise
    if (nrow(x)) x <- x %>% filter(entry_date == as.character(input$exercise_date))
    datatable(x, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })

  observeEvent(input$make_plan, { rv$plan_created <- TRUE })
  output$workout_plan <- renderUI({
    req(rv$plan_created)
    mins <- input$available_time
    level <- input$fitness_level
    activity <- input$preferred_workout
    warm <- max(3, round(mins * 0.15))
    main <- max(5, round(mins * 0.7))
    cool <- max(2, mins - warm - main)
    tags$div(
      h4(paste(level, input$planner_goal, "routine")),
      tags$ul(
        tags$li(paste("Warm-up:", warm, "minutes of gentle movement")),
        tags$li(paste(activity, ":", main, "minutes at a comfortable", tolower(level), "pace")),
        tags$li(paste("Cool-down and stretching:", cool, "minutes"))
      ),
      p("Take breaks as needed. Increase duration gradually rather than pushing through discomfort.")
    )
  })
  output$weekly_plan <- renderTable({
    data.frame(
      Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
      Suggested_activity = c("Walk + mobility", "Strength basics", "Gentle cardio",
                             "Yoga / flexibility", "Cardio", "Enjoyable activity", "Rest / gentle walk")
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  selected_dates <- reactive({
    req(input$analytics_dates)
    as.Date(input$analytics_dates)
  })
  filtered_food <- reactive({
    x <- rv$food
    if (!nrow(x)) return(x)
    date_filter(x, from = selected_dates()[1], to = selected_dates()[2])
  })
  filtered_exercise <- reactive({
    x <- rv$exercise
    if (!nrow(x)) return(x)
    date_filter(x, from = selected_dates()[1], to = selected_dates()[2])
  })
  output$intake_plot <- renderPlotly({
    x <- filtered_food()
    if (!nrow(x)) return(plotly_empty() %>% layout(title = "No food records in this date range"))
    d <- x %>% group_by(entry_date) %>% summarise(Calories = sum(calories), .groups = "drop")
    ggplotly(ggplot(d, aes(entry_date, Calories)) + geom_col(fill = "#237A57") +
               labs(x = "Date", y = "Calories (kcal)") + theme_minimal())
  })
  output$burn_plot <- renderPlotly({
    x <- filtered_exercise()
    if (!nrow(x)) return(plotly_empty() %>% layout(title = "No exercise records in this date range"))
    d <- x %>% group_by(entry_date) %>% summarise(Calories = sum(calories_burned), .groups = "drop")
    ggplotly(ggplot(d, aes(entry_date, Calories)) + geom_col(fill = "#E5A33B") +
               labs(x = "Date", y = "Estimated kcal burned") + theme_minimal())
  })
  output$macro_plot <- renderPlotly({
    x <- filtered_food()
    if (!nrow(x) || sum(x$protein, x$carbs, x$fat, na.rm = TRUE) == 0) {
      return(plotly_empty() %>% layout(title = "Add macronutrient values to see this chart"))
    }
    d <- data.frame(Nutrient = c("Protein", "Carbohydrates", "Fat"),
                    Grams = c(sum(x$protein), sum(x$carbs), sum(x$fat)))
    plot_ly(d, labels = ~Nutrient, values = ~Grams, type = "pie",
            textinfo = "label+percent")
  })
  output$weight_plot <- renderPlotly({
    x <- rv$weight
    if (nrow(x)) x <- x %>% filter(entry_date >= as.character(selected_dates()[1]),
                                    entry_date <= as.character(selected_dates()[2]))
    if (!nrow(x)) return(plotly_empty() %>% layout(title = "Save your profile to begin weight history"))
    ggplotly(ggplot(x, aes(as.Date(entry_date), weight)) +
               geom_line(color = "#237A57") + geom_point(color = "#237A57") +
               labs(x = "Date", y = "Weight (kg)") + theme_minimal())
  })

  output$download_food <- downloadHandler(
    filename = function() paste0("fitlife-food-", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$food, file, row.names = FALSE)
  )
  output$download_exercise <- downloadHandler(
    filename = function() paste0("fitlife-exercise-", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$exercise, file, row.names = FALSE)
  )

  observeEvent(input$dataset_file, {
    req(input$dataset_file)
    tryCatch({
      rv$dataset <- readr::read_csv(input$dataset_file$datapath,
                                    col_names = isTRUE(input$dataset_header),
                                    show_col_types = FALSE,
                                    name_repair = "unique")
      showNotification("CSV uploaded and read successfully.", type = "message")
    }, error = function(e) {
      rv$dataset <- NULL
      showNotification(paste("Could not read CSV:", conditionMessage(e)), type = "error",
                       duration = NULL)
    })
  })
  output$dataset_preview <- renderDT({
    req(rv$dataset)
    datatable(head(rv$dataset, 100), options = list(scrollX = TRUE, pageLength = 10))
  })
  output$dataset_summary <- renderUI({
    req(rv$dataset)
    x <- rv$dataset
    tags$div(
      p(strong("Rows: "), nrow(x), " | ", strong("Columns: "), ncol(x)),
      p(strong("Duplicate rows: "), sum(duplicated(as.data.frame(x)))),
      p(strong("File columns: "), paste(names(x), collapse = ", "))
    )
  })
  output$missing_table <- renderDT({
    req(rv$dataset)
    x <- rv$dataset
    miss <- data.frame(
      Column = names(x),
      Type = vapply(x, function(z) paste(class(z), collapse = "/"), character(1)),
      Missing_values = vapply(x, function(z) sum(is.na(z)), integer(1))
    )
    datatable(miss, rownames = FALSE, options = list(dom = "t"))
  })
}

shinyApp(ui = ui, server = server)
