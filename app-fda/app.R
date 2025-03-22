
source("global.R")

box::use(
  bslib[page_navbar, nav_panel, nav_spacer, nav_item],
  bsicons[bs_icon]
)

box::use(
  ./modules/home
)

ui <- page_navbar(
  title = tags$span("FDA", style = "color: #428bca; font-weight: bold; font-size: 1.5rem;"),
  window_title = "FDA",
  nav_panel(
    title = "Home",
    icon = bs_icon("bar-chart-fill"),
    home$ui("home")
    ),
  nav_spacer(),
  nav_item("FDA Adverse Events Reporting System (FAERS) Public Dashboard"),
  footer = tags$div(style = "padding: 4px;","Data as of December 31, 2024")
  
  
)


server <- function(input, output, session) {

  home$server("home",
              db_report_type = db_report_type,
              db_reporter = db_reporter,
              db_reporter_region = db_reporter_region,
              db_report_seriousness = db_report_seriousness,
              db_report_age_group = db_report_age_group,
              db_sex = db_sex)
}


shinyApp(ui = ui, server = server)

