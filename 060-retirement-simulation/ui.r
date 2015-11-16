library(shiny)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
        sliderInput(paste0(prefix, "_", "n_obs_br"), "Number of years before retirement:", min = 0, max = 40, value = 20),
        sliderInput(paste0(prefix, "_", "n_obs"), "Number of years after retirement:", min = 0, max = 50, value = 30),       
        sliderInput(paste0(prefix, "_", "start_capital"), "Initial capital invested :", min = 0, max = 10000000, value = 500000, step = 10000, pre = "$", sep = ","),
        sliderInput(paste0(prefix, "_", "monthly_additions_br"), "Monthly capital invested :", min = 00, max = 5000, value = 400, step = 100, pre = "$", sep = ","),
        sliderInput(paste0(prefix, "_", "monthly_withdrawals"), "Monthly capital withdrawals in Retirement:", min = 1000, max = 20000, value = 7000, step = 500, pre = "$", sep = ",")
      ),
      column(6,
             sliderInput(paste0(prefix, "_", "annual_mean_return_br"), "Annual investment return before retirement (in %):", min = 0.0, max = 20.0, value = 7.5, step = 0.5),
             sliderInput(paste0(prefix, "_", "annual_mean_return"), "Annual investment return after retirement(in %):", min = 0.0, max = 15.0, value = 6.5, step = 0.5),
             sliderInput(paste0(prefix, "_", "annual_ret_std_dev"), "Annual investment volatility (in %):", min = 0.0, max = 15.0, value = 2.0, step = 0.1),
        sliderInput(paste0(prefix, "_", "annual_inflation"), "Annual inflation (in %):", min = 0, max = 10, value = 2.5, step = 0.1),
        sliderInput(paste0(prefix, "_", "annual_inf_std_dev"), "Annual inflation volatility. (in %):", min = 0.0, max = 5.0, value = 0.0, step = 0.05),

        sliderInput(paste0(prefix, "_", "n_sim"), "Number of simulations:", min = 0, max = 200, value = 10)
      )
    ),
    p(actionButton(paste0(prefix, "_", "recalc"),
      "Re-run simulation", icon("random")
    ))
  )
}

# Define UI for application that plots random distributions
shinyUI(fluidPage(theme="simplex.min.css",
  tags$style(type="text/css",
    "label {font-size: 12px;}",
    ".recalculating {opacity: 1.0;}"
  ),

  # Application title
  tags$h4("Retirement: simulating wealth with random returns, inflation and withdrawals"),
#   p("An adaptation of the",
#     tags$a(href="http://glimmer.rstudio.com/systematicin/retirement.withdrawal/", "retirement app"),
#     "from",
#     tags$a(href="http://systematicinvestor.wordpress.com/", "Systematic Investor"),
#     "to demonstrate the use of Shiny's new grid options."),
  hr(),

  fluidRow(
    column(6, tags$h3("Scenario A")),
    column(6, tags$h3("Scenario B"))
  ),
  fluidRow(
    column(6, renderInputs("a")),
    column(6, renderInputs("b"))
  ),
  fluidRow(
    column(6,
           tabsetPanel(
               tabPanel("Plots", plotOutput("a_distPlot", height = "500px")),
               tabPanel("Returns_before_retirement",  DT::dataTableOutput("a_retirement_table_br") ),
               tabPanel("Returns_after_retirement",  DT::dataTableOutput("a_retirement_table_ar") ) )
    ),
    column(6,
#      plotOutput("b_distPlot", height = "500px")
          tabsetPanel(
                tabPanel("Plots", plotOutput("b_distPlot", height = "500px")),
                tabPanel("Returns_before_retirement",  DT::dataTableOutput("b_retirement_table_br") ),
                tabPanel("Returns_after_retirement",  DT::dataTableOutput("b_retirement_table_ar") ) )

    )
  )
))







