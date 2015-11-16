library(shiny)

paramNames <- c("start_capital", "annual_mean_return", "annual_ret_std_dev",
	"annual_inflation", "annual_inf_std_dev", "monthly_withdrawals", "n_obs",
	"n_sim", "monthly_additions_br", "annual_mean_return_br", "n_obs_br")

# simulate_nav <- function(start_capital = 2000000, annual_mean_return = 5.0, 
#                          annual_ret_std_dev = 7.0, annual_inflation = 2.5,
#                          annual_inf_std_dev = 1.5, monthly_withdrawals = 1000,
#                          n_obs = 20, n_sim = 200, monthly_additions_br = 1000,
#                          annual_mean_return_br = 5.0, n_obs_br = 20 
# )

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#
shinyServer(function(input, output, session) {

	getParams <- function(prefix) {
		input[[paste0(prefix, "_recalc")]]

		params <- lapply(paramNames, function(p) {
			input[[paste0(prefix, "_", p)]]
		})
		names(params) <- paramNames
		params
	}

  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
   navA <- reactive(do.call(simulate_nav, getParams("a")))
# navA <- simulate_nav()
   navB <- reactive(do.call(simulate_nav, getParams("b")))

  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot

  output$a_distPlot <- renderPlot({
  	plot_nav(navA()[[1]])
  })
  output$b_distPlot <- renderPlot({
  	plot_nav(navB()[[1]])
  })
  output$a_retirement_table_br <-  DT::renderDataTable({ navA()[[2]] }) 
  output$a_retirement_table_ar <-  DT::renderDataTable({ navA()[[3]] }) 
  output$b_retirement_table_br <-  DT::renderDataTable({ navB()[[2]] }) 
  output$b_retirement_table_ar <-  DT::renderDataTable({ navB()[[3]] }) 
})


  
simulate_nav <- function(start_capital = 2000000, annual_mean_return = 5.0, 
                         annual_ret_std_dev = 7.0, annual_inflation = 2.5,
                         annual_inf_std_dev = 1.5, monthly_withdrawals = 1000,
                         n_obs = 10, n_sim = 10, monthly_additions_br = 1000,
                         annual_mean_return_br = 5.0, n_obs_br = 5 
                         ) {
  #-------------------------------------
  # Inputs
  #-------------------------------------

  # Initial capital
  start.capital = start_capital

  # Investment
  annual.mean.return = annual_mean_return / 100
  annual.mean.return.br = annual_mean_return_br / 100  
  annual.ret.std.dev = annual_ret_std_dev / 100

  # Inflation
  annual.inflation = annual_inflation / 100
  annual.inf.std.dev = annual_inf_std_dev / 100

  # Withdrawals
  monthly.withdrawals = monthly_withdrawals
  monthly.additions.br = monthly_additions_br
  
  # Number of observations (in Years)
  n.obs = n_obs
  n.obs.br = n_obs_br
  

  # Number of simulations
  n.sim = n_sim


  #-------------------------------------
  # Simulation
  #-------------------------------------

  # number of months to simulate
  n.obs = 12 * n.obs
  n.obs.br = 12 * n.obs.br

  # monthly Investment and Inflation assumptions
  monthly.mean.return = annual.mean.return / 12
  monthly.mean.return.br = annual.mean.return.br / 12
  monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)

  monthly.inflation = annual.inflation / 12
  monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)

  # simulate Returns # n.obs.br <- 20; n.obs <- 20; n.sim <- 20; monthly.mean.return.br <- .004
  monthly.invest.returns.br = matrix(0, n.obs.br, n.sim)
  monthly.invest.returns = matrix(0, n.obs, n.sim)
  monthly.inflation.returns = matrix(0, n.obs, n.sim)
  monthly.inflation.returns.br = matrix(0, n.obs.br, n.sim)
  
  monthly.invest.returns.br12 = matrix(0, n.obs.br/ 12 , n.sim)
  
  #  monthly.mean.return.br <- .004; monthly.ret.std.dev <- (2/100)/ sqrt(12)
  # monthly.inflation <- .03/12; monthly.inf.std.dev <-  (1/100)/ sqrt(12)

  monthly.invest.returns.br[] = rnorm(n.obs.br * n.sim, mean = monthly.mean.return.br, sd = monthly.ret.std.dev)
  monthly.invest.returns[] = rnorm(n.obs * n.sim, mean = monthly.mean.return, sd = monthly.ret.std.dev)
  monthly.inflation.returns[] = rnorm(n.obs * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)
  monthly.inflation.returns.br[] = rnorm(n.obs.br * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)
  
  annualize_returns <- function(M){
    rowsM <- nrow(M)
    M12 = matrix(0, rowsM/ 12 , n.sim)
    for(i in 1: n.sim){
      for(k in 1: rowsM/ 12 ){
        value <- 1.0
        for(j in 1:12){
          value <- value*(M[(k-1)*12 + j , i] + 1 )
        }
        M12[k,i] =  round((value - 1)* 100, digits = 1)
      }
    }
    return(M12)
  }
  
  monthly.invest.returns.br12 <- annualize_returns(monthly.invest.returns.br - monthly.inflation.returns.br )
  monthly.invest.returns.12 <- annualize_returns(monthly.invest.returns - monthly.inflation.returns )
#   monthly.invest.returns.br12 = matrix(0, n.obs.br/ 12 , n.sim)
#   for(i in 1: n.sim){
#     for(k in 1: nrow(monthly.invest.returns.br12) ){
#       value <- 1.0
#       for(j in 1:12){
#        value <- value*(monthly.invest.returns.br[(k-1)*12 + j , i] + 1 )
#       }
#       monthly.invest.returns.br12[k,i] =  round((value - 1)* 100, digits = 1)
#     }
#   }


  
  
  # simulate Withdrawals
  # start.capital = 2000000; 
  nav = matrix(start.capital, n.obs.br + n.obs + 1, n.sim)
  
  # take this loop below and create another one that starts at n.obs and fills in for retirement.
  # the beauty is that the 
  if(n.obs.br > 0){
    for (j in 1:n.obs.br) {
      nav[j + 1, ] = nav[j, ] * (1 + monthly.invest.returns.br[j, ] - monthly.inflation.returns.br[j, ]) + monthly.additions.br
    }
  }
  for (j in 1  :n.obs ) {
    nav[j + 1 + n.obs.br , ] = nav[j + n.obs.br , ] * (1 + monthly.invest.returns[j, ] - monthly.inflation.returns[j, ]) - monthly.withdrawals
  }


  # once nav is below 0 => run out of money
  nav[ nav < 0 ] = NA

  # convert to millions
  nav = nav / 1000000
  make_df <- function(M){
    DF_i_r <- data.frame(M)
    colnums <- seq(1, ncol(DF_i_r), 1)
    colnames(DF_i_r) <- as.character(paste0('sim ' , colnums))
    return(DF_i_r)
  }
 DF_invest_returns <-  make_df(monthly.invest.returns.br12)
 DF_invest_returns_ar <-  make_df(monthly.invest.returns.12)
#   DF_invest_returns <- data.frame(monthly.invest.returns.br12)
#   colnums <- seq(1, ncol(DF_invest_returns), 1)
#   colnames( DF_invest_returns) <- as.character(paste0('sim ' , colnums))
 nav <- list(nav, DF_invest_returns, DF_invest_returns_ar )
  return(nav)
}

plot_nav <- function(nav) {

  layout(matrix(c(1,2,1,3),2,2))

  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))

  # create matrix at annual slices only
  rows <- seq(from = 1, to = (nrow(nav))-1 , by = 12)
  nav <- nav[rows ,]
  # plot all scenarios
  matplot(nav,
    type = 'l', lwd = 0.5, lty = 1, col = 1:5,
    xlab = 'Years', ylab = 'Millions',
    main = 'Projected Value of Initial Capital')

  # plot % of scenarios that are still paying
  p.alive = 1 - rowSums(is.na(nav)) / ncol(nav)

  plot(100 * p.alive, las = 1, xlab = 'Years', ylab = 'Percentage Paying',
    main = 'Percentage of Paying Scenarios', ylim=c(0,100))
  grid()


  last.period = nrow(nav)

  # plot distribution of final wealth
  final.nav = nav[last.period, ]
  final.nav = final.nav[!is.na(final.nav)]

  if(length(final.nav) ==  0) return()

  plot(density(final.nav, from=0, to=max(final.nav)), las = 1, xlab = 'Final Capital',
    main = paste0('Distribution of Final Capital\n', 100 * p.alive[last.period], '% are still paying'))
  grid()
}
