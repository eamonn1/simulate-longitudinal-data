#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(tidyverse)
require(ggplot2)
library(shiny) 
library(nlme)
library(MASS)
options(max.print=1000000)
fig.width <- 1200
fig.height <- 450
library(shinythemes)        # more funky looking apps
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=100)
set.seed(123)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"),
                
                shinyUI(pageWithSidebar(
                    
                    
                    headerPanel("
                                Simulating, plotting and analysing a cohort followed up over time "),
                    
                    sidebarPanel( 
                        
                        div(p("We simulate, plot and analyse longitudinal data. Two options for the data generation and analysis are provided. 
                              'Log transforming the predictor (time) variable' and 'No transformation to the predictor (time) variable'. For the former log of time is included in
                              the data generation and accounted for in the model. In the latter, no log transformation is applied nor included in the model. 
                              The sliders can be used to select the true population parameters. Two plot options are provided, 'All profiles together' and 'Individual profile plots'.")),
                        
                        div(
                            
                            selectInput("Plot",
                                        strong("Select plot preference "),
                                        choices=c("All profiles together", "Individual profile plots" )),
                            
                            selectInput("Model",
                                        strong("Select modelling preference "),
                                        choices=c( "Log transforming the predictor (time) variable" , "No transformation to the predictor (time) variable" )),
                            
                           
                            actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/simulate-longitudinal-data/master/app/app.R', '_blank')"),   
                            actionButton("resample", "Simulate a new sample"),
                            br(), br(),
                          
                            div(strong("Select true population parameters"),p(" ")),
                            
                            
                            div(("Select the number of subjects, average intercept, average slope, the auto correlation, error SD, intercept SD, slope SD and the slope intercept correlation as well as the maximum number of visits.
                                 
                                 
                                 Another sample can be taken from the same population/data generating mechanisim by clicking 'Simulate a new sample'.")),
                            br(),
                            
                            sliderInput("n",
                                        "No of subjects",
                                        min=2, max=500, step=1, value=200, ticks=FALSE),
                            
                            sliderInput("beta0", "Average intercept", 
                                        min = 50, max = 2000, step=.5, value = c(400), ticks=FALSE) ,
                            
                            sliderInput("beta1", "Average slope",
                                        min = -100, max = 100, step=.5, value = c(-60),ticks=FALSE),
                            
                            sliderInput("ar.val", "True autocorrelation",  
                                        min = -1, max = 1, value = c(.4), step=0.05, ticks=FALSE),
                       
                            sliderInput("sigma", "True error SD", #    
                                        min = 2, max = 200, value = c(100), step=.5, ticks=FALSE),
                            
                            sliderInput("tau0", "True intercept SD", #   
                                        min = 1, max = 100, value = c(25.5), step=.5, ticks=FALSE),
                            
                            sliderInput("tau1", "True slope SD", #    
                                        min = 1, max = 100, value = c(10),step=.5,  ticks=FALSE),
                            
                            sliderInput("tau01", "True intercept slope correlation", #   
                                        min = -1, max = 1, value = c(0), step=0.05, ticks=FALSE),
                            
                            sliderInput("m", "maximum number of visits", #   
                                        min = 2, max = 100, value = c(10), ticks=FALSE, step=1)
                        
                            
                               
                        )
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        #    tabsetPanel(type = "tabs", 
                        navbarPage(       
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                            tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: lightgrey;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")), 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                            tabPanel("Plot and analysis", 
                                     
                                     div(plotOutput("reg.plot", width=fig.width, height=fig.height)),  
                                     
                                     p(strong("Above a plot of the data and below the output from the statistical analysis.")) ,
                                     
                                     div( verbatimTextOutput("reg.summary"))
                                     
                            ) 
                            
                            #,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          #  tabPanel("2 tab", value=3,) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                           # tabPanel("3 tab", value=3, ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                           # tabPanel("4 tab", ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                          #  tabPanel("5 tab",  )
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        )
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels
                    
                    #  ) #new
                )
                )
)

server <- shinyServer(function(input, output) {
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated only random noise is required to be generated
    random.sample <- reactive({
        
        # Dummy line to trigger off button-press
        foo <- input$resample
        
        
         n <- input$n 
         beta0<- input$beta0
         beta1<- input$beta1
         ar.val <- input$ ar.val 
         sigma <- input$sigma
         tau0 <- input$tau0
         tau1<- input$tau1
         tau01<- input$tau01
         m <- input$m
        
        
        
        
        return(list( 
            n =n ,
            beta0=beta0,
            beta1=beta1,
            ar.val= ar.val ,
            sigma =sigma,
            tau0 =tau0,
            tau1=tau1,
            tau01=tau01,
            m =m
        ))
        
    }) 
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    make.regression <- reactive({
        
        #   https://stats.stackexchange.com/questions/28876/difference-between-anova-power-simulation-and-power-calculation
        
        sample <- random.sample()
        n<- sample$n
        beta0<- sample$beta0
        beta1<- sample$beta1
        ar.val <- sample$ar.val 
        sigma <- sample$sigma
        tau0 <- sample$tau0
        tau1<- sample$tau1
        tau01<- sample$tau01
        m <- sample$m
        
        p <- round(runif(n,4,m))
        
        ### simulate observation moments (assume everybody has 1st obs)
        obs <- unlist(sapply(p, function(x) c(1, sort(sample(2:m, x-1, replace=FALSE)))))
        
        ### set up data frame
        dat <- data.frame(id=rep(1:n, times=p), obs=obs)
        
        ### simulate (correlated) random effects for intercepts and slopes
        mu  <- c(0,0)
        S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
        tau <- c(tau0, tau1)
        S   <- diag(tau) %*% S %*% diag(tau)
        U   <- mvrnorm(n, mu=mu, Sigma=S)
        
        ### simulate AR(1) errors and then the actual outcomes

        dat$eij <- unlist(sapply(p, function(x) arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma))
        

        if (input$Model == "Log transforming the predictor (time) variable") {
           dat$yij <- (beta0 + rep(U[,1], times=p)) + (beta1 + rep(U[,2], times=p)) *  log(dat$obs) + dat$eij  
        }   else if (input$Model == "No transformation to the predictor (time) variable") {    
           dat$yij <- (beta0 + rep(U[,1], times=p)) + (beta1 + rep(U[,2], times=p)) *  (dat$obs) + dat$eij  
        }
        
        ### note: use arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma
        ### construction, so that the true error SD is equal to sigma
        ### create grouped data object
         dat <- groupedData(yij ~ obs | id, data=dat)
        
         return(list(dat=dat )) 
        
    })  
    
    # --------------------------------------------------------------------------
    # Fit the specified regression model
    fit.regression <- reactive({
        
        data <- make.regression()
        
        df <- data$dat
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Conditionally fit the model
        
        if (input$Model == "Log transforming the predictor (time) variable") {
            
            fit.res <-  
                tryCatch( 
                    lme(yij ~ log(obs), random = ~ log(obs) | id, correlation = corAR1(form = ~ 1 | id), data=df)
                    ,  error=function(e) e)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ###http://stackoverflow.com/questions/8093914
            ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
            
            if (!inherits(fit.res, "error")) {
                
                fit.res <-  summary(fit.res) # for the residuals

            } else  {
                
                fit.res <- NULL

            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        } else if (input$Model == "No transformation to the predictor (time) variable") {          
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            fit.res <-  
                tryCatch( 
                    lme(yij ~  (obs), random = ~  (obs) | id, correlation = corAR1(form = ~ 1 | id), data=df)
                    ,  error=function(e) e)
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (!inherits(fit.res, "error")) {
           
                fit.res <-  summary(fit.res) # for the residuals
                
            } else  {
                
                fit.res <- NULL
                
            }
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # Get the model summary
        if (is.null(fit.res)) {
            
            fit.summary <- NULL
            
        } else {
            
            fit.summary <-  (fit.res)
        }
        
        return(list(  
          
            fit.summary=fit.summary 
                    
        ))
        
    })     
    
    # --------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data1 <- make.regression()
        
        df <- data1$dat
        
        # Conditionally plot
        if (input$Plot == "All profiles together") {
            
            #base plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #https://rstudio-pubs-static.s3.amazonaws.com/308410_2ece93ee71a847af9cd12fa750ed8e51.html
            names(df) <- c("ID","VISIT","eij","value")
        
            df_summary <- df %>% # the names of the new data frame and the data frame to be summarised
                group_by(VISIT) %>%                # the grouping variable
                summarise(mean_PL = mean(value, na.rm=TRUE),  # calculates the mean of each group
                          sd_PL = sd(value, na.rm=TRUE),      # calculates the sd of each group
                          n_PL = length(na.omit(value)),      # calculates the sample size per group
                          SE_PL = sd(value, na.rm=TRUE)/sqrt(length(na.omit(value)))) # SE of each group
            
            df_summary1 <- merge(df, df_summary)  # merge stats to dataset
            
            df_summary1$L2SE <- df_summary1$mean_PL - 2*df_summary1$SE_PL
            df_summary1$H2SE <- df_summary1$mean_PL + 2*df_summary1$SE_PL
            
            
            pr1 <- ggplot((df_summary1), aes(x = VISIT, y =value, color = ID)) +
                geom_line( size=.5, alpha=0.2) +
                stat_summary(geom="line",  fun.y=mean, colour="black", lwd=0.5) +  # , linetype="dashed"
                stat_summary(geom="point", fun.y=mean, colour="black") +
                geom_errorbar(data=(df_summary1), 
                              aes( ymin=L2SE, ymax=H2SE ), color = "black",
                              width=0.05, lwd = 0.05) +
                scale_y_continuous(expand = c(.1,0) ) +
                
                
                
                scale_x_continuous(breaks = c(unique(df$VISIT)),
                                   labels = 
                                       c(unique(df$VISIT))
                ) +
            
            EnvStats::stat_n_text(size = 4, y.pos = max(df_summary1$value, na.rm=T)*1.1 , y.expand.factor=0, 
                                  angle = 0, hjust = .5, family = "mono", fontface = "plain") + #295 bold
                
                theme(panel.background=element_blank(),
                      # axis.text.y=element_blank(),
                      # axis.ticks.y=element_blank(),
                      # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                      # stop axis being clipped
                      plot.title=element_text(), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                      legend.text=element_text(size=12),
                      legend.title=element_text(size=14),
                      legend.position="none",
                      axis.text.x  = element_text(size=10),
                      axis.text.y  = element_text(size=10),
                      axis.line.x = element_line(color="black"),
                      axis.line.y = element_line(color="black"),
                      plot.caption=element_text(hjust = 0, size = 7))
            
            
            print(pr1 + labs(y="Response", x = "Visit") + 
                      ggtitle(paste0("Individual responses ",
                                     length(unique(df$ID))," patients & arithmetic mean with 95% CI shown in black\nNumber of patient values at each time point") )
            
                  )
       
        } else {
            
            plot(df, pch=19, cex=.5)
             
        }
        
    })
    #---------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot residuals 
    
  #  output$residual <- renderPlot({         
        
        # Get the current regression model
        # d  <- fit.regression()
        # 
        # f<- d$ff
        # 
        # par(mfrow=c(3,2))
        # plot(f)
        # 
        # #dd <- d$fit.res
        # anova.residuals <- residuals( object =  f) # extract the residuals
        # # A simple histogram
        # hist( x = anova.residuals , breaks=50, main=paste("Histogram of ANOVA residuals, SD=",p2(sd(anova.residuals)),"")) # another way of seeing residuals
        # par(mfrow=c(1,1)) 
        
 #   })
    
    #---------------------------------------------------------------------------
    # Show the summary for the 
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression()$fit.summary
        
        if (!is.null(summary)) {
            
            return(fit.regression()$fit.summary)
            
        } else if (is.null(summary)){
            
            return("error")
        }
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # the data to print
    output$summary2 <- renderPrint({ 
        
        return(make.regression()$dat)
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # the data to print, I wooulf like to reuse this but dont think it is possible? So I add another function to collect the same information below
    # output$byhand <- renderPrint({
    #     
    #     return(explain()$ANOVA)
    #     
    # })
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # output$byhand2 <- renderPrint({
    #     
    #     return(explain()$ANOVA2)
    #     
    # })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)