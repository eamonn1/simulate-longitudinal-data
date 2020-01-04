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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"),
                
                shinyUI(pageWithSidebar(
                    
                    
                    headerPanel("
                                title "),
                    
                    sidebarPanel( 
                        
                        div(p("intro here")),
                        
                        div(
                            
                            selectInput("Plot",
                                        strong("Select plot preference "),
                                        choices=c("All profiles together", "Individual profile plots" )),
                            
                            selectInput("Model",
                                        strong("Select modelling preference "),
                                        choices=c( "base R" , "VCA package" )),
                            
                            
                            actionButton("resample", "Simulate a new sample"),
                            br(),br(),
                            
                            actionButton(inputId='ab1', label="R code here", 
                                         icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/One-way-ANOVA/master/app.R', '_blank')"),
                            
                            div(strong("Select true population parameters"),p(" ")),
                            
                            
                            div(("describe input options...another sample can be taken from the same population/data generating mechanisim by clicking 'Simulate a new sample'.")),
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
                            
                            sliderInput("m", "maximum number of possible observations", #   
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
                            tabPanel("1 tab", 
                                     
                                     div(plotOutput("reg.plot", width=fig.width, height=fig.height)),  
                                     
                                     p(strong("text under plot here")) ,
                                     
                                     div( verbatimTextOutput("reg.summary"))
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("2 tab", value=3, 
                                     
                                     p(strong("When the sample sizes are equal we can explain ANOVA very simply. Let us estimate:")) ,
                                     
                                     p("$$\\begin{align}
                      \\sigma^2 \\\\
                      \\end{align}$$"),
                                     p(""), 
                                     p(strong("using a pooled estimate of the variance in each group ",HTML(" <em>i</em>")," this is just the mean variance")),
                                     
                                     p("$$\\begin{align}
                      s_p^2 =\\frac{\\sum  s_i^2}{I} \\approx \\sigma^2 \\\\
                      \\end{align}$$"),
                                     p(""), 
                                     
                                     p(strong("Here is the trick, we have another way to estimate the population variance, if the group means do not differ the sample means are normally 
                distributed with variance:")),
                                     
                                     p("$$\\begin{align}
                      \\frac{\\sigma^2}{n} = s_\\bar{Y}^2 \\\\
                      \\end{align}$$"),
                                     p(""), 
                                     
                                     p(strong("So if we calculate the variance of the sample means and multiply by the group size ",HTML(" <em>n</em>")," (remember we have equal group sizes), we have a second estimate of the population variance...")), 
                                     
                                     p("$$\\begin{align}
                      \\sigma^2 = (n)(s_\\bar{Y}^2)  \\\\
                      \\end{align}$$"),
                                     p(""), 
                                     
                                     p(strong("Under the null hypothesis of equal means and standard deviation the ratio of the variances")), 
                                     
                                     p("$$\\begin{align*}
                      \\frac{ (n)(s_\\bar{Y}^2)} { s_p^2 }   \\\\
                      \\end{align*}$$"),
                                     p(""), 
                                     
                                     p(strong("will follow an F-Distibution.")), 
                                     
                                     p(strong("The quantity  $$s_p^2$$ has degrees of freedom defined by group size-1 x number of groups")), 
                                     
                                     p(strong("The quantity  $$s_\\bar{Y}^2$$ has degrees of freedom defined by number of groups-1")), 
                                     
                                     p(strong("If the null hypothesis is not true (means not all equal) then  $$(n)(s_\\bar{Y}^2)$$ estimates $$\\sigma^2 + 
                             \\text{positive constant}$$ so that the ratio $$\\frac{ (n)(s_\\bar{Y}^2)} { s_p^2 }$$  will tend to be larger than 1")),
                                     
                                     p(strong("See below an ANOVA table created by manual calculation, tagged on the right is the simple approach expounded upon above, this will only match if the group sizes are all the same, collapse the group size slider to one size to see.")), 
                                     
                                     div( verbatimTextOutput("byhand") ),
                                     
                                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
                                     # for some reason this is need or abpve will not render!
                                     withMathJax(
                                         helpText('
                            $$   $$')),  
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("3 tab", value=3, 
                                     
                                     p(strong("More general calculations follow. That is, the groups do not have to be equal in size. We allow the number of observations to vary from group to group, so the within group estimate of 
                          the population variances now becomes a weighted sum of sample variances. Where ",HTML(" <em>i</em>")," denotes the group, ",HTML(" <em>I</em>")," the number of groups, 
                         ",HTML("<em>s</em>")," the standard deviation and 
                          ",HTML("<em>n</em>"),"
                          the number of observations.")),
                                     
                                     
                                     p("$$\\begin{align*}
                      B= \\frac{   (n_1 - 1)s_1^2  + (n_2 - 1)s_2^2 + ... + (n_i - 1)s_i^2}        {n_1 + n_2 + ... + n_i - i}  =   \\frac{\\sum  (n_i -1)s_i^2} {\\sum (n_i-1)} = \\frac{\\sum  (n_i -1)s_i^2}{n-I} \\\\
                      \\end{align*}$$"),
                                     p(""), 
                                     
                                     
                                     p(strong("So we have the within group estimate of population variance, mean square.")) ,
                                     
                                     p("$$\\begin{align*}
                      B= \\frac{\\sum  (n_i -1)s_i^2}{n-I}   \\\\
                      \\end{align*}$$"),
                                     p(""), 
                                     
                                     p("$$\\begin{align*}
                       \\text{    degrees of freedom}  {=n-I}\\\\
                      \\end{align*}$$"),
                                     p(""), 
                                     
                                     
                                     p(""), 
                                     p(""), 
                                     p(""), 
                                     p(strong("But we have another way to estimate the population variance; between (among) group means, mean square calculated thus...")),
                                     
                                     p("$$\\begin{align}
                     A= \\frac{\\sum  n_i (\\bar{Y}_{i.}  - \\bar{Y}_{..} ) ^2  }{I-1}\\\\
                      \\end{align}$$"),
                                     p(""), 
                                     
                                     p("$$\\begin{align*}
                       \\text{  degrees of freedom}  {=I-1}\\\\
                      \\end{align*}$$"),
                                     p(""), 
                                     
                                     p(strong("The mean of group ",HTML(" <em>i</em>")," is denoted by")),
                                     
                                     p("$$\\begin{align}
                       \\bar{Y}_{i.}   
                      \\end{align}$$"),
                                     p(""), 
                                     p(strong("")),
                                     
                                     p(strong("The grand mean is denoted by")),
                                     
                                     p("$$\\begin{align}
                       \\bar{Y}_{..}   
                      \\end{align}$$"),
                                     p(""), 
                                     
                                     p(strong("As before we can calculate a P-Value with reference to the F distribution. See below, for R code where 'pf' is the F distribution function.
                          This will return the P-Value. The function 'pf' is the probability function, so in order to get the area in the right tail we take way from 1.")),
                                     
                                     p(strong("1 - pf(A/B, ",HTML(" <em> I - 1, n - I </em>)")," ")),
                                     
                                     p(strong(" ")),
                                     p(strong("Below is the ANOVA generated by manual calculations using the above approach:")),
                                     
                                     div( verbatimTextOutput("byhand2") ),
                                     
                                     # for some reason this is need or abpve will not render!
                                     withMathJax(
                                         helpText('
                            $$   $$')),  
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("4 tab", 
                                     
                                     p(strong("Is there evidence that the residuals are skewed or otherwise mis shapen
                in a way that would influence the results? Note, our sample will be imperfect and our population
                will not necessarily be 'perfectly normal' either.
                Here we prefer simple plotting to look for an unspecifiable amount of
                non normality that may help look into any issues rather than a formal approach using statistical tests.")),
                                     
                                   #  div(plotOutput("residual", width=1200, height=800)) ,
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("5 tab", 
                                     
                                     p(strong("IV is the independent variable, DV is the dependent variable. mu and sd are just for information and are the true mean and sd for each IV group.")),
                                     
                                   #  div( verbatimTextOutput("summary2")),
                                     
                            )
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
        dat$yij <- (beta0 + rep(U[,1], times=p)) + (beta1 + rep(U[,2], times=p)) * log(dat$obs) + dat$eij
        
        ### note: use arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma
        ### construction, so that the true error SD is equal to sigma
        
        ### create grouped data object
         dat <- groupedData(yij ~ obs | id, data=dat)
        
        ### profile plots
        #plot(dat, pch=19, cex=.5)
        
        ### fit corresponding growth model
        # res <- lme(yij ~ log(obs), random = ~ log(obs) | id, correlation = corAR1(form = ~ 1 | id), data=dat)
        # summary(res)
        # 
        
        return(list(dat=dat )) 
        
    })  
    
    # --------------------------------------------------------------------------
    # Fit the specified regression model
    fit.regression <- reactive({
        
        data <- make.regression()
        
        df <- data$dat
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Conditionally fit the model
        
        if (input$Model == "base R") {
            
            fit.res <-  
                tryCatch( 
                    lme(yij ~ log(obs), random = ~ log(obs) | id, correlation = corAR1(form = ~ 1 | id), data=df)
                    , 
                         error=function(e) e)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ###http://stackoverflow.com/questions/8093914
            ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
            
            if (!inherits(fit.res, "error")) {
                
               
                fit.res <-  summary(fit.res) # for the residuals
              
                
                
            } else  {
                
                fit.res <- NULL
               
                
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        } else if (input$Model == "VCA package") {          
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            fit.res <-  
                tryCatch( 
                    lme(yij ~ log(obs), random = ~ log(obs) | id, correlation = corAR1(form = ~ 1 | id), data=df)
                    , 
                    error=function(e) e)
            
            
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
    # Set up the dataset based on the inputs 
    # explain <- reactive({
    #     
    #     data <- make.regression()
    #     
    #     df <- data$df
    #     
    #     #### useful statistics
    #     Nj        <- length(df$DV)                # total no of observations
    #     Grandmean <- mean(df$DV)                  # grand mean
    #     grpn      <- tapply(df$DV, df$IV, length) # group sizes
    #     no.grps   <- length(names(table(  df$IV)))# no of groups
    #     means     <- tapply(df$DV, df$IV, mean)   # group means
    #     vars      <- tapply(df$DV, df$IV, var)    # group variances
    #     
    #     # simple approiach only for balanced designs
    #     # estimate sigma2 using a pooled estimate of the variance of each group
    #     ms.wb <-sum(vars)/no.grps
    #     
    #     # we have another way , if the 4 means do not differ the sample means are normally
    #     # distributed with variance sigma2/group size. sigma2/group size can be estimated by the
    #     # variance of the smaple means
    #     # so group size x the above is another estimate of sigma2
    #     
    #     ms.bb <- var(means)*unique(grpn)
    #     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #     # more generally, if groups are of different sizes...
    #     # within sum of squares
    #     ss.w <- sum( (grpn-1) * vars )
    #     
    #     # within df
    #     df.w <- Nj -  no.grps
    #     
    #     # mean square within
    #     ms.w <- ss.w /df.w
    #     
    #     # between sum of squares
    #     ss.b <- sum(grpn * (means - Grandmean)^2)
    #     
    #     # between df
    #     df.b <- no.grps -1
    #     
    #     # mean square between
    #     ms.b <- ss.b /df.b
    #     
    #     #pvalue
    #     pv  <- 1 - pf( ms.b/ms.w, df.b, df.w)
    #     
    #     A <- c(  df.b,   ss.b, ms.b, ms.b/ms.w, pv , ms.bb)
    #     B <- c(  df.w  , ss.w, ms.w, NA,        NA , ms.wb)
    #     
    #     ANOVA <- NULL
    #     ANOVA <- as.data.frame(rbind(A,B))
    #     
    #     n1 <- c("Df","Sum Sq","Mean Sq","F value","Pr(>F)", "Mean Sq balanced only")
    #     n2 <- c("IV","Residuals")
    #     
    #     colnames(ANOVA) <- n1
    #     rownames(ANOVA) <- n2
    #     
    #     ANOVA <-  as.data.frame(ANOVA[,1:6])
    #     ANOVA2 <-  as.data.frame(ANOVA[,1:5])
    #     
    #     return(list( ANOVA=ANOVA, ANOVA2=ANOVA2)) 
    # })  
    
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
            
           
            #df$value <- log(df$value)  #log the GH values!!!!!!!!!!!!!!! 
            
            
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
                
                
                #  geom_segment(aes(x = 1, xend = 3, y = (1), yend=(1)), color = "blue" , size=0.05, linetype="dashed", alpha=0.02) +
                #  geom_segment(aes(x = 1, xend = 3, y = (2.5), yend=(2.5)), color = "blue" , size=0.05, linetype="dashed", alpha=0.02) +
                
                # theme(axis.text.y   = element_text(size=10),
                #       axis.text.x   = element_text(size=10),
                #       axis.title.y  = element_text(size=14),
                #       axis.title.x  = element_text(size=14),
                #       panel.background = element_blank(),
                #       panel.grid.major = element_blank(), 
            #       panel.grid.minor = element_blank(),
            #       legend.position="none",
            #       legend.text=NULL,
            #       legend.title=NULL,
            #       axis.line = element_line(colour = "black", size=0.05),
            #       panel.border = element_rect(colour = "black", fill=NA, size=0.05)
            # ) +
            
            
            
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

# Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }

# Run the application 
# shinyApp(ui = ui, server = server)
