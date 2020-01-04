#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny) 
library(nlme)
library(VCA)
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
                                        choices=c("ggplot", "VCA package plot" )),
                            
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
                            
                            sliderInput("top",
                                        "Select the number of independent groups",
                                        min=3, max=26, step=1, value=7, ticks=FALSE),
                            
                            sliderInput("range1", "Select group sizes: randomly select using range or uniquely select:", 
                                        min = 2, max = 2000, value = c(2, 500), ticks=FALSE) ,
                            
                            sliderInput("range2", "Select true group means: randomly select using range or uniquely select:",
                                        min = -100, max = 100, value = c(-50, 50),ticks=FALSE),
                            
                            sliderInput("range3", "Select true group standard deviations: randomly select using range or uniquely select",
                                        min = 2, max = 50, value = c(3, 16), ticks=FALSE)
                            
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
                                     
                                     div(plotOutput("residual", width=1200, height=800)) ,
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("5 tab", 
                                     
                                     p(strong("IV is the independent variable, DV is the dependent variable. mu and sd are just for information and are the true mean and sd for each IV group.")),
                                     
                                     div( verbatimTextOutput("summary2")),
                                     
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
        
        x1 <- input$range1[1]  #size
        x2 <- input$range1[2]
        x3 <- input$range2[1]  #mean
        x4 <- input$range2[2]
        x5 <- input$range3[1]  #sd
        x6 <- input$range3[2]
        
        top <-  input$top  # number of groups
        
        # seems that I need to use both c(x1,x2) c(x1:x2) so sample function works correctly
        
        if (x1==x2) {
            
            middle <-  sample(c(x1,x2),   top, replace=TRUE)    # choose top count between sliders 
            
        } else {
            
            middle <-  sample(c(x1:x2),   top, replace=TRUE)    #  
        }
        
        
        if (x3==x4) {
            
            lower <-   sample(c(x3,x4),   top, replace=TRUE )  #group means
            
        } else {
            
            lower <-   sample(c(x3:x4),  top, replace=TRUE )  # groups means 
            
        }
        
        if (x5==x6) {
            
            replicates <-  sample(c(x5,x6),  top, replace=TRUE )   #group sds
            
        } else {
            
            replicates <-  sample(c(x5:x6),   top, replace=TRUE )   #grp sds
            
        }
        
        return(list( 
            middle=middle, top=top, lower=lower, replicates=replicates
        ))
        
    }) 
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    make.regression <- reactive({
        
        #   https://stats.stackexchange.com/questions/28876/difference-between-anova-power-simulation-and-power-calculation
        
        sample <- random.sample()
        
        top <-        sample$top
        middle <-     sample$middle
        lower <-      sample$lower
        replicates <- sample$replicates
        
        Nj    <- sum(middle)                  # sum each group size 
        
        muJ   <- rep(lower, rep(middle))      # expand means by group sizes
        
        sds   <- rep(replicates, rep(middle)) # expand sd by group sizes
        
        grpnames <- LETTERS[1:top]
        
        IV <- factor( rep( grpnames, rep(middle) ) )
        
        d <- data.frame(IV=IV,
                        mu= muJ, 
                        sd= sds,
                        x=1
        )
        
        d$DV = rnorm(d$x, d$mu, d$sd)  # create the response
        
        df <- as.data.frame(d)
        
        dd <- plyr::arrange(df, IV)    # sort and create for better order
        
        dd$x <- NULL
        
        return(list(df=df, dd=dd)) 
        
    })  
    
    # --------------------------------------------------------------------------
    # Fit the specified regression model
    fit.regression <- reactive({
        
        data <- make.regression()
        
        df <- data$df
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Conditionally fit the model
        
        if (input$Model == "base R") {
            
            fit.res <-  
                tryCatch(aov(DV ~IV, df), 
                         error=function(e) e)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ###http://stackoverflow.com/questions/8093914
            ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
            
            if (!inherits(fit.res, "error")) {
                
                ff <- fit.res
                fit.res <-  anova(fit.res) # for the residuals
                
                df.b     <- fit.res[['Df']][1] 
                df.w     <- fit.res[['Df']][2] 
                ss.b     <- fit.res[['Sum Sq']][1]
                ss.w     <- fit.res[['Sum Sq']][2]
                ms.b     <- fit.res[['Mean Sq']][1]
                ms.w     <- fit.res[['Mean Sq']][2]
                f        <- fit.res[['F value']][1]
                p        <- fit.res[['Pr(>F)']][1]
                
                
            } else  {
                
                fit.res <- NULL
                ff <- NULL
                
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        } else if (input$Model == "VCA package") {          
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            o <- fit.res<- tryCatch(anovaVCA(DV~IV, df), 
                                    error=function(e) e) 
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (!inherits(fit.res, "error")) {
                
                fit.res <- VCAinference(fit.res, ci.method = "sas")
                ff <- fit.res
                x <- as.matrix(o)
                features <- attributes(x)
                
                emu      <-  (features$Mean) 
                
                o <- as.matrix(o)
                
                df.b     <-  (o["IV",   "DF"])
                df.w     <-  (o["error","SD"])
                ss.b     <-  (o["IV"   ,"SS"])
                ss.w     <-  (o["error","SS"])
                ms.b     <-  (o["IV"   ,"MS"])
                ms.w     <-  (o["error","MS"])
                f        <-  NULL
                p        <-  NULL
                
                
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
        
        return(list(df.b=df.b, df.w=df.w, ss.b=ss.b, 
                    ss.w=ss.w,ms.b=ms.b,ms.w=ms.w,f=f,p=p,
                    fit.res=fit.res, fit.summary=fit.summary, ff=ff
                    
        ))
        
    })     
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    explain <- reactive({
        
        data <- make.regression()
        
        df <- data$df
        
        #### useful statistics
        Nj        <- length(df$DV)                # total no of observations
        Grandmean <- mean(df$DV)                  # grand mean
        grpn      <- tapply(df$DV, df$IV, length) # group sizes
        no.grps   <- length(names(table(  df$IV)))# no of groups
        means     <- tapply(df$DV, df$IV, mean)   # group means
        vars      <- tapply(df$DV, df$IV, var)    # group variances
        
        # simple approiach only for balanced designs
        # estimate sigma2 using a pooled estimate of the variance of each group
        ms.wb <-sum(vars)/no.grps
        
        # we have another way , if the 4 means do not differ the sample means are normally
        # distributed with variance sigma2/group size. sigma2/group size can be estimated by the
        # variance of the smaple means
        # so group size x the above is another estimate of sigma2
        
        ms.bb <- var(means)*unique(grpn)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # more generally, if groups are of different sizes...
        # within sum of squares
        ss.w <- sum( (grpn-1) * vars )
        
        # within df
        df.w <- Nj -  no.grps
        
        # mean square within
        ms.w <- ss.w /df.w
        
        # between sum of squares
        ss.b <- sum(grpn * (means - Grandmean)^2)
        
        # between df
        df.b <- no.grps -1
        
        # mean square between
        ms.b <- ss.b /df.b
        
        #pvalue
        pv  <- 1 - pf( ms.b/ms.w, df.b, df.w)
        
        A <- c(  df.b,   ss.b, ms.b, ms.b/ms.w, pv , ms.bb)
        B <- c(  df.w  , ss.w, ms.w, NA,        NA , ms.wb)
        
        ANOVA <- NULL
        ANOVA <- as.data.frame(rbind(A,B))
        
        n1 <- c("Df","Sum Sq","Mean Sq","F value","Pr(>F)", "Mean Sq balanced only")
        n2 <- c("IV","Residuals")
        
        colnames(ANOVA) <- n1
        rownames(ANOVA) <- n2
        
        ANOVA <-  as.data.frame(ANOVA[,1:6])
        ANOVA2 <-  as.data.frame(ANOVA[,1:5])
        
        return(list( ANOVA=ANOVA, ANOVA2=ANOVA2)) 
    })  
    
    # --------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data1 <- make.regression()
        
        df <- data1$df
        
        # Conditionally plot
        if (input$Plot == "ggplot") {
            
            #base plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #https://rstudio-pubs-static.s3.amazonaws.com/308410_2ece93ee71a847af9cd12fa750ed8e51.html
            require(ggplot2)
            ggplot(df, aes(reorder(IV,DV),DV,fill=IV))+
                # ggplot(tyre, aes(Brands,Mileage,fill=Brands))+ # if you want to leave them alphabetic
                geom_jitter(colour = "gray",width= 0.05) +
                stat_boxplot(geom ='errorbar',width = 0.4) +
                geom_boxplot()+
                labs(title="Boxplot, dotplot and Standard error of mean for groups", 
                     x = "Groups (sorted)",
                     y = "Response",
                     subtitle ="Gray dots=sample data points, Black dot=outlier, Blue dot=mean, Red=99% confidence interval",
                     caption = "") +
                guides(fill=FALSE) +
                stat_summary(fun.data = "mean_cl_normal", colour = "red", size = 1.5, fun.args = list(conf.int=.99)) +
                stat_summary(geom="point", fun.y=mean, color="blue") +
                theme_bw() 
            
        } else {
            
            #VCA plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            require(VCA)
            varPlot(DV~IV, df, 
                    BG=list(var="IV", 
                            col=c("#f7fcfd","#e5f5f9","#ccece6","#99d8c9",
                                  "#66c2a4","#41ae76","#238b45","#006d2c","#00441b"), 
                            col.table=TRUE), 
                    VLine=list(var=c("IV"), 
                               col=c("black", "mediumseagreen"), lwd=c(2,1), 
                               col.table=c(TRUE,TRUE)), 
                    JoinLevels=list(var="IV", col=c("lightblue", "cyan", "yellow"), 
                                    lwd=c(2,2,2), 
                                    MeanLine=list(var="DV", col="blue", lwd=2) ,
                                    
                                    # Title=list(main=paste("Variability Chart. Truth (estimate): intercept "
                                    #                       ,input$intercept,"(",fit.regression()$emu,"), top level sd=",
                                    #            input$a,"(",fit.regression()$etop,")", ",\n middle level sd=",
                                    #            input$b ,"(",fit.regression()$eday,"), lowest level sd=",
                                    #            input$c, "(",fit.regression()$erun,") & random error sd=", 
                                    #            input$d,"(",fit.regression()$esigma,")")),
                                    
                                    # MeanLine=list(var="mid", col="pink", lwd=2),
                                    Points=list(pch=list(var="mid", pch=c(21, 22, 24)), 
                                                bg =list(var="mid", bg=c("lightblue", "cyan", "yellow")), 
                                                cex=1.25))    )
        }
        
    })
    #---------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot residuals 
    
    output$residual <- renderPlot({         
        
        # Get the current regression model
        d  <- fit.regression()
        
        f<- d$ff
        
        par(mfrow=c(3,2))
        plot(f)
        
        #dd <- d$fit.res
        anova.residuals <- residuals( object =  f) # extract the residuals
        # A simple histogram
        hist( x = anova.residuals , breaks=50, main=paste("Histogram of ANOVA residuals, SD=",p2(sd(anova.residuals)),"")) # another way of seeing residuals
        par(mfrow=c(1,1)) 
        
    })
    
    #---------------------------------------------------------------------------
    # Show the summary for the 
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression()$fit.summary
        
        if (!is.null(summary)) {
            
            return(fit.regression()$fit.summary)
        }
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # the data to print
    output$summary2 <- renderPrint({ 
        
        return(make.regression()$dd)
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # the data to print, I wooulf like to reuse this but dont think it is possible? So I add another function to collect the same information below
    output$byhand <- renderPrint({
        
        return(explain()$ANOVA)
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$byhand2 <- renderPrint({
        
        return(explain()$ANOVA2)
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
