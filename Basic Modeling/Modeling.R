##############################################
##############################################
#### GVPT Political Methodology R Workshops ##
#### Eric Dunford (edunford@umd.edu)        ##
#### November 4, 2016                       ##
##############################################
##############################################
######## ****  Workshop 3 **** ###############
######## ****  MODELING_1 **** ###############
##############################################
##############################################

install.packages("dplyr")

require(dplyr)

# Plan for today:


    # (a) Review advanced data manipulations from last time
    # (b) GGPLOTs 
    # (c) Linear Models (Diagnostics, Basics, Hetero., Post-Estimation)
    # (d) Generalized Linear Models (hetero, marginal effects/pred. probs etc.)
    

# ------------------------------------ LAST TIME -----------------------------------

# We need to review a few things from last time that we weren't able to cover --
# mainly advanced data manipulations. These manipulations are common in 
# political science, so having different ways to approach the problem is useful.



####
####                            *** LAGS *** 
####

      # GENERATE fake data
      data <- data.frame(country=c(rep("A",10),rep("B",10)),
                         year=rep(1995:2004,2),
                         v1=runif(20,0,10),stringsAsFactors = F)
      
      
      head(data)

      # Issue!
        data$lag1 <- lag(data$v1,1) # Lag
      
        # Country A's values "bleed into" Country B's...no good.
        head(data)
      
      
        
      # Solution! 
        
      # (1) Lag Using a Loop
          groups = unique(data$country)
groups 

          for (i in 1:length(groups) ){
            lagged_value = lag(data[data$country==groups[i],"v1"])
            data[data$country==groups[i],"lag2"] <- lagged_value
          }
          head(data)
          
          
      # (2) Lag using dplyr approach
          
          data <- data %>% group_by(country) %>% mutate(lag3 = lag(v1, order_by=year))
          head(data)
          
          
          
          
####          
####                    *** Event Counters *** 
####          
          
          
          # GENERATE fake data
          event_data <- data.frame(country="Nigeria",
                                   year=1999:2010,
                                   event = c(1,0,0,0,1,0,0,0,0,1,0,0))
          event_data 
          
          
          
          # (1) Use a Loop
              j = 0
              event_data$time_since <- 0
              for(i in 1:nrow(event_data)){
                if(event_data$event[i]==1){
                  j <- 0
                  event_data$time_since[i] <- j
                }else{
                  j <- j + 1
                  event_data$time_since[i] <- j
                }
              }
              event_data
              
              
          # (2) Use dplyr
              
              event_data2 <- event_data %>% 
                group_by(country, spell_id = cumsum(event == 1)) %>% 
                mutate(time_since2 = row_number()-1) %>% 
                select(-spell_id) %>% ungroup(.)
              event_data2
              
          # (3) Use duration package
              install.packages("spduration")
              require(spduration)

              event_data3 <- add_duration(event_data,
                                          y="event",
                                          unitID="country",
                                          tID="year",
                                          freq="year")
              
              # Clean so that it doesn't count the initiation period
              event_data3[event_data3$event==1,"duration"] <- 0 
              event_data3[,c(1:3,11)]
              
              
              
####
####                        *** Expansions/Contractions *** 
####              
              
              
              
              # GENERATE fake data
              data <- data.frame(country = c("Russia","US","Belize","Mexico"),
                                 start=c(1994,1992,2000,1990),
                                 end=c(1998,1995,2002,2003),
                                 eventA=c(1,0,1,0),
                                 measureA=c(56.89,72.90,81.32,34.89))
              data
              
              
              # EXPANSION
              out <- c() # Create a container
              for(i in 1:nrow(data)){
                data_range <- seq(from=data$start[i],to=data$end[i],by=1) 
                dd <- data.frame(year=data_range)
                vars = data[i,c("country","eventA","measureA")] # The other vars
                rownames(vars) <- NULL
                data_out <- data.frame(dd,vars)
                out <- rbind(out,data_out)
              }
              out
              
              
              # CONTRACTION
              out %>% group_by(country) %>% 
                summarise(.,start=min(year),end=max(year),eventA=max(eventA),measureA=max(measureA))
             
              
              
####        
####                        *** "Wide" Data *** 
####
          
              # GENERATE fake data (similar to World Bank data)
              data <- data.frame(country=c("A","B","C","D","E"),
                                 matrix(rnorm(50,6,10),nrow=5,ncol=10))
              colnames(data)[2:11] <- 2000:2009
              data
              
              
          
              # (1) Using a loop
              out <- c() # create a container
              for( country in 1:nrow(data)){
                x <- t(data[country,-1]) %>% as.data.frame(.)
                colnames(x) <- "indicator"
                x$year <- rownames(x)
                rownames(x) <- NULL
                x$country <- data[country,1]
                the_goods <- x[,c("country","year","indicator")]
                out <- rbind(out,the_goods)
              }
              out
          
              
              # (2) Using "Reshap2"
              # install.packages("reshape2")
              require(reshape2) # load the package
              
              data2 <- melt(data,id="country")
              colnames(data2) <- c("country","year","indicator")
              data2 
          
      
      
      
              
# -------------------------------------- GGPLOT2 -----------------------------------
install.packages("ggplot2")
install.packages("gridExtra")
require(ggplot2)
require(gridExtra)
              
            # We'll use data that is store in the ggplot package. The data is
            # big, so we'll sample from it.
            data = diamonds %>% sample_n(1000)
            
            head(data) # Data on diamonds
            dim(data)
            
            
            # ggplot2 is a versatile and robust graphics language in R. With it,
            # we are able to make some impressive graphic manipulations. It 
            # operates as a graphics "language" where the additive functions do 
            # what it sounds like they should do (the package was made by the
            # same guy who designed the dplyr package.)
              
            # Two basic graphing approaches exist:
            
                # ggplot() # Which we build on
                # qplot()  # Which can serve as a "quick plot"
            
            
####            
####                      ****** qplot() ******
####
            
            # qplot() adapts instantly to the variable 
            
            # Bar Plots
            qplot(x=cut,data=data)
            
            # Histograms
            qplot(x=depth,data=data)
            
            # Scatter
            qplot(y=price,x=carat,data=data)
            
            # Box plots  (geom == "the style of plot I want")
            qplot(factor(cut), price, data = data, geom = c("boxplot"))
            
            # Jitter Plots
            qplot(factor(cut), price, data = data, geom = c("jitter"))
            
            # Violin plots
            qplot(factor(cut),price, data = data, geom = c("violin"))
            
            # Etc. 
            
            # There are many options contained within. Feel free to explore!
            ?qplot
            
            # For example, here we have color set by the cut, the size of the
            # points set by the depth. 
            qplot(y=price,x=carat,data=data,color=cut,size=depth)
            
            # Also note that ggplots can be stored as objects
            plot1 = qplot(y=price,x=carat,data=data)
            plot1
    
                    
####
####                        **** ggplot() ****
####            
            
            # The more versatile way to use ggplot is to "build" with it. 
            
            # ggplot() takes two fundamental arguments: data and aesthetics "aes()"
            # (which are values contained in the data) 
            
            ggplot(data,aes(x=carat,y=price)) # basic plotting window
            
            # we then "add" the functional value that we want to the open graph
            
            ggplot(data,aes(x=carat,y=price)) + geom_point()
            
            
            
            # Everything can then be added on including unique "themes" that
            # alter the aesthetics of the graphic
            
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_bw()
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_classic()
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_dark()
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_light()
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_linedraw()
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_minimal()
            # etc. .... 
            
                # any additive function also works with "qplot()"
                qplot(x=carat,data=data) + theme_bw()
            
            ?qplot
            
            # Labeling takes on the same form
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_bw() + 
              labs(x = "Carats",y = "Money I'll Never Have",title="My Diamond Plot")


            # OR
            ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_bw() + 
            xlab("Carats") + ylab("Money I'll Never Have") + ggtitle("My Diamond Plot")
            
            # Note that since we can save a graphic as an object, we can easily
            # build off it by using the same additive framework.
            p = ggplot(data,aes(x=carat,y=price)) + geom_point() + theme_bw()
            p
            p = p + labs(x = "Carats",y = "Money I'll Never Have",title="My Diamond Plot")
            p
            
            
            
            # We can have tight control over the coloring of the graphic,
            # especially when we "group" the data
            
            ggplot(data,aes(x=carat,y=price,group=cut,color=cut)) + geom_point() + theme_bw() 
            
            p2 = ggplot(data,aes(x=carat,y=price,group=cut,color=cut)) + geom_point() + theme_bw() +
              scale_colour_manual(name='', values=c('Fair'='grey', 'Good'='red', 
                                                          'Very Good'='blue', 'Premium'='yellow', 
                                                          'Ideal'='black'))
            p2
            
            # And the "dimensions" of the plot
            p + xlim(0,5) + ylim(0,50e3)
            p + coord_flip()
            
            
            # In addition there are a lot of useful functions that map onto
            # existing graphics
            p + geom_smooth() # Lowess Reg Line with bootstrapped CIs
            p + geom_hex()
            p + geom_hline(yintercept = 10e3,color="red",lwd=4)
            
            
            # The "theme" function really lets you alter any micro details. 
            
            # Angling the axis values
            p + theme(axis.text.x=element_text(angle=45), axis.text.y=element_text(angle=90))
            
            # Altering the back panel 
            p + theme(panel.background = element_rect(fill = 'steelblue'),
                      panel.grid.major = element_line(colour = "#EB7B1D", size=3),
                      panel.grid.minor = element_line(colour = "blue", size=1))
            
            # Reseting the margins and the backdrop
            p + theme(plot.background=element_rect(fill="lightgrey"), 
                      plot.margin = unit(c(2, 4, 1, 3), "cm")) 
            
            # Dropping or repositioning legends
            p2
            p2 + theme(legend.position="none")
            p2 + theme(legend.position="bottom")
            
            
            # All in all, if you can think it, you can probably change it. This makes the 
            
            p2 + theme_bw() + theme(panel.border = element_blank(),
                                    axis.line.y = element_line(colour = "black"),
                                    axis.line.x = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    axis.ticks.length=unit(-0.20, "cm"), 
                                    axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                                    axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                                    plot.margin = unit(c(1,1,1,1), "cm"),
                                    axis.text=element_text(family = "serif"),
                                    axis.title=element_text(family = "serif"),
                                    legend.position="bottom",
                                    legend.key = element_blank(),
                                    legend.text = element_text(family = "serif")) 
            
            
####
####                      **** Griding Plots **** 
####        
            
            # With the gridExtra package, we can easily stack plots.
            head(data)
            p = qplot(factor(cut),carat,data=data,geom="violin") + theme_bw() + 
              geom_jitter(width = .2,col="darkgrey")
            p2 = ggplot(data,aes(x=carat,fill=cut)) + geom_bar() + theme_bw() + 
              theme(legend.position="none")
            p3 = qplot(carat,price,data=data) + theme_bw() + geom_smooth()
            
            grid.arrange(p,p2,p3,ncol=1)
            grid.arrange(p,p2,p3,nrow=1)
            
# This is only the start. There is a lot more functionality and many packages 
# that have been built off the ggplot framework, making it the dominant graphics
# language in R.
            
            # For example, ggmap()...
            # install.packages("ggmap")
            require(ggmap)
            map = qmap(location="Morrill Hall, College Park, MD 20740", 
                       zoom = 18, source = "osm")
            map  # HERE WE ARE!
            
            
              
# ---------------------------- Diagnostics and Linear Models -----------------------------------

             
          # Simulating the Data Generating Process
              
                  set.seed(235) # For replication of random vars
                  
                  x <- rnorm(100,10,.4) # random variable
                  e <- rnorm(100,0,1) # assumption-following error
                  
                  y <-  1 + 2*x + e # y is a linear function of x + error
                  
                  # visualize
                  grid.arrange(qplot(y),qplot(x),qplot(x,y)+ geom_smooth(),ncol=3)
                  
                  data = data.frame(y,x)
                  head(data)
                  summary(data)
              
                  
####                  
####                  *** Initial Diagnostics ***
####                  
                  
                  # Correlation
                  cor(y,x)
                  
                  # Correlation Tests
                  cor.test(y,x,method = "pearson")
                  cor.test(y,x,method = "kendal")
                  
                  # Normal Distribution Test (Shapiro)
                  shapiro.test(y) # NULL == "normally distributed"
                  shapiro.test(x) # NULL == "normally distributed"
                  
                  # Difference in means (t-test)
                  t.test(y,x) # NULL == "no meaningful difference in means"
                  
                  # Difference Variance Test
                  var.test(y,x)  # NULL == "no meaningful difference in var"
                  
                  # Differences in distributions (Kolmogorov And Smirnov)
                  ks.test(y,x) # NULL == "drawn from the same dsitribution"
                  
                  # Other useful test:
                  # chisq.test() # for categorical variables
                  # wilcox.test() # a t-test for two variables that aren't normal
                  
              
                  
####                  
####                       *** Linear Model ***
####                  
                  
                  
                  
              # The modeling framework in R straightforward:
                  # takes a formula (any with a tilde, e.g. "y~x")
                  # takes data
                  
                  
              model1 = lm(y~x,data=data)
              
              # The model output is only so informative
              model1
              
              # so use "summary" function
              summary(model1) 
                  # Significant, and we recover the true coefficient. 
              
              
              
              # Model Diagonostics, which offer
                        # (1) linearity test,
                        # (2) normality assump. test, 
                        # (3) equal variance test (homosk.),
                        # (4) locating outliers (influencial cases)
              plot(model1)
              
              
              
              
                # To demonstrate the usefulness of the data diagnostics, consider 
                # the following "BAD DATA" (i.e. data that doesn't follow the
                # necessary assumptions)
                
                      # Insert extreme value (influence of outlier)
                          x2 = x 
                          x2[100] <- 21
                          
                          y2 = 1 + 2*x2 + e 
                          bad1 = data.frame(y=y2,x=x2)
                          qplot(x,y,data=bad1)
                          
                          model2 = lm(y~x,data=bad1)
                          summary(model2)
                          plot(model2)
                          
                          
                      # Heteroscedasticity in the errors
                          
                          # Generate variance in the errors that are dependent on
                          # values of x
                          M = cbind(x,e) 
                          M[M[,1]>=10.2,2] = rnorm(nrow(M[M[,1]>=10.2,]),0,4)
                          
                          y2 = 1 + 2*x + M[,2]
                          bad2 = data.frame(y=y2,x=x)
                          qplot(x,y,data=bad2)
                          
                          model3 = lm(y~x,data=bad2)
                          summary(model3)
                          plot(model3)
                        
                  
              
                        
            # Drawing out useful values
                        
                  m1.sum = summary(model1)
                        
                  # Summary and model output contain different info
                  str(model1)
                  str(m1.sum)
                  
                        
                  # Coefficients
                  model1$coefficients
                  m1.sum$coefficients
                    
                      # Specifically,
                      m1.sum$coefficients[,4] # P-values
                      m1.sum$coefficients[,3] # T-Stats
                      m1.sum$coefficients[,2] # Std. Err
                      m1.sum$coefficients[,1] # coefs
                  
                        
                  
                  # Original data
                  model.matrix(model1) # X matrix
                  model1$model # Original Data
                      
                  
                  
                  # Prediction (Y-hats)
                  model1$fitted.values
                  yhat = predict(model1)
                  qplot(x,yhat)
                  
                        # OR Build it yourself!
                        X  = model.matrix(model1) 
                        B = model1$coefficients
                        yhat2 = X%*%B
                        qplot(yhat,yhat2)
                        
                        
                  
                  
                  # Residuals
                  res1 = m1.sum$residuals
                  res2 = data$y - yhat
                  identical(round(res1,2),round(res2,2))
                  qplot(res1,res2)
                  
                  
                  
                  
                  # Variance-Covariance Matrix
                  vcov(m1.sum)
                  m1.sum$cov.unscaled
                  
                  
                  # Stats
                  m1.sum$fstatistic
                  m1.sum$r.squared
                  # Goodness of Fit Stats: the lower the better
                  AIC(model1) 
                  BIC(model1) 
                  
       
                             
####             
####                *** HETEROSKEDASTICITY ***
####       
                  
                  
            # NOTE: standard error corrections in R (as compared to STATA) are a 
            # pain in the butt. There is no getting around it. Let's me provide you 
            # with some useful corrections that will make your life easier.
        
                          
            # Some Useful Packages
            install.packages("sandwich")
            install.packages("lmtest")
            require(sandwich)
            require(lmtest)
            
            
            # Recall our "Bad" model with the heteroskedasticity from above?
            qplot(x,y,data=bad2) + theme_bw()
            
            
            model3 = lm(y~x,data=bad2)
            summary(model3)
            
            
            # Test for Hetero.
            bptest(model3) # Breusch-Pagan test to see if the errors are consistent
            # NULL == "varyign variance" OR "Non-constant variance across all values of x"
            
            
            # !! Adjustments !!
            
            
                vcov(model3) # Standard Errors
                vcovHC(model3) # White-Adjust Std. Errors
                
                
                # "coeftest()" from the lmtest package presents estimates as we're use to 
                # reading them
                
                coeftest(model3)
                
                # Here we can insert the adjusted covariance matrix to get robust standard errors
                coeftest(model3,vcov. = vcovHC(model3)) 
                
                
                
                # Are these consistent with the robust standard errors in STATA?
                    foreign::write.dta(bad2,file="~/Desktop/bad.dta")
                    # reg y x,robust
                    # Not exacly...
                    
                
                
            # "BEATING YOUR DATA WITH A HAMMER"  --- STATA robust std. errors
                
                # Three Ways: one long (but you know what is going on), another easy
                # because R has a great community
                
                
                    
                # ONE: [HARD] Build it yourself -- STATA implements a degrees of freedom 
                # adjustment to their robust standard errors, which adds another
                # layer to the process
                    
                   
                    # Remember that the variance-covariance matrix is just....
                        X <- model.matrix(model1)
                        t(X)%*%X * (1/nrow(x))
                        vcov(model1)
                        
                    # For adjusted std. errors, we need this... 
                    # $$ Variance_{robust} = \bigl (\frac{N}{N-K} \bigr )(X'X)^{-1}\sum_{i=1}^N \{X_iX_i'\hat{\varepsilon}_i^2\}(X'X)^{-1} $$
                    
                    
                    # Let's design our own function...   
                    robust_white <- function(model) {
                      s <- summary(model)
                      X <- model.matrix(model)
                      squared_errors <- residuals(model)^2
                      
                      # Sum of the var-covar (with residual adjustment and without df adjust)
                      meat <- 0
                      for(i in 1:nrow(X)) {
                        meat <- meat + squared_errors[i]*X[i,]%*%t(X[i,])
                      }
                      X_inverse <- solve(t(X)%*%X) # inverse(X'X)
                      
                      # Inverse matrices as bread, adjusted raw vcov as meat
                      varcovar <- X_inverse %*% meat %*% X_inverse # adjusted vcov sandwich
                      dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X)) # degrees of freedom adjustment
                      stdh <- dfc*sqrt(diag(varcovar)) # standard errors
                      
                      # Adjust the t-stat and p-vals
                      t <- model$coefficients/stdh
                      p <- 2*pnorm(-abs(t))
                      
                      # Map Adjustments on Original Summary Object
                      s$coefficients[,"Std. Error"] <- stdh
                      s$coefficients[,"t value"]  <- t
                      s$coefficients[,"Pr(>|t|)"] <- p
                      return(s)
                    }
                    
                    robust_white(model3)
                
                
                    
                    
                # TWO: [EASIER] use the "vcovHC()" function and specify "HC1" which is stata's default
                    coeftest(model3,vcov. = vcovHC(model3,"HC1")) 
                
                  
                
                # THREE: [EASIEST] Download new functionality from u/IsidoreBeautrelet:
                # https://economictheoryblog.com/2016/08/08/robust-standard-errors-in-r/
                
                    # DOWNLOAD CODE FROM GITHUB:
                    install.packages("RCurl")
                    library(RCurl) # Web-Scraping/Interaction Package
                    # import the function from repository
                    url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
                    eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
                         envir=.GlobalEnv)
                    
                    # Robust-Standard errors are now built into the functionality of
                    # the summary() function. Cool!
                    summary(model3,robust=F)
                    summary(model3,robust=T)
                      
                    
                    
                    
####                      
####              *** CLUSTERED STANDARD ERRRORS ***
####              
                    
                    
                # Assume you have "groups" in your data (i.e. "clusters): these 
                # could be states, rebel groups, countries, you name it. Now, 
                # assume that the errors are homoskedastic within those groups 
                # but heteroskedastic across them, which is to say, that the 
                # variance is different given different groups.
                    
                    
                    
                # First, let's create some "FAKE DATA" that simulates the
                # assumed conditions.
                    
                    # Say we have 5 groups with their own unique variance signatures
                    set.seed(2234)
                    rebs1 <- rnorm(20,1,3)
                    rebs2 <- rnorm(20,1,.2)
                    rebs3 <- rnorm(20,1,1.1)
                    rebs4 <- rnorm(20,1,2.6)
                    rebs5 <- rnorm(20,1,5)
                    x = c(rebs1,rebs2,rebs3,rebs4,rebs5)
                    
                    # Law-abiding error
                    e <- rnorm(100,0,1)
                    
                    y = 1 + 3*x + e # y is a linear combination of our groups
                    
                    groups = c(rep("rebs1",20),rep("rebs2",20),
                               rep("rebs3",20),rep("rebs4",20),
                               rep("rebs5",20))
                    cl.data = data.frame(y,x,groups)
                    
                    qplot(x,y,data=cl.data)
                    
                    model4 = lm(y~x,data=cl.data)
                    summary(model4)
                    plot(model4)
                    bptest(model4) # Things are looking hetero.
                    
                
                    
                    
                # TWO WAYS to deal with clustered standard errors  
                    
                    # Again, using the "lmtest" and the "sandwich" package, we can
                    # build an easy solution to this.
                    
                # ONE: [HARD] Build it yourself
                    
                    # For Clustered standard errors we need this:
                    # $$ Variance_{cluster} = \bigl (\frac{M}{M-1}\frac{N-1}{N-K} \bigr )(X'X)^{-1}\sum_{j=1}^M \{X_M\hat{\varepsilon}_M\hat{\varepsilon}'_MX_M\}(X'X)^{-1}$$
                    # Where:
                        # M = no. of clusters
                        # N = no. of obs.
                        # K = no. of covars
                    
                cluster_se   <- function(data, model, cluster){
                  M <- length(unique(cluster))
                  N <- length(cluster)
                  K <- length(model$coefficients)
                  degrees_adj <- (M/(M-1))*((N-1)/(N-K))
                  cluster_estimates  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
                  clustered_vcov <- degrees_adj*sandwich(model, meat=crossprod(cluster_estimates)/N)
                  coeftest(model, clustered_vcov) 
                }
                cluster_se(cl.data,model4,cl.data$groups)
                
                    
                # TWO: [EASY] with the functionality that we imported form 
                # u/IsidoreBeautrelet, we also got a clustering option in the
                # summary. So after importing the github code, we can just...

                summary(model4,cluster = "groups")
                
                # STATA check 
                foreign::write.dta(cl.data,file="~/Desktop/group.dta")
                
                
                
                
                
####                
####                *** OUT OF SAMPLE Predictions ***
####
                
          # Out of sample predictions have a simple premise, can we use some of 
          # the data to fit our model, and then use the other portion of the 
          # data to see how well that fit performs. This is a really useful way
          # to test model fit.
          
          # Here let's create a "training" and "test" dataset using the
          # IRIS data.
          
          data = iris 
          head(data)
          
          set.seed(5556)
          train.loc = sample(1:nrow(data),.8*nrow(data)) # Let's extract 80% of the data to train
          train.set = data[train.loc,]
          test.set = data[-train.loc,]
          
          mod1 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data=train.set)
          summary(mod1)
          
          # Using the model, predict the values for the test set
          preds = predict(mod1,test.set)
          
          # Let's compare the "actual" the values in the test set to the
          # predicted values
          compare = data.frame(actual=test.set$Petal.Length,prediction=preds)    
          cor(compare) # Highly correlated! (.95) Which is a good sign of model fit
          
          # Minimum-Maximum Accuracy
          mean(apply(compare,1,min)/apply(compare,1,max))
          
          # Absolute Error Deviation
          mean((abs(compare$prediction-compare$actual))/compare$actual)
          
          # Squared Error Deviation
          mean(((compare$prediction-compare$actual)^2)/compare$actual)
          
          
####          
####            *** K-Fold Validation ***
####       
          
          # The logic behind a K - fold validation is simple: break your data up
          # into K number of random "portions" are drawn. Specficially, Each
          # fold is removed, in turn, while the remaining data is used to re-fit
          # the regression model and to predict at the deleted observations. If
          # the prediction accuracy doesn't change a whole lot, then we have a
          # good model.
          
          # Specifically, we want to make sure that the prediction sample
          # doesn't vary to much, and that the fits don't differ that much
          # either.
          
          install.packages("DAAG")
          require(DAAG)
          
          val_results = suppressWarnings(cv.lm(data=data,form.lm=Petal.Length ~ Sepal.Length + Sepal.Width,
                                               m=5,printit=FALSE))   
          
          str(val_results)
          attr(val_results,"ms") # Mean Squared errors (we want these to be small)
                  
                  
        
          # So, let's compare some fits!
          form.full = Petal.Length ~ Sepal.Length + Sepal.Width  # FUll Model
          form.simple = Petal.Length ~ Sepal.Length # Simple Model
          
          # 5 folds
          results_full = suppressWarnings(cv.lm(data=data,form.lm=form.full,m=5,printit=FALSE))   
          results_simple = suppressWarnings(cv.lm(data=data,form.lm=form.simple,m=5,printit=FALSE))   
          
          attr(results_full,"ms")
          attr(results_simple,"ms")
                  
          # Clearly the full model performs better! So we have good evidence
          # that including Sepal.Width brings something to the table!
          

          
          
          
          
# ----------------------- Generalized Linear Models -----------------------------------         

          
####            
#### Binary Outcomes (Bernoulli-Distributions)
####
              
          
          # First, let's again simulate the data generating process. 
          
                set.seed(1988)
                x1 <- rnorm(500,.1,1) 
                x2 <- round(runif(500,0,3))
                # e <- rnorm(500,0,1) 
                z <- .2 + -.8*x1 + .2*x2 # Latent Linear combination
                pr = exp(z)/(exp(z) + 1) # Logit-Link
                # pr = pnorm(z) # Gausian-Link
                y <- rbinom(length(pr),1,pr) # Fit to a binomial distribution
                
                data = data.frame(y,x1,x2)
            
               
                
                # The probability of observing y == 1 and y == 0
                mean(pr); 1 - mean(pr)
                prop.table(table(y))
                
                data = data.frame(y,x1,x2)
                
                qplot(x1,y) + geom_smooth(se = F) + theme_bw()
                qplot(x2,y) + geom_smooth(se = F) + theme_bw()
                
                
                  
          # As usual, we need to decide on our link function of choice. I'll
          # proceed using the logit, but probit is quite simple to impliment. 
          
              # Logit Link Function
              model1 = glm(y~x1+x2,data=data,family=binomial(link="logit"))
              summary(model1)     
              
              # Probit Link Function
              model2 = glm(y~x1+x2,data=data,family=binomial(link="probit"))
              summary(model2)
          
          
          # Extraction of meaningful values follows the same logic as the linear model
          
          m1.sum = summary(model1)
          
          # Coefficients
          model1$coefficients
          coefficients(model1)
          m1.sum$coefficients
          
          # Data 
          model1$data
          
          # Variance-Covariance Matrix
          m1.sum$cov.scaled
          vcov(model1)
          
          # Predictions
          mean(predict(model1,type="response"))
          1-mean(predict(model1,type="response"))
 
                   
          
####          
#### *** HETEROSKEDASTICITY *** 
####
          
          # Again, we can make adjustments to the variance-covariance matrix
          # using the "sandwich()" and "lmtest()" package 
          
          ####
          #### * Robust Standard Errors *
          ####
          
              coeftest(model1,vcovHC(model1,"HC1"))
          
          ####
          #### * Clustering Standard Errors * 
          ####
          
            # One of the easiest ways to cluster standard errors is using the
            # "rms" package.
          
            # install.packages("rms")
            library(rms)
            
            # Let's make some fake groups in the fake data
            data2 <- data
            data2$groups <- c("A","B","C")[round(runif(500,1,3))]
              
            robcov(lrm(y~x1+x2, x=T, y=T, data=data2), cluster=data2$groups)  
    
              
                       
####
####        *** Marginal Effects/Predicted Probabilities ***
####              
          
          # R is getting better about predicted probabilities and marginal 
          # effects. Specifically, here is a package that will calculate 
          # marginal effects for a logit model while simultaneously calculating
          # robust or clustered standard errors. 
          
          # install.packages("mfx")
          require(mfx)
              
          me = logitmfx(formula=y~x1+x2, data=data,atmean = F,robust = T)
          me
          # when "atmean" == F, we retrieve the average partial effect (observed
          # value); robust == T (White/robust standard errors); clustervar1 ==
          # "var.name" (clustered) standard errors
          
    
                
####          
####            "MARGINS" plots in R 
####         
          
          
          install.packages("devtools") # To install developing code NOT on cran
          devtools::install_github("leeper/margins")
          require(margins)
          
          m = margins(model1,data=data)
          plot(m)
          summary(m)
          
          # Plot Predicted Probabilities 
          cplot(model1, x = "x1", se.type = "shade",what="prediction")
          cplot(model1, x = "x2", se.type = "shade",what="prediction")
          
          # Plot Marginal Effects
          cplot(model1, x = "x1", se.type = "shade",what="effect",type="response",
                col="darkgrey",lwd=4,se.fill ="#97C3FB")
          cplot(model1, x= "x2", se.type = "shade",what="effect",type="response",
                col="darkgrey",lwd=4,se.fill ="#97C3FB",n=3)
          cplot(model1, x= "x1",dx = "x2", se.type = "shade",what="effect",
                col="darkgrey",lwd=4,se.fill ="#97C3FB",scatter=T)
          
          # 3D plots of the marginal effects?
          persp(model1,xvar="x1",yvar="y")
          persp(model1,xvar="x2",yvar="y")
          
          # Or a density image 
          image(model1,xvar="x1",yvar="y")
          
          
          # OR take the data and do your own thing with it in ggplot
          tmp <- cplot(model1, x = "x1", what = "effect", draw = FALSE)
          ggplot(tmp, aes(x = xvals, y = yvals)) + 
            geom_line(lwd = 2) + 
            geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)+ 
            theme_bw() + labs(x="Predictor",y="Marginal Effect on Y")
         
      
              
####          
####      Predicted Probabilities ("Observed Values Approach")
####      
          # install.packages("mvtnorm")
          require(mvtnorm) # for multivariate normal distribution
          
          n_draws <- 1000 # Number of simulations
          manipulate <- 0:3 # Here Let's manipulate 
          sim_coefs <- rmvnorm(n_draws, model1$coef, vcov(model1)) # Multinormal simulated coefs. 
          
          output = c() # Container for full output
          for(j in 1:length(manipulate)){
            
            X <- cbind(1,model1$model[,-1]) # Drop DV, Add Constant
            
            # Set the manipulation as a constant for the predictor ... but
            # keeping the other vars at their "observed" values
            X[,3] <- manipulate[j] 
            X <- as.matrix(X)
            
            pred_prob = c() # Container to store pred. probs
            for(i in 1:n_draws){                 
              linpred <- X %*% sim_coefs[i,]
              prediction  <- exp(linpred)/(exp(linpred)+1) # Logit Link
              sim_mean	<- mean(prediction,na.rm=TRUE) 
              pred_prob = c(pred_prob,sim_mean)
            }
            
            pp <- mean(pred_prob) # Mean Value
            ci = quantile(pred_prob,probs = c(.05,.95),names = F) # Conf. Inter.
            
            temp <- data.frame(var=manipulate[j] , pp, lower=ci[1], upper=ci[2])
            output <- rbind(output,temp)
          }
          
          # Plot 
          ggplot(output,aes(x=var,y=pp)) + geom_point() + 
            geom_segment(aes(y=lower,yend=upper,x=var,xend=var)) +
            theme_minimal() + labs(x="Manipulated Var",y="Pr(y)")
          
          
         
          
          # apply() family approach 
              # If you have many values to iterate through, the above code is not
              # efficient enough. No worries! Just Vectorize using the apply family!
          
          
              manipulate <- fivenum(model1$model[,2]) # All values of the x1
              out = sapply(1:length(manipulate),function(j){
                X <- as.matrix(cbind(1,model1$model[,-1]))
                X[,2] <- manipulate[j] # position should always correspond with loc in mat
                predProbs = apply(sim_coefs,1,function(x){
                  p = X %*% x; pr = exp(p)/(exp(p)+1);mean(pr,na.rm=T)
                })
                data.frame(var=manipulate[j] , pp=mean(predProbs), 
                           lower=quantile(predProbs,.05,names = F), 
                           upper=quantile(predProbs,.95,names = F))
              })
              results = as.data.frame(t(out))
          

              
              
####
####                      **** OTHER GENERALIZATIONS ****
####              
          
          ###        
          ###  POISSON Distributed data 
          ###    
              
              # Simulate DGP
              set.seed(1998)
              x <- runif(100,0,5)
              lambda = exp(1 + .3*x)
              y <- rpois(100,lambda)
              count = data.frame(y,x)
              
              qplot(x,y,data=data)
              
              cmod = glm(formula = y ~ x,data=count,family = poisson(link = log))
              summary(cmod)
              
              # Predicted Probs and Marg. Effects.
              cplot(cmod,x="x",what="effect")
              cplot(cmod,x="x",what="prediction")
              
              
          ###
          ### Ordered Logit/Probit
          ###
              
              head(mtcars) # head data
              
              # The Ordered Logistic/Probit function is in the "MASS" package
              require(MASS)
              
              DF = mtcars %>% dplyr::select(gear,cyl)
              DF$gear = as.numeric(as.factor(DF$gear))-1
              
              # predict the number of gears using the number of cylanders
              omod = polr(factor(gear)~cyl,data=DF,method = "logistic")
              summary(omod)
          
              
              
          ###
          ### Multinomial Logit
          ### 
              
              # Use the "nnet" package
              # install.packages("nnet")
              require(nnet)
              
              head(diamonds)
              # Back to the diamonds data. Let's see if we can't predict cut quality using carat. 
              DF = diamonds %>% dplyr::select(cut,carat)
              
              mmod = multinom(cut~carat,data=DF)
              summary(mmod)
              

              
              
# NEXT TIME: duration/hierarhical/Bayesian models (if time) & out of sample 
# predictions for glms. In addition, we'll go into all things presentation, and 
# we'll scratch the surface of string manipulations.
