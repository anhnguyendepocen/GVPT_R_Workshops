#################################################################
#################################################################
########    GVPT Political Methodology R Workshops ##############
########    Eric Dunford (edunford@umd.edu)        ##############
########    November 18, 2016                      ##############
#################################################################
#################################################################
##############  ****   Workshop 4  **** #########################
######## ****  PRESENTATION and MODELING_2 **** #################
#################################################################
#################################################################


# Last time we covered basic modeling strategies. This week we will quickly wrap
# up a few things on modeling, and then spend some time on presentation. Finally
# we'll wrap up with regular expressions, and other useful tricks to make life 
# easier in R. Hopefully, we'll leave a little time open for questions.


# MODELING 2 ---------------------------------------------------------
    
  # *** ZELIG Framework ***
  # by Gary King and Friends

      # PITCH: easy-to-use, free, open source, general purpose statistics program for
      # estimating, interpreting, and presenting results from any statistical
      # method. Plus side, all modeling requires easy-to-use, free, open source,
      # general purpose statistics program for estimating, interpreting, and
      # presenting results from any statistical method.
  
   
      # See "http://zeligproject.org/" # For documentation and pitch.

      
    install.packages("Zelig")
    require(Zelig)
      
      # Like last time, we will simulate the data generating process for the
      # following varibles.
      


    # CONTINUOUS DV
              set.seed(235) # For replication of random vars
              x <- rnorm(100,10,.4) # random variable
              e <- rnorm(100,0,1) # assumption-following error
              y <-  1 + 2*x + e # y is a linear function of x + error
              D1 = data.frame(y,x)
              
              head(D1)
        
        model = zelig(y ~ x, data = D1, model = "ls")
        summary(model)
      
      
      
      
    # BINARY DV
              set.seed(1988)
              x1 <- rnorm(500,.1,1) 
              x2 <- as.integer(round(runif(500,0,3)))
              z <- .2 + -.8*x1 + .2*x2 # Latent Linear combination
              pr = exp(z)/(exp(z) + 1) # Logit-Link
              y <- rbinom(length(pr),1,pr) # Fit to a binomial distribution
              D2 = data.frame(y,x1,x2)
        head(D2)
        
        zelig(y ~ x1 + x2, data = D2, model = "ls") # LPM
        model2 = zelig(y ~ x1 + x2, data = D2, model = "logit") # ML
        summary(model2)
        
        
        set1 = setx(model2,x2=0:3) # Easily set values of interest
        sim1 = sim(model2,set1) # And simulate confidence intervals
        sim1
        ci.plot(sim1)
        
            # setx() sets the variables not explicitly listed to their mean if
            # numeric, and their median if ordered factors, and their mode if
            # unordered factors, logical values, or character strings.
        
        # Observed Values
        set2 = setx(model2,x1=x1,x2=0:3)
        sim2 = sim(model2,set1,num=1000)
        
        ci.plot(sim2,qi="ev",ci = c(95),ylab = "Pr(Y|X)")
        
        
                # !!!DETOUR!!!
                  # Test: Hanmer/Kalkan Approach (how does it compare?)
                  # install.packages("mvtnorm")
                  require(mvtnorm) 
                  mod = glm(y ~ x1 + x2, data = D2,family=binomial(link="logit"))
                  manipulate <- 0:3
                  sim_coefs <- rmvnorm(1000, mod$coef, vcov(mod)) # Multinormal simulated coefs. 
                  out = sapply(1:length(manipulate),function(j){
                    X <- as.matrix(cbind(1,mod$model[,-1]))
                    X[,3] <- manipulate[j] # position should always correspond with loc in mat
                    predProbs = apply(sim_coefs,1,function(x){
                      p = X %*% x; pr = exp(p)/(exp(p)+1);mean(pr,na.rm=T)
                    })
                    data.frame(var=manipulate[j] , pp=mean(predProbs), 
                               lower=quantile(predProbs,.05,names = F), 
                               upper=quantile(predProbs,.95,names = F))
                  })
                  results = as.data.frame(t(out))
          
                  par(mfrow=c(1,2))
                  plot(x=results$var,y=results$pp,type="both",ylim=c(.45,.75),main="Hanmer/Kalkan")
                  lines(x=results$var,y=results$lower)
                  lines(x=results$var,y=results$upper)
                  ci.plot(sim2,qi="ev",ci = c(95),ylab = "Pr(Y|X)",main="Zelig")
                  
                  # Does fairly well...
                  
    # Count Models
         
            set.seed(1998)
            x <- runif(100,0,5)
            lambda = exp(1 + .3*x)
            y <- rpois(100,lambda)
            count = data.frame(y,x)
                  
      model = zelig(y~x,data=count,model="poisson")
      summary(model)
      zelig(y~x,data=count,model="negbin")
      # Same process thereafter setx() and sim() 
     
       
    # Ordered Probit
          # zelig(y~x,data=data,model="oprobit")
    # Ordered Logit
          # zelig(y~x,data=data,model="ologit")
    # Multinomial Logit
          # zelig(y~x,data=data,model="mlogit")
    # Timeseries
          # zelig(y~x,data=data,model="arima")
    # Even Bayesian Functions ... etc. 
    # see http://docs.zeligproject.org/en/latest/ for more info.
      
    # DOWNSIDE TO ZELIG: the output objects are purposely obscured, which makes 
    # it difficult to extract any functionality out. I imagine this is so you 
    # have to stay and play with their suite of code, but this can be an issue.
    
str(model)

      
  # *** Hierarchical Models ***
      
      install.packages("lme4")
      install.packages("arm")
      require(lme4)
      require(arm)
      
      ?sleepstudy # Reaction time given sleep deprivation
      df = sleepstudy
      
      plot(df$Days,df$Reaction,pch=16)
      
      
      h.model <- lmer(Reaction ~ Days + (1 + Days | Subject), df)

      summary(h.model) # General Summary
      
      # Extracting fixed and random effects
      fixef(h.model)
      se.fixef(h.model)
      
      ranef(h.model)
      se.ranef(h.model)
      
      # Simple Plot of the effects
      ran = ranef(h.model)[[1]]
      fixed = fixef(h.model)
      # Fixed effect reported as deviations
      ran[,1] = (ran[,1] + fixed[1])
      ran[,2] = (ran[,2] + fixed[2])
      plot(df$Days,df$Reaction,pch=16)
      for(i in 1:nrow(ran)){ # random and varying values
        if(ran[i,2] > 0){
          abline(a=ran[i,1],b=ran[i,2],col="orange",lwd=2)
        }else{
          abline(a=ran[i,1],b=ran[i,2],col="blue",lwd=2)
        }
      }
      abline(a=fixed[1],b=fixed[2],col="purple",lwd=6) # Global
      
      
      # For Generalized linear models use the function glmer(); to load data on
      # the second level, switch to bayes.
      
      
# Presentation -------------------------------------------------------
      

  # **** R Markdown, R "NoteBooks", R Presentation **** 
      
      # (1) Open the markdown_basics.rmd file. [Note that a bunch of packages will download
      # on their own.]
      
      # (2) Then let's walk through R Notebooks. The newest feature of RStudio.
      
      # (3) Finally, let's look at how to put together some great slides. Open the
      # slides_basics.rmd
      
      
  # **** LaTex Tables ****
      # For published work we need clean tables and figures. LaTex helps us do 
      # just that. The following set of functions will help show you how one can
      # translate tables and figures into publishable tables. 
      
      # install.packages("stargazer")
      # install.packages("xtable")
      require(stargazer)
      require(xtable)
      
      
      # STARGAZER #
      
      # Both produce output that is translated into latex. 
      stargazer(iris)
      
      # We can craft a lot of the output using the function.
      stargazer(iris,title = "Descriptive Statistics",style = "AJPS",
                covariate.labels = c("Sepal Length",
                                     "Sepal Width",
                                     "Petal Length",
                                     "Petal Width"))
      
      
      # When Presenting Models, we can bring multiple models together with ease.
      
      # Create some fake data
      X <- matrix(data = NA,nrow=100,ncol=5)
      for(i in 1:5){X[,i] <- rnorm(100,1,5)}
      y <- cbind(1,X)%*%c(2,.3,2,3,.5,1) + rnorm(100,0,1)
      data = data.frame(cbind(y,X))
      colnames(data) <- c("y",paste0("x",1:5))
      head(data)
      
      model1 <- lm(y~x1,data=data)
      model2 <- lm(y~x1+x2+x3,data=data)
      model3 <- lm(y~x1+x2+x3+x4+x5,data=data)
      
      stargazer(model1,model2,model3,
                title="OLS Results")
      
      
      # XTABLE #
      
      # Stargazer is great, but it doesn't always play well with all objects. 
      # When you have a weird table/output that you want to latex and stargazer
      # won't behave, use xtable. 
      
      table(mtcars$gear,mtcars$carb)
      stargazer(table(mtcars$gear,mtcars$carb))
      xtable(table(mtcars$gear,mtcars$carb))
      
      # Not as pretty, but it gets the job done!
      
      
  ## ** PUBLISHABLE GRAPHS ***
      
      # cairo_pdf() # vectorize PDF
      # cairo_ps() # Post Script
      
      # Both functions return graphs that can be manipulated in illustrator. 
      # This (a) makes it easy for a publisher to manipulate your graphs, and 
      # (b) it ensures that your graphics will be clear (rather than fuzzy and
      # granular)
      
      cairo_pdf("~/Desktop/test.pdf")
      hist(rnorm(1000,3,4),col="lightblue",border="white",
           breaks=50,xlab="X",main="My Histogram")
      dev.off()
      
      
      
# Dealing with Dirty Data ------------------------------------------------
      
      # The data in political science is rarely "clean"; that is, for most novel
      # and interesting data, it's often riddled with errors and issues that are
      # costly to resolve. In a sense, it's dirty. We can't run statistical 
      # analysis on dirty data. So we have to clean it. The following will be a 
      # very brief introduction into this world. The main goal is to give you a
      # light tool kit for dealing with a number of data issues. 
      
      
  ## *** REGULAR EXPRESSIONS AND STRING MANIPULATIONS ***
      
      countries = c("Canada","Russia","New Zealand","New Guinea")
      
      
      # Locating word in a vector
      grep(pattern = "New",countries) # provides location in a vector
            
            # E.g. 
            countries[1]
            countries[grep(pattern = "New",countries)]
            
      grepl(pattern = "New",countries) # Provides a logical vector
      
            # E.g.
            countries[grepl(pattern = "New",countries)]
      
                  
      gsub(pattern = "New",replacement = "Old",countries) # replace values in a vector
      
      # manipulate case
      tolower("THIS IS IMPORTANT")
      toupper("This is a normal sentence.")
      
      # trim white space
      trimws("\n\n This string has so much white space.  ")
      
      ### Some handy characters to know:
      
          # `+` = match 1 or more of the previous character
          # `*` = match 0 or more of the previous character
          # `?` = the preceding item is optional (i.e., match 0 or 1 of the previous character).
          # `[ ]` = match 1 of the set of things inside the bracket
          # `\\w` = match a "word" character (i.e., letters and numbers). 
          # `\\d` = match digits
          # `\\s` = match a space character
          # `\\t` = match a "tab" character
          # `\\n` = match a "newline" character
          # `^` = the "beginning edge" of a string
          # `$` = the "ending edge" of a string
          # {n} = the preceding character is matched n times
      
      trouble = c("4Texas::","BigSmall","!But! What about Number 5?")
      
      trouble[grep("\\d",trouble)] # draw out digits
      trouble[grep("Tex*",trouble)] # find fuzzy text
      trouble[grep("Bi+",trouble)] # find fuzzy text
      trouble[grep("[::]",trouble)] # find specific punctuation.
      trouble[grep("[!]",trouble)]
      trouble[grep("?Sm",trouble)] # Locate partials.
      trouble[grep("?Sm|[!]",trouble)] # Conditionals work
      trouble[grep("[[:upper:]]",trouble)] # Locate strings with upper case
      
      # Use this combination to, say, clean a string of punctuation
      dirt = "Clean this $%&*_@ string!"
      gsub("[[:punct:]]","",dirt)
      
      
      text = "We're all very sad that Hillary Clinton lost the election."
      
      # Manipulation requires that you tokenize a string (i.e. break it up)
      
      token = strsplit(text,split = " ") # Splitting by Spaces.
      token = token[[1]]
      token[1]
      token[2]
      token[10]
      
      # we can "paste" vectors back together after tokenizing
      
          paste0(token,collapse = " ")
          # Collapse denotes what to piece the string together by
          paste0(token,collapse = "")
          paste0(token,collapse = "<<>>")
          
      # paste() and paste0() are extremely useful string functions. I use them
      # on EVER SCRIPT. 
          
          paste("Fruit","Juice") # Requires a seperator; default is a space
          paste("Fruit","Juice",sep="--")
          
          paste0("Fruit","Juice") # doesn't require a seperator. Is how you present the info.
          paste0("Fruit"," ","Juice")
          
          
      # When we have a dictionary, we can use it to locate actors in a text 
      important.people = c("Clinton","Obama","Putin")
      
      query = paste0("?",important.people[1])
      token[grep(query,token)]
      
      # Say if we new something about how names were presented, we could extract
      # the token before the located one to get the full name.
      loc = grep(query,token)
      loc = c(loc - 1,loc)
      paste0(token[loc],collapse=" ")
     
       
      
    # "STRINGR" is an extremely versatile regex package. 
      # install.packages("stringr")
      require(stringr)
      
      text
      
      str_count(text) # count the number of charaters
      str_count(token) 
      str_detect(token,"Clinton") # detect a word (same as grepl)
      str_dup("Laughable ",5) # repeat a word
      str_extract(token,"\\w+[']\\w+") # Extract Word from String
      str_c(token,collapse = " ") # similar to paste
      str_match(text,"Hillary")
      str_locate(text,"Hill") # Locate where and the length in the text
      str_replace(text,"Hillary Clinton","Donald Trump") # Replace a value in a string
      str_split(text," ") # same as strsplit()
      
      str_to_title("the great gatsby")
      str_to_lower(text)
      str_to_upper(text)
      
      str_trim("\n\n This string has so much white space.  ") # same as trimws()
      
      str_trunc(text,25) # truncate after the first 25 characters.
    
        
  ## *** DATES *** ##
      
      my.date = "2009-Jan-01"
      
      as.Date(my.date)
      
      clean.date = as.Date(my.date,"%Y-%B-%d")
      clean.date
      
      # Date formats have nice properties
      clean.date + 1
      clean.date + 31
      clean.date + 365
      clean.date - as.Date("2008-01-01")
      
      
      # Date formating features to keep in mind
      
      # %d = day as a number 
      # %a = abbrev. weekday
      # %A = Unabbrex. weekday
      # %m = month as number
      # %b = abbrev. month
      # %B = Unabbrev. month
      # %y = 2 digit year
      # %Y = 4 digit year
      
      dates = c("January 7, 2009","10/30/88","7th of May 2000")
      dates
      
      as.Date(dates[1],"%B %d, %Y")
      as.Date(dates[2],"%m/%d/%y")
      as.Date(dates[3],"%dth of %B %Y")
      
      
    # Use what we know to easily manipulate date data
      dates.data = data.frame(month=c(1,3,5,10),
                              day = c(30,4,6,16),
                              year = c(1988,2021,1977,2014))
      dates.data
      
      # Paste dates together
      dates.data$date = paste(dates.data$month,dates.data$day,dates.data$year,sep="-") # paste
      
      # Convert to date
      dates.data$date = as.Date(dates.data$date,"%m-%d-%Y")
      head(dates.data)
      
      # Analyze
      dates.data$days_since_min = as.numeric(dates.data$date - min(dates.data$date))
      dates.data$years_since_min = round(dates.data$days_since_min/365,2)
      head(dates.data)
      
      
      
  ## *** Annoying COUNTRY NAME Issues *** ##
      
      # Real quick: for the IR people in the crowd. Here is a quick and easy 
      # package for dealing with country names.
      
      # install.packages("countrycode")
      require(countrycode)
      
      countries = c("Democratic Republic of Congo","DR Congo","Congo","Zaire")
      
      countries
      countrycode(countries,origin = "country.name",destination = "country.name") # Standardize Country Names
      countrycode(countries,"country.name","cowc") # Cow three letter code
      countrycode(countries,"country.name","cown") # Cow numerical code
      countrycode(countries,"country.name","wb") # World Bank code 
      # etc.
      
          