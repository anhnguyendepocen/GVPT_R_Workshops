##########################################
### GVPT Political Methodology R Workshops
### Eric Dunford (edunford@umd.edu)
### October 7, 2016
##########################################
##########################################
############## BASICS 2 ##################
##########################################
##########################################

# Packages that we will use (make sure to install them if you haven't
# already,e.g. install.packages("package name"))
require(foreign)
require(readstata13)


    # Set the working directory to make life easier.
    getwd()
    setwd("/Users/edunford/Desktop") # This path will be unique given which computer you are on
    
    
    # Load in both datasets:
    fake.data = read.csv("example.csv")
    vote.data = read.dta("stu_vote.dta")
    
    
    # Examine the structure of each data frame
    str(fake.data)
    str(vote.data)
    
    
    # View the Data
    View(fake.data)
    View(vote.data)

    
    
# Examining the data --------------------------------------

      summary(fake.data)
      summary(vote.data)
      
      
      mean(fake.data$predictor)
      round(mean(fake.data$predictor),5) # We can also round a numerical value
      
      
      median(fake.data$predictor)
      
      
      sd(fake.data$predictor)
      var(fake.data$predictor)
      
      
      min(fake.data$predictor)
      max(fake.data$predictor)
      range(fake.data$predictor)
      
      
      quantile(fake.data$predictor)
      fivenum(fake.data$predictor)
      
      
      colMeans(fake.data[,-1]) # drop the id column
      rowMeans(fake.data[,-1]) # a mean for every row
      
      
      colSums(fake.data[,-1])
      rowSums(fake.data[,-1])
      
      
      # Correlation 
      cor(fake.data$predictor,fake.data$outcome) 
      # They're correlated, but are they significantly correlated?
      cor.test(fake.data$predictor,fake.data$outcome) # Yep! Almost by design...
      
      
      # How about a Cross Tab?
      table(vote.data$warsup,vote.data$female)
      ftable(vote.data$warsup,vote.data$female)
      # Two ways to get the same thing --- but frequencies aren't always super
      # informative.
      
      # For good cross-tabs let's use the `descr` package.
      require(descr)
      crosstab(dep = vote.data$warsup,indep = vote.data$female,prop.c = T,plot = F)
      crosstab(dep = vote.data$warsup,indep = vote.data$pid,prop.c = T,plot = T)
      # The visualization can be a useful way to eye ball the relationship

      
      
# Vizualizing the Data --------------------------------------
      
      # Basic Visualize the data
      plot(fake.data$predictor,fake.data$outcome)
      
      # For more Advanced Visualization (feel free to play around with this),
      # you just have to know what is going on underneith the hood.
      
          # pch = point type
          # cex = point size
          # col = point/line color
          # lty = line type
          # lwd = line width
          # xlab = label on the x axis
          # ylab = label on the y axis
          # main = plot title
      
      # For more information on the arguments a function takes, use a ? to
      # access the documentation
      ?plot()
      ?hist()
      
      # Example:
          plot(fake.data$predictor,fake.data$outcome,pch=20,cex=2,
               col="lightblue",xlab="x",ylab="y",main="Plot!")
          
          # Let's Highlight all values between 0 and 5 on the x axis
          points(fake.data$predictor[fake.data$predictor>=0 & fake.data$predictor<=5],
                 fake.data$outcome[fake.data$predictor>=0 & fake.data$predictor<=5],pch=20,cex=2,
                 col="darkred")
          # Some Dark bars just to quarter off that space
          abline(v=0,lwd=5)
          abline(v=5,lwd=5)
          
          # Fit a Regression Line to the Plot
          abline(reg = lm(outcome~predictor,data=fake.data),lty=1,lwd=4, col = "#F39E1B")
          
          # And a lowess curve for kicks
          l = lowess(fake.data$predictor,fake.data$outcome)
          lines(l,lty=2,lwd=3,col="#65B267")
          
          # Lastly, add a legend
          legend(10,-20,legend = c("Regression","Lowess"),
                 lty=c(1,2), 
                 lwd=c(2,2),
                 cex=.7,
                 col=c("#F39E1B","#65B267")) 
      
      # Though it looks like there is a lot going on here, there isn't. We are 
      # just piecing together the graph one part at a time. This offers a lot of
      # flexibility in putting graphics together. 

          
          
# Saving ----------------------------------------------------
          
          
      # SAVING GRAPHICS is straight forward: we just sandwhich the output
      # in-between the pdf() and dev.off() functions
      
              
      cairo_pdf(file="plot.pdf",width =8,height=5) # Start
            # Run Code
            plot(fake.data$predictor,fake.data$outcome,pch=20,cex=2,
                 col="lightblue",xlab="x",ylab="y",main="Plot!")
            points(fake.data$predictor[fake.data$predictor>=0 & fake.data$predictor<=5],
                   fake.data$outcome[fake.data$predictor>=0 & fake.data$predictor<=5],pch=20,cex=2,
                   col="darkred")
            abline(v=0,lwd=5)
            abline(v=5,lwd=5)
            abline(reg = lm(outcome~predictor,data=fake.data),lty=1,lwd=4, col = "#F39E1B")
            # And a lowess curve
            l = lowess(fake.data$predictor,fake.data$outcome)
            lines(l,lty=2,lwd=3,col="#65B267")
            abline(v=0,lwd=5)
            abline(v=5,lwd=5)
            legend(10,-20,legend = c("Regression","Lowess"),
                   lty=c(1,2), 
                   lwd=c(2,2),
                   cex=.7,
                   col=c("#F39E1B","#65B267")) 
            
      dev.off() # End
      

      # SAVING DATA
      write.csv(fake.data,file = "newData.csv")
      write.dta(fake.data,file = "newData.dta")
      save.dta13(fake.data,file = "newData.dta")
      
      save(fake.data,file="newData.Rdata") # save as an Rdata object.
      
      # Heck, let's save the entire environment!
      save.image(file = "/Users/edunford/Desktop/environ.Rdata")
      