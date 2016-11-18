##########################################
### GVPT Political Methodology R Workshops
### Eric Dunford (edunford@umd.edu)
### October 7, 2016
##########################################
##########################################
############## BASICS 1 ##################
##########################################
##########################################

# OPERATORS ---------------------------------------------------------------

# First, to complement our discussion on objects in R, let's briefly review
# common operators.

x <- 3
y <- 5

# Mathematical Operators

        x + y  # addition
        x - y  # subtraction
        x * y  # multiplication
        x / y  # division
        x^y    # exponentiation
        x %*% y  # matrix multiplication
        
        log(x)
        exp(x)
        

# Boolean ("logical") Operators
        
        x == y      # equals to
        x != y      # does not equal
        x >= y      # greater than or equal to
        x <= y      # less than or equal to
        x > y       # greater than
        x < y       # less than
        x==1 & y==5 # "and" conditional statements
        x==1 | y==5 # "or" conditional statements

                
# Remember, an object stores a specific piece of information, which we can
# manipulate down the line.
        
        x = 100
        z <- x + 7
        w = z * 8
        p = w/1000
        p
        
# To recall all the objects we've created, use the `ls()`
        
        ls() 

# ACCESSING specific data ----------------------------------------------------------------
        
  # Now, let's use what we know about objects, mathematical operators, and boolean
  # operators to meaningfully manipulate data.
          
  # (1) Locating specific instances in a vector. 
        
        vec <- c("a","b","c","d")
        num = 1:4
        
        data.frame(vec,num)
        
        # NOTE: c() stands for "concatenate" in R, which just means "link all
        # these objects together"
        
        class(vec)
        str(vec)
        
        vec_fac = as.factor(vec)
        class(vec_fac)
        str(vec_fac)
        as.numeric(vec_fac)
        as.character(vec_fac)
        
        # Accessing the structure
        vec
        vec[1]
        vec[3]
        vec[5]
        
      
        
        # Boolean operators give us the location of a specific instance in a 
        # vector. for example, we know that "a" is True for the first element in
        # the vector. We can leverage this information to draw out that specific
        # element.
        
        vec == "a"
        "a" %in% vec
        vec %in% "a"
        
        vec[1]
        vec[vec == "a"]
        
        vec[vec == "b"]
        
        vec[vec == "z"] 
        # Nothing there, because
        vec == "z" 
        # "z" doesn't exist in the vector.
        
        # Can also use or/and boolean statements
        # or = |
        # and = &
        
        vec == "a" | vec == "d"
        vec == "a" & vec == "d"
        
        vec[vec == "a" | vec == "d"]
        
    # (2) Locating a specific instance in a data frame.
        
        # Let's use an "off the shelf" data.frame that is build into are for
        # example purposes.
        
        dim(iris)
        str(iris)
        class(iris)
        
        View(iris) # Command to view the data.frame up close and personal
        
        # Or we can just look at bits and pieces of the data
        head(iris)
        tail(iris)
        
        # recall that we can access the specific structure of a data object 
        # either by referencing a specific variable, or numerically pointing to
        # a specific cell in the data.frame
        
        str(iris)
        
        iris[3,]
        iris[3,3]
        iris$Petal.Length
        iris$Petal.Length[3] 
        
        
        # We can also exclude data via negative values.
        head(iris)
        head(iris[,c(-1,-5)])
        
        
        # We can use specific numeric vectors to draw out specific positions in
        # the data.frame
        vec2 = c(1,2,10,40,13)
        vec2
        iris[vec2,]
        
        # Likewise we can subset with an ordered vector
        1:7
        iris[1:7,]
        
        # Or a column name
        iris[,"Species"]
        iris$Species
        
        iris[,("Sepal.Length","Species")]
        
        
        # Let's leverage what we know about boolean operators to subset the
        # data. Let's try to subset the data so that we only have information
        # regarding the "setosa" species. How would we go about doing that?
        iris$Species == "setosa"
        
        iris[iris$Species == "setosa",] 
        # Here I reference all ROWs where Species == "setosa"
        
        # We could also call out specific columns as we are doing this.
        iris[iris$Species == "setosa",2:3] 
        
        # Another subset: I only care about plants with long sepals.
        iris$Sepal.Length >= 7.1
        iris[iris$Sepal.Length >= 7.1,]
        
        # In addition, more complex boolean statements work
        iris[(iris$Species == "setosa" | iris$Species == "versicolor") & iris$Sepal.Length>=5,] 
        # above we wanted setosas or veriscolors with a Sepal Length greater than or equal to 5
        
        
        # Lastly, we can assign a subset to a new object
        data2 = iris[iris$Species == "setosa",] 
        
        
    # (3) using the information contained within a data.frame -- here we'll 
    # lightly touch on creating a variable, which we will go into at greater
    # length next week.
        
        str(data2)
        
        data2$Sepal.Length + 5
        
        # Create a new variable?
        data2$new.variable <- data2$Sepal.Length + 5
        
        data2$ones <- 1
        
        head(data2)
        
        # Drop that variable.
        data2$new.variable <- NULL
        
        head(data2)
        
        data2[1,] <- "e"
      

# EXPLORING Different Data Structures -------------------------------------------
        
        M = as.matrix(data2) # coerce data2 to a matrix
        L = as.list(data2) # coerce data2 to a list
        
        # Matrix
          class(M)
          head(M)
          str(M)
          # Why doesn't the dollar sign work?
          
          M[1,1]
          class(M[1,1])
          # Why did all the numbers become characters?
          
          # Matrices can only hold one class: when the entire matrix needs to be
          # converted into a specific class, it will do so in a way that retains
          # the most information. 
        
          
        # List
          class(L)
          head(L)
          str(L)
          L$Sepal.Length
          
          # How is a list different than a data frame -- they look similar to me?
          
          L$New.Variable <- 1:1000 
          str(L) # Non-relational
          
          data2$New.Variable <- 1:1000
          str(data2) # Relational
        
        