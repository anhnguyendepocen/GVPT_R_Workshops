R Basics
======================================================== 
author: Eric Dunford 
date: GVPT Political Methodology R Workshops
font-import: http://fonts.googleapis.com/css?family=Garamond
font-family: 'Garamond'
autosize: true</code>

October 7, 2016

<style>

/* Altering the Title Slides */
.reveal body {
  background: #EDE0CF;
}

/* slide titles */
.reveal h3 { 
  font-size: 60px;
  color: steelblue;
}

/* subheader titles */
.reveal h2 { 
  font-size: 100px;
}

/* starter-section */
.starter-section .reveal .state-background {
  background: #A3A5AD;
} 
.starter-section .reveal h3 {
    font-size: 100px;
    color: white;
}

/* Special Slide Type */
.exclaim .reveal .state-background {
  background: #ff8080;
} 
.exclaim .reveal h3 {
  font-size: 60px;
  color: black;
}
.exclaim .reveal p {
  color: white;
}

</style>

Overview
========================================================
Today we'll cover:

- Objects
- Data Structures
- Packages 
- Data Basics (importing/summarizing/graphing)

All code and scripts are imbedded in the slides, which can be accessed via: http://rpubs.com/dunforde/Rbasics

R in a Nut Shell
========================================================
incremental: true
`R` is a statistical and graphical programming language that is based off a much older language called `S`. It's source code is written in C, Fortran, and R. And it's completely **free** under a [GNU General Public License](https://en.wikipedia.org/wiki/GNU_General_Public_License0).

### What this means for us:
- **No Barriers to Entry**: easy to acquire, easy to contribute
- **Active Community**: if you can think it, there is likely a package out there that does it.
- **Powerful and Adaptive**: build an estimator from scratch, scrape a web-site, automate the coding of a dataset. All is within one's reach. 

Objects
=======================================================
type: sub-section

Objects
=======================================================
incremental:true
R uses a specific set of rules to goven how it looks up values in the environment. 

We manage data by assigning it a name, and referencing that name when we need to use the information again. 

Officially, this is called `lexical scoping`, which comes from the computer science term "[lexing](https://en.wikipedia.org/wiki/Lexical_analysis)". Lexing is the process by which text represents meaningful pieces of information that the programming langauge understands.

Assigning an Object
=======================================================
In simple terms, an `object` is a bit of text that represents a specific value.

```r
x <- 3
x
```

```
[1] 3
```

Here we've assigned the value `3` to the letter `x`. Whenever we type `x`, `R` understands that we really mean `3`. 

Assigning an Object
=======================================================
There are three standard assignment operators:
- `<-`
- `=`
- `assign()`

"Best practice" is to use the `<-` assignment operator.

```r
x1 <- 3 
x2 = 3
assign("x3",3)
cat(x1, x2, x3)
```

```
3 3 3
```

Assigning an Object
=======================================================
Note that lexical scoping is flexible. Objects can be written and re-written when necessary.

```r
object <- 5
object
```

```
[1] 5
```

```r
object <- "A Very Vibrant Shade of Purple"
object
```

```
[1] "A Very Vibrant Shade of Purple"
```

### Down the road it will help to give objects meaningful names!

Objects
=======================================================
One can see all the objects in the environment by either looking at the user interface in RStudio, or typing `ls()`.

```r
ls()
```

```
[1] "object" "x"      "x1"     "x2"     "x3"    
```


Object Classes
=======================================================
Once assigned, an object has a `class`. 

```r
class(x)
```

```
[1] "numeric"
```

The object `x` is of class `numeric`, i.e. a number.

A `class` describes the **properties of the values** assigned to an object. 

Object Classes
=======================================================
There are [many classes](https://www.tutorialspoint.com/r/r_data_types.htm) that an object can take. 

```r
obj1 <- "This is a sentence"
obj2 <- TRUE
obj3 <- factor("This is a sentence")
c(class(obj1),class(obj2),class(obj3))
```

```
[1] "character" "logical"   "factor"   
```
Understanding what class of object one is dealing with is imperative! --- as it will determine what kind of manipulations one can do or what functions an object will work with.

Object Coersion
=======================================================
When need be, an object can be **coerced** to be a different class. 

```r
x
```

```
[1] 3
```

```r
as.character(x)
```

```
[1] "3"
```
Here we transformed `x` -- which was an object containing the value `3` --  into a character. `x` is now a string with the text "3".

Data Structures
=======================================================
type: sub-section

Data Structures
=======================================================
There are also many ways data can be **organized** in `R`.

The same object can be organized in different ways depending on the needs to the user. Some commonly used data structures include:

- `vector`
- `matrix`
- `data.frame`
- `list`
- `array`

Data Structures: Vector
=======================================================

```r
X <- c(1, 2.3, 4, 5, 6.78, 6:10)
X
```

```
 [1]  1.00  2.30  4.00  5.00  6.78  6.00  7.00  8.00  9.00 10.00
```

```r
class(X)
```

```
[1] "numeric"
```

```r
length(X)
```

```
[1] 10
```

Data Structures: Data Frame
=======================================================

```r
as.data.frame(X)
```

```
       X
1   1.00
2   2.30
3   4.00
4   5.00
5   6.78
6   6.00
7   7.00
8   8.00
9   9.00
10 10.00
```

Data Structures: Matrix
=======================================================

```r
as.matrix(X)
```

```
       [,1]
 [1,]  1.00
 [2,]  2.30
 [3,]  4.00
 [4,]  5.00
 [5,]  6.78
 [6,]  6.00
 [7,]  7.00
 [8,]  8.00
 [9,]  9.00
[10,] 10.00
```

Data Structures: List
=======================================================

```r
as.list(X)
```

```
[[1]]
[1] 1

[[2]]
[1] 2.3

[[3]]
[1] 4

[[4]]
[1] 5

[[5]]
[1] 6.78

[[6]]
[1] 6

[[7]]
[1] 7

[[8]]
[1] 8

[[9]]
[1] 9

[[10]]
[1] 10
```

Data Structures: Accessing Data
=======================================================
One must understand the **structure of an object** in order to systematically access the material contained within it.

```r
data <- women # Let's reference an R dataset
class(data)
```

```
[1] "data.frame"
```

```r
str(data)
```

```
'data.frame':	15 obs. of  2 variables:
 $ height: num  58 59 60 61 62 63 64 65 66 67 ...
 $ weight: num  115 117 120 123 126 129 132 135 139 142 ...
```

Data Structures: Accessing Data
=======================================================

```r
data[,2] # Access the 2nd column
```

```
 [1] 115 117 120 123 126 129 132 135 139 142 146 150 154 159 164
```

```r
data[1,] # Access the 1st row
```

```
  height weight
1     58    115
```

```r
data$weight # calling a specific column
```

```
 [1] 115 117 120 123 126 129 132 135 139 142 146 150 154 159 164
```

Data Structures: Accessing Data
=======================================================
Most data objects can be accessed using `$` call sign.

```r
str(data)
```

```
'data.frame':	15 obs. of  2 variables:
 $ height: num  58 59 60 61 62 63 64 65 66 67 ...
 $ weight: num  115 117 120 123 126 129 132 135 139 142 ...
```

```r
data$height
```

```
 [1] 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72
```

Data Structures: Accessing Data
=======================================================
Understanding the dimensions of an data structure is important too (especially when writing loops or functions)

```r
dim(data) # Dimensions 
```

```
[1] 15  2
```

```r
nrow(data) # Number of Rows
```

```
[1] 15
```

```r
ncol(data) # Number of Columns
```

```
[1] 2
```

Data Structures: Accessing Data
=======================================================
There are also some useful functions built into R to view portions of a data structure. 

```r
head(data,3)
```

```
  height weight
1     58    115
2     59    117
3     60    120
```

```r
tail(data,3)
```

```
   height weight
13     70    154
14     71    159
15     72    164
```

Applied Example 1
========================================================
type:prompt
Understanding objects and data structures in R. Please download the following [script](https://www.dropbox.com/s/bh2yu2ryjoj2qg3/basics1.R?dl=0) for an applied look at operators, classes, and data structures.

Packages
========================================================
type: sub-section

R Packages 
========================================================
incremental: true
There are a number of `packages` that are supplied with the R distribution. These are known as ``[base packages](https://stat.ethz.ch/R-manual/R-devel/library/base/html/00Index.html)" and they are in the background the second one starts a session in R.

A `package` is a set of functions and programs that perform specific tasks. By installing packages, we introduce new forms of functionality to the R environment. 

R Packages 
========================================================
To use the content in a package, one first needs to **install it**. One can do this by utilizing the following function: `install.packages()`. By inserting the name of a specific package, we can connect to an R "mirror" and download the binary of the package.

```r
install.packages("ggplot2")
```

R Packages 
========================================================
Once installed, it's on the system for good. You can then reference or load the package any time you wish to use a function from it.

```r
library(ggplot2)

# or 

require(ggplot2)
```
`library()` and `require()` load and attach packages to the current R session. 

R Packages 
========================================================
Sometimes one has _a lot_ of packages running simultaneously. No problem: we can see what packages are up and running with the following command. 

```r
sessionInfo()
```

It's easy to forget which packages one has loaded when running an analysis. A "**best practice**" is to load all relevant packages at the top of the script to recall all the packages one's analysis is dependent on.

```r
# Here are the packages I'll be using
require(ggplot2)
require(NLP)
require(tm)

# Now onto my analysis...
```

Remember to Load Your Package!
========================================================
type:exclaim

If you ever try to run a function and you get the following prompt...

`Error: could not find function "qplot"`

It's likely you forgot to **load the package**. 

```r
require(ggplot2) # First Load the package
qplot() # Then run the function
# Wah-la!
```

Data in, Analysis out
========================================================
type: sub-section

Importing and Exporting Data
========================================================
`R` allows you to import a large variety of datasets into the environment. However, `R`'s base packages only support a few data types. No Fear: there is usually always an external package that can do the job!


```r
install.packages("foreign") # For .spss, .sas, .dta (STATA 12 or lower)

install.packages("readstata13") # For loading in .dta's from STATA 13 or higher

install.packages("XLConnect") # For excel spreadsheets.

# Among others!
```

But where is my data exactly?
========================================================
`R` doesn't intuitively know where your data is. If the data is in a special folder entitled "super secret research", we have to tell `R` how to get there. 

We can do this two ways:

  1. Set the **working directory** to that folder
  2. Establish the **path** to that folder
  
Setting the Working Directory
========================================================
Every time `R` boots up, it does so in the same place, unless we tell it to go somewhere else. 

```r
getwd() # Get the current working directory
```
`/Users/edunford/`

Setting a new working director

```r
setwd("/Users/edunford/Desktop/super secret research")
getwd()
```
`/Users/edunford/Desktop/super secret research/`

Establishing the Path
========================================================
We can also just point directly to the data by outlining the specific path.

```r
path.to.data <- "/Users/edunford/Desktop/super secret research/data.csv"
read.csv(path.to.data)
```

Either way -- works and it inevitably becomes a thing of preference. 

Importing data
========================================================
Here we will review how to import three seperate data types: `.csv`,`.dta`, and `.xlsx`.
### .dta
For STATA 12 <= 

```r
require(foreign)
data <- read.dta(file = "data.dta")
```
For STATA 13 >=

```r
require(readstata13)
data <- read.dta13(file = "data.dta")
```

Importing data
========================================================
### .csv

```r
data = read.csv(file = "data.csv",
                stringsAsFactors = F)
# Or

data = read.table(file = "data.csv",
                  header = T, 
                  sep=",",
                  stringsAsFactors = F)
```
These functions have specific arguments that we are referencing: `stringsAsFactors` means that we don't want all character vectors in the data.frame to be converted to Factors. `header` means the first row of the data are column names. `sep` means that entries are seperated by commas.

Importing data
========================================================
### .xlsx

```r
require(XLConnect)

# load all the spreadsheets
wb <- loadWorkbook(filename = "data.xlsx") 

# Grab the spreadsheet w/ the data
data <- readWorksheet(object = wb,
                      sheet="Sheet 1") 
```

Exporting data
========================================================
Exporing data is the same process in reverse. Assuming the we have the `foreign`, `readstata13`, and `XLConnect` packages loaded:

```r
# Stata
write.dta(data,file="data.dta") # <= 12
save.dta13(data,file="data.dta") # >= 13

# .csv
write.csv(data,file="data.csv",row.names = F)
write.table(data,file="data.csv",sep = ",")

# Excel Workbook
writeWorksheet(wb,data,
               sheet="sheet 1",
               startRow = 1,
               startCol = 1,header=T)
```

Descriptive Statistics
========================================================
Now that we can get data into R, we want to explore and summarize what's going on. 

`summary()` allows for one to quickly summarize the distributions across a set of variables

```r
summary(data)
```

```
     height         weight     
 Min.   :58.0   Min.   :115.0  
 1st Qu.:61.5   1st Qu.:124.5  
 Median :65.0   Median :135.0  
 Mean   :65.0   Mean   :136.7  
 3rd Qu.:68.5   3rd Qu.:148.0  
 Max.   :72.0   Max.   :164.0  
```

Descriptive Statistics
========================================================
There are a wealth of useful summary operators that are built into `R`.

```r
mean()
sd()
var()
range()
min()
max()
median()
quantile()
fivenum()
colMeans()
rowMeans()
table()
```

...to name a few!

Base Graphics
========================================================
A rather flexible graphing language comes built into `R`. Though there are more powerful and easy to use graphical packages out there (e.g. `ggplot2` and `lattice`), the base plotting functions offer a lot of functionality. The benefit of these functions is that they are easy to manipulate and use. 
- histograms: `hist()`
- scatter plots: `plot()`
- barplot: `barplot()`
- pie chart: `pie()`
- density plot: `plot(density())`

And more!

Base Graphics: Histogram
========================================================

```r
hist(iris$Sepal.Length)
```

![plot of chunk unnamed-chunk-33](basics_pres-figure/unnamed-chunk-33-1.png)

Base Graphics: Histogram
========================================================

```r
hist(iris$Sepal.Length,breaks=25,
     col="steelblue",border="white",
     ylab = "frequency",
     xlab = "Sepal Length",
     main = "Cool Histogram")
abline(v=mean(iris$Sepal.Length),
       lty=2,col="red",lwd=5)
```

![plot of chunk unnamed-chunk-34](basics_pres-figure/unnamed-chunk-34-1.png)

Base Graphics: Scatter Plots
========================================================

```r
plot(iris$Sepal.Length,iris$Petal.Length)
```

![plot of chunk unnamed-chunk-35](basics_pres-figure/unnamed-chunk-35-1.png)

Base Graphics: Scatter Plots
========================================================

```r
plot(iris$Sepal.Length,iris$Petal.Length,
     pch=20,cex=4,col="#E58526",
     xlab="Sepal Length",
     ylab="Petal Length")
l = lowess(iris$Sepal.Length,iris$Petal.Length)
lines(l,col="#E52643",lwd=7)
```

![plot of chunk unnamed-chunk-36](basics_pres-figure/unnamed-chunk-36-1.png)

Base Graphics: Density Plots
========================================================

```r
dens = density(iris$Petal.Length)
plot(dens)
```

![plot of chunk unnamed-chunk-37](basics_pres-figure/unnamed-chunk-37-1.png)

Base Graphics: Density Plots
========================================================


```r
x1 <- min(which(dens$x >= 1.6001))  
x2 <- max(which(dens$x <  6.2999))
plot(dens,lwd=4,main="Density",ylab="",
     xlab="Petal Length")
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), 
                   y= c(0, y[x1:x2], 0), col="#FFC300"))
```

![plot of chunk unnamed-chunk-38](basics_pres-figure/unnamed-chunk-38-1.png)

Applied Example 2
========================================================
type: prompt

Importing, exploring, and visualizing data is the bread and butter of any statistical programing language. Download the [script](https://www.dropbox.com/s/9rvisqtwj2e5g71/basics2.R?dl=0) and data for an applied example.

Data:
- [data.csv](https://www.dropbox.com/s/j9w0zphoc9f3vne/example.csv?dl=0)
- [data.dta](https://www.dropbox.com/s/z1cv4abkxivs8l4/stu_vote.dta?dl=0)

Further References
========================================================
There are some great resources out there to help you climb the `R` learning curve.

- CodeSchool's [tryr](http://tryr.codeschool.com/) -- an interactive game to learn the basics of `R`.
- [swirl](http://swirlstats.com/) -- learn `R` from the consol. `Swirl` provides an interactive experience to learning `R` from _within_ `R`.
- Some Suggested Readings:
 + [The Art of R Programming](https://www.amazon.com/Art-Programming-Statistical-Software-Design/dp/1593273843/ref=sr_1_1?ie=UTF8&qid=1431630271&sr=8-1&keywords=the+art+of+r+program)
 + [stackoverflow.com](http://stackoverflow.com/)
 + Nearest [Google Browser](https://www.google.com/)

Next Time: Data Management
========================================================
Next week -- **October 14, 2016** -- we will cover the following:

- Loops and Functions
- Transformations, Lags, Decays
- Expansions/Contractions/Subgroup Manipulations
- Advanced Plots (ggplot)

Please provide feedback and comments: https://www.surveymonkey.com/r/TWVFH8W


