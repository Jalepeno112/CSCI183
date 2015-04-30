#Recreates the plot from: 
#http://fivethirtyeight.com/datalab/there-are-few-libertarians-but-many-americans-have-libertarian-views/#fn-1
#
#Uses the 2014 survery data instead of 2010-2012


require("ggplot2")
require("foreign")
require("vcd")


#read data
gss2014Data <- read.spss("GSS2014.sav", to.data.frame=TRUE)

marriageVsInequality = data.frame(marriage = gss2014Data$marhomo, income = gss2014Data$eqwlth) #,id=gss2014Data$id)

#change the levels of the marriage data to be FAVOR, DO NOT DISAGREE OR AGREE, and OPPOSE
mVi <- marriageVsInequality
levels(mVi$marriage)[1:2] <- "FAVOR"
levels(mVi$marriage)[3:4] <- "OPPOSE"
levels(mVi$marriage)[2] <-"NEUTRAL"

#do something similar for the income
mVi$income <- cut(mVi$income, br = c(0,3,4,Inf), labels=c("OPPOSE","NEUTRAL","FAVOR"))

table_mVi <- table(mVi)

m <- mosaic(table_mVi, shade=TRUE, 
            legend=TRUE, pop=FALSE)
labels <- round(prop.table(table_mVi),3)
labeling_cells(text=labels,margin=0)(table_mVi)

