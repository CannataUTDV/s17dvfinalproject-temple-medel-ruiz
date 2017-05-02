#ETL 
#1. Load packages 
require(jsonlite)
require(readr)
require(plyr)
require(RCurl)

#2. Set up credentials for rest server 
redacted = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmZlcm5hbmRvcnVpeiIsImlzcyI6ImFnZW50OmZlcm5hbmRvcnVpejo6NmNjODQ5ODktOTJhYS00OTRjLTk5MWQtODgwMTc4MmI0NDI5IiwiaWF0IjoxNDg0Njk3MzAzLCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.39MCSG-bnjjoaMUsOVErEGUxkk4ONQTDPXp9fn6rnnB6VG0guuOX0Km8StzlLCx1lFxbPERrd18iSpfrFJKROg"
username = 'fernandoruiz'

#3.Access rest server
df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ",'oraclerest.cs.utexas.edu:5024/rest/native/?query="select `Column A`,`Column C`, `Column E`,`Column G`,`Column I`,`Column K`,`Column M`,`Column O`,`Column Q`, `Column S`,`Column U`,`Column W`,`Column Y`,`Column AA`,`Column AC`,`Column AE`,`Column AG`,`Column AI`, `Column AK` from energyByState"')),httpheader=c(DB='jdbc:data:world:sql:pavilionall:s-17-dv-project-6', USER= username, PASS=redacted, MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
print(summary(df))
print(head(df))


#4.Remove special cheracters 
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
  print(df[n])
}


#5. Since data is taken from data.world we need to fix the column name bug by renaming the columns   
colnames(df) <- c("State", "Abbreviation", "Region","Biomass","Geothermal","Solar","Wind","Hydro","HPS","Coal","Petroleum", "Gas","Nuclear" ,"Other", "Non-hydro RE Generation (MWh)", "Total RE Generation (MWh)","Total Generation (MWh)","Non-Hydro Renewable Percent Total Generation", "Renewable Percent Total Generation")

#6. Remove first blank column
df <- df[-1,]

#6. Now we write out our file and add it to our data.world data set:
# https://data.world/jacobbtemple/finalprojectdata

write.csv(file="energyByStateClean.csv", x = df, row.names=FALSE, na = "")
