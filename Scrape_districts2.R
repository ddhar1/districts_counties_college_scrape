library(rvest)

#
# Since I can't find a list of California community colleges (CCC) and counties that
# get funding from them, 
#

# get website to read, read it
ccd_link <- 'http://californiacommunitycolleges.cccco.edu/Districts.aspx'

ccd_read_link <- read_html(ccd_link)

# read districts, counties, schools
# properties of page guaged with Selector Gadget chrome extension
districts_temp <- html_nodes(ccd_read_link,'#tabItem14 span, #tabItem15 span, #tabItem16 span, #tabItem17 span, #tabItem18 span, #tabItem19 span')
districts <- html_text(districts_temp)

counties_temp <- html_nodes(ccd_read_link,'tr+ tr td:nth-child(2)')
counties_temp2 <- html_text(counties_temp)

schools_temp <- html_nodes(ccd_read_link, 'tr+ tr td~ td+ td')
schools_temp2 <- html_text(schools_temp)
d <- html_text( schools_temp )

# make empty lists
schools <- vector( mode = "list", length = length(schools_temp2))
counties <- vector( mode = "list", length = length(counties_temp2) )


# Iterate through scrapped counties and schools, clean the text
for ( i in 1:length(counties) )
{
  # 
  schools_temp9 <- strsplit(schools_temp2[i],"\n            ", fixed = FALSE, perl = FALSE, useBytes = FALSE)
  counties_temp9 <- strsplit(counties_temp2[i],"\n            ", fixed = FALSE, perl = FALSE, useBytes = FALSE)
  
  
  #schools_temp9[[1]][2:length(schools_temp9[[1]])]
  schools_temp9[[1]][length(schools_temp9[[1]])] <- substring( schools_temp9[[1]][length(schools_temp9[[1]])], 1, nchar( schools_temp9[[1]][length(schools_temp9[[1]])]) - 1  )
  counties_temp9[[1]][length(counties_temp9[[1]])] <- substring( counties_temp9[[1]][length(counties_temp9[[1]])], 1, nchar( counties_temp9[[1]][length(counties_temp9[[1]])]) - 1  )
  
  schools[[i]] <- schools_temp9[[1]][2:length(schools_temp9[[1]])]
  counties[[i]]<- counties_temp9[[1]]
  
}


# create a district to school DF
districts_schools_wide <- setNames( data.frame( districts, matrix( ncol = 8, nrow = length(districts))), c("districts", as.character(1:8) ) )

# create a district to county DF
districts_counties_wide <- setNames( data.frame( districts, matrix( ncol = 8, nrow = length(districts))), c("districts", as.character(1:8) ) )


# for each district, put the appropriate/corresponding school and county 
#nto the district/school or district/county, respectively
for ( i in 1:length(districts) )
{
  for ( j in 1:length(schools[[i]] ) )
  {
    districts_schools_wide[i, j+1] <- schools[[i]][[j]]
    
  }
  
  for( k in 1:length(counties[[i]]) )
  {
    districts_counties_wide[i, k+1] <- counties[[i]][[k]]
  }
}

districts_schools_wide[ is.na(districts_schools_wide) ] <- ""
districts_counties_wide[ is.na(districts_counties_wide) ] <- ""

# write dataframes to file
write.csv( unlist(schools), "~/Thesis/CCC/Lookup Info/schools.csv", row.names = F)
write.csv( districts_counties_wide, "~/Thesis/CCC/Lookup Info/districts_counties_wide.csv", row.names = F)
write.csv( districts_schools_wide, "~/Thesis/CCC/Lookup Info/districts_schools_wide.csv", row.names = F)