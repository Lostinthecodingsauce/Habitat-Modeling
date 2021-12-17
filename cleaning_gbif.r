###Cleaning RawGBIF Data####


#Package requirements
#tidyverse
#CoordinateCleaner
#saxize
#spatstat
#raster

clean_gbif <- function(df, filter = NULL, resolveTaxonomy = FALSE, resolveID = NULL){
  
  #This function requires the raw dataset downloaded from GBIF. Ill explain each step 1 by 1
  #df = raw GBIF dataset
  #filter = a genus filter to only clean and export for a single genus
  #resolveTaxonomy is a boolean for if you want to resolve via the TAXIZE package
  #resolveID is if you want to resolve the taxonomy via a particular database (view taxize help documentation)
  
  require(tidyverse)
  require(CoordinateCleaner)
  require(taxize)
  
  #Subset only significant columns
  df <- card_gbif_readr[,c("gbifID","basisOfRecord","decimalLatitude","decimalLongitude",
              "coordinateUncertaintyInMeters","coordinatePrecision",
              "scientificName",
              "class","order","family","genus","species","month","year")]
  
  #Optional filter by Genus. Null by default
  if(!is.null(filter)){
    
    df <- filter(df, df$genus == filter)
    
    print(nrow(df))
  }
  
  #clean blank species
  df <- df %>%
    filter(!is.na(df$species))
  df <- df%>%
    filter(df$species!= ",")
  df <- df%>%
    filter(df$species != "")
  df <- df %>%
    filter(df$species != " ")
  
  #remove blank coordinates
  df <- df %>%
    filter(.$decimalLatitude != ",")%>%
    filter(.$decimalLongitude != ",")
  
  #remove records such as fossils
  df <- filter(df, df$basisOfRecord == "HUMAN_OBSERVATION" |
                 df$basisOfRecord == 'OBSERVATION' |
                 df$basisOfRecord == 'PRESERVED_SPECIMEN')
  
  #remove records with high uncertainty (you can tweak values as you see fit)
  
  df <- filter(df, is.numeric(df$coordinateUncertaintyInMeters) <= 50000)
  
  #renames columns for the CoordinateCleaner package
  names(df)[3:4] <- c('decimallatitude', 'decimallongitude')
  
  #clean using CoordinateCleaner
  df <- df %>%
    cc_val() %>% #removes non-numeric/nonvalide lat_long and 
    cc_equ() %>% #removes equal coordinates (often wrong/data entry error)
    cc_cap() %>% #removes capitals as they are likely incorrect
    cc_cen() %>% #removes points at centers of countries/regions
    cc_gbif() %>% #removes coordinates at GBIF hq
    cc_inst()  #removes coordinates at zoos,museums, collections etc.
    
    ######
df <- df %>%
    cc_sea() #removes coordinates over the ocean (need internet connection)
  
df <- df %>%
    cc_zero()%>% #removes coordinates at 0 lat or long with a small radius. 
    cc_dupl() #removes duplicates. This is usually the largest filter so make sure you know if you want it or not
  
  #df <- df%>%
     # filter(month %in% (3:9)) ## Filer by breeding range 
# Remove any saltators 
df <- df %>%
  filter(genus !="Saltator")
  
  #create taxon list
  taxon <- as.character(df$species)%>%
    unique()
  
  #There is the option to use Taxize package to resolve taxonomy. 
  #You can supply the Datasource ID so it only checks your desired database
  #It wont remove species, but adds columns of possible nomenclature changes 
  if(resolveTaxonomy == TRUE){
    print("Now Resolving Taxonomy")
    #using taxize to clean the scientific names and merge
    new_tax <- taxize::gnr_resolve(taxon, best_match_only = TRUE, 
                                   canonical = TRUE, http = 'post', data_source_ids = as.numeric(names))
    
    df <-merge(df,new_tax, by.x = 'species', by.y = 'user_supplied_name')
  }
  
  #The function will return a cleaned data frame
  return(df)
}

###CLeaning Data with KDE####


KDE_filter <- function(points, unispecies = FALSE, low_r = .25, up_r = .75){
  
  ###Clean using Kernel Density Estimates according to gomez 2018####
  
  #points = df as organized below
  #unispecies = boolean for if you are doing 1 species or multiple
  #low_r = the lower bound of outlier exclusion (default .25)
  #up_r = the upper bound of outlier exclusion (default .75)
  
  #For KDE datasets you need to have the supplied df (points) in a specific format
  #3 columns labeled species, decimallongitude, decimallatitude (case matters)
  #the code below can be fed directly from the ouput of the Clean_GBIF function 
  #where occ_clean_sub is the dataframe
  #======================
  #df_kde <- data.frame(species = df$species, 
                  #     decimallongitude = df$decimallongitude,
                 #      decimallatitude = df$decimallatitude)
  
  #=================
  
  require(spatstat)
  require(tidyverse)
  require(raster)
  
  #have the option to do this for a single species or multiple ones
  if(unispecies == TRUE){
    #points_clean_kde = data.frame(x,y)
    
    #This set of win functions makes the "window" for where the KDE will occur based on the species
    
    win <- extent(matrix(c(occ_clean_sub$decimallongitude,occ_clean_sub$decimallatitude), nrow = nrow(occ_clean_sub)))
    win <- data.frame(c(win@xmin, win@xmax),
                      c(win@ymin, win@ymax))
    
    win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))
    
    #create ppm
    gbif_ppp <- ppp(occ_clean_sub$decimallongitude,occ_clean_sub$decimallatitude, window = win)
    
    #KDE
    gbif_kde <- density.ppp(gbif_ppp, at = "points")
    gbif_kde <- data.frame(gbif_kde, gbif_ppp$x,gbif_ppp$y)
    
    #Exclude Outliers
    q <- quantile(gbif_kde$gbif_kde, probs = c(low_r,up_r))
    iqr <- IQR(gbif_kde$gbif_kde)
    uq <- q[2] + (iqr * 1.5)
    lq <- q[1] - (iqr * 1.5)
    
    gbif_kde <- gbif_kde[gbif_kde$gbif_kde <= uq,]
    gbif_kde <- gbif_kde[gbif_kde$gbif_kde >= lq,]
    
  }else{
    #Code for multi species KDE
    #setup the dataframe we will iterate over
    
    points_clean_kde = data.frame("species" = as.character(),
                                  'x' = as.numeric(),
                                  'y' = as.numeric(),
                                  "KDE_status" = as.character())
    
    #starts the for loop. In essence going over each species
    for (i in seq_along(levels(factor(points[,1])))){
      print(i)
      
      points_ <- subset(points, points[,1] == levels(factor(points[,1]))[i])
      
      #removes a species from analysis if they have fewer than 5 records. 
      #Their info is still kept in the output however, they will have a status column (KDE_status)
      if(nrow(points_)<=5){
        
        print(paste("Removing ", points_[1,1], " due to fewer than 5 records"))
        points_merge <- data.frame('species' = points_[1,1],
                                   'x' = points_$decimallongitude, 'y'= points_$decimallatitude,
                                   "KDE_status" = "Removed_tooFewRecords")
        
        points_clean_kde <- rbind(points_clean_kde,points_merge)
        next
      }else{
        
        
        #create the "Window" (extent of occurences) for which the KDE will operate over
        win <- extent(matrix(c(points$decimallongitude,points$decimallatitude), nrow = nrow(points)))
        win <- data.frame(c(win@xmin, win@xmax),
                          c(win@ymin, win@ymax))
        win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))
        
        #creates point pattern dataset in 2 dimensions. i.e. the data to create a "heat map". 
        gbif_ppp <- ppp(points$decimallongitude,points$decimallatitude, window = win)
        
        #KDE - Where the estimation actually takes place
        gbif_kde <- density.ppp(gbif_ppp, at = "points")
        gbif_kde <- data.frame(gbif_kde, gbif_ppp$x,gbif_ppp$y)
        
        #Exclude Outliers based on user supplied lower bound (low_r) and upper bounds (up_r)
        q <- quantile(gbif_kde$gbif_kde, probs = c(low_r,up_r))
        iqr <- IQR(gbif_kde$gbif_kde)
        uq <- q[2] + (iqr * 1.5)
        lq <- q[1] - (iqr * 1.5)
        
        gbif_kde <- gbif_kde[gbif_kde$gbif_kde <= uq,]
        gbif_kde <- gbif_kde[gbif_kde$gbif_kde >= lq,]
        
        
        #if the KDE removes all points it is noted here. 
        if(nrow(gbif_kde)<=1){
          
          print(paste(points_[1,1], " removed following filtering "))
          points_merge <- data.frame('species' = points_[1,1],
                                     'x' = points_$decimallongitude, 'y'= points_$decimallatitude,
                                     "KDE_status" = "Removed_KDE_RemovedAll")
          points_clean_kde <- rbind(points_clean_kde,points_merge)
          next
        }else{
          #Here is a Successful KDE filter
          print(paste('Adding ',points_[1,1]))
          
          points_merge <- data.frame('species' = points_[1,1],
                                     'x' = gbif_kde$gbif_ppp.x, 'y'= gbif_kde$gbif_ppp.y,
                                     "KDE_status" = "Filter_Successful")
          points_clean_kde <- rbind(points_clean_kde,points_merge)
          
        }
      }
    }
  }
  #returns the cleaned dataset.
  # you can easily filter the successful output via:
  # df_alpha <- occ_KDE[occ_KDE$KDE_status == "Filter_Successful",1:3]
  # where occ_KDE is the output of this function
  return(points_clean_kde)
}
