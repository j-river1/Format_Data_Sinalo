#The putting Chiapas Data.
#Arguments.    -list of files Chiapas
#Return.       -data with format for using in the scripts weather

mainDir <- getwd()
dir.create(file.path(mainDir, "Data_Format"), showWarnings = FALSE)

#list.files(path="./Data_Chiapas")
#variable <- c("HR", "Pp", "RadG", "Tmax","Tmin")

format_chiapas <- function (listfiles, variables = c("HR", "Pp", "RadG", "Tmax","Tmin"))
{
  
  #List of files
  listFiles <- lapply(listfiles,  choose_file, variables = variables)
  namesEsta <- lapply(listfiles, change_names)
  names(listFiles) <- namesEsta
  
  #Remove Null
  listFiles<- lapply(listFiles, function (x){ if(length(x)==1)
                                                      {x <- NULL}
                                                      return (x) 
                                                                })
  listFiles <- listFiles[!sapply(listFiles, is.null)]
  
  
  #Save Files
  save_files <- lapply(seq_along(listFiles), function(y,n,i){
    name <- n[[i]]
    write.table(y[[i]], file = paste0("./Data_Format/", name ), row.names = FALSE, sep="")
  }, y=listFiles, n=names(listFiles))
  
  
  
  
  return (listFiles)
  
}


#Choose_file works for taking the file with ending _HR, _Pp, _RadG, _Tmax, _Tmin.
choose_file <- function (file, variables=c("HR", "Pp", "RadG", "Tmax", "Tmin"))
{
  #Find the file 
  sliptName <- split_name(file)
  
  #Find the value 
  variableFile <- sliptName[length(sliptName)]
  
  if(variableFile %in% variables)
  {
    file <- read.table(paste0("./Data_Original/", file), header = TRUE)
    
    if(ncol(file)!=2)
    {
      stop("The file ", file, " has no two columns")
    }
    else
    {
      colnames(file) <- c("Date","Value")
      #file$Value <- as.numeric(file$Value )
      #file$Value <- as.numeric(levels(file$Value))[file$Value] 
      file$Value <- as.numeric(as.character(file$Value))
      file$Date <- as.Date(as.character(file$Date), format= "%d/%m/%Y")
      file$Date <- format(file$Date, "%Y-%m-%d")
    }

    }
  
  return(file)  
  
}



#split_name works for splitting file 

split_name <- function(filename)
{
         split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
         split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
         
           return(split_name)
}

#change_names works for changing names of files to standar form.
#Arguments variableOrig. Originales variables 
#          varaiblesScrip: Name of variables for script.
#_HR, _Pp, _RadG, _Tmax, _Tmin

change_names <- function (filename, variablesScrip = c("HR", "Pp", "RadG", "Tmax", "Tmin"))
{
  #Delete blank spaces
  name <- gsub("[[:blank:]]","",filename)
  
  #Delete ocurrence first of character
  name <- sub("_", "", name)
  
  #Delete parenthesis 
   name <- gsub("\\)|\\(", "", name)
  
  #Variable
  variable <- gsub(".*_", "", name)
  variable <- gsub(".txt", "", variable)
  
  
  if((variable %in% variablesScrip) == FALSE)
  {
    return(name)
  }
  
  #Checking which variable belongs to variableScrip
  else
  {
    variableToReplace <- variablesScrip[which(variablesScrip == variable)]
    
    if(variableToReplace == 'HR')
    {
      name <- gsub('HR','RH_NE', name)
    }
    
    else if (variableToReplace == 'Pp')
    {
      name <- gsub('Pp','P_MM', name)
    }
    
    else if (variableToReplace == 'RadG')
    {
      name <- gsub('RadG','SR_WAM2', name)
    }
  
    else if(variableToReplace == 'Tmax')
    {
      name <- gsub('Tmax','TX_CD', name)
    }
    
    else 
    {
      name <- gsub('Tmin','TM_CD', name)
    } 
    
    return (name)
    
  
  }
  
}

