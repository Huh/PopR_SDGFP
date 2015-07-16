		#  Survival file conversion
		#  Josh Nowak
		#  07/2015
#################################################################################
		#  Required packages
		require(tidyr)
		require(dplyr)
		#  all females
#################################################################################
		read_convert <- function(fname, sp, aclass){
			#  Takes a file name as input, file must be .csv,
			#  Also takes sp which is species with entry Mule Deer or 
			#   White-tailed Deer exactly
			#  The argument aclass is the ageclass of the animals with 
			#   yoy, juv and adult relating to the ageclass at capture ... 
			#   animals transition to the next ageclass on October 1 of each year
			#   this means that a yoy cannot be captured between October and 
			#   May/June
			#  Returns a long format summary of survival for each individual that
			#  matches database formatting
			
			#  Add .csv to filename if needed
			if(!grepl("csv", fname)){
				fname <- paste(fname, "csv", sep = ".")
			}
			
			#  Read file
			x <- read.csv(file.path(getwd(), fname), as.is = T)

			#  Convert wide to long format
			out <- x %>%
					gather(date, state, -Animal_ID, -Freq, -Unit, 
							-CaptureDate, -COD) %>%
					mutate(Date = gsub("X", "", date),
							Date = format(as.Date(Date, "%m.%d.%Y"), "%m/%d/%Y"),
							Time = NA,
							Species = sp,
							Id = Animal_ID,
							Frequency = Freq,
							Status = state,
							Fate = NA,
							Capture_Unit = Unit,
							Monitoring_Unit = NA,
							Fate_Unit = NA,
							Ageclass = aclass,
							Age = NA,
							Capture_Type = NA,
							Censored = NA,
							Dead = NA,
							Sex = "f",
							Weight = NA,
							HoofGrowth = NA,
							DeathEvidence = NA,
							Cause_of_Death = COD,
							Observer = NA,
							Region = NA,
							DAU = NA,
							Latitude = NA,
							Longitude = NA,
							CollectionType = NA,
							Comments = NA,
							Frequency2 = paste(Frequency, "(", Id, ")")) %>%
					select(-Animal_ID, -Freq, -Unit, -CaptureDate, -COD, -state, 
							-date) %>%
					mutate(Status = ifelse(Status == 0, NA, Status),
							Status = ifelse(Status == 11, 0, Status),
							Status = ifelse(Status == 10, 1, Status),
							Cause_of_Death = ifelse(Cause_of_Death == "", NA, 
													Cause_of_Death)) %>%
					group_by(Id) %>%
					mutate(Dead = grepl("other|harv", Cause_of_Death),
							Censored = !Dead,
							Dead = substr(Dead, 1, 1),
							Censored = substr(Censored, 1, 1),
							CollectionType = c("Deer Capture", 
												rep("Deer Monitoring", n() - 2),
												NA),
							CollectionType = ifelse(Dead == "T", 
									c(CollectionType[n()-1], "Deer Mortality"), 
									CollectionType))
							
		return(out)
		}
#################################################################################
		#  Example call
		#  Set working directory 
		setwd("C:/tmp/sdsurv")
		
		#  List all of the files in the folder, these should all be of the same
		#  species and ageclass...if multiple ageclasses or species are required
		#  then create a folder for each ageclass by species combination
		#  If desired we can break out sex too
		fnames <- list.files()
		#  If the working directory was not set above the line below is 
		#   equivalent
		#  fnames <- list.files("C:/tmp/sdsurv")
		
		#  One file at a time
		new_data <- read_convert(fnames[1], "White-tailed Deer", "adult")
		
		#  All of the files at once
		new_data2 <- do.call(rbind, sapply(fnames, read_convert, 
							"White-tailed Deer", "adult", simplify = F))
							
		#  All in one line...
		new_data3 <- do.call(rbind, sapply(list.files("C:/tmp/sdsurv"), 
								read_convert, "White-tailed Deer", "adult", 
								simplify = F)) 
								
		#  Save file as .csv
		write.csv(new_data3, file = "C:/tmp/mydata.csv")

		#  If you write the output to the directory with the raw data list.files
		#  will try to run the function on the new output and fail, so it is
		#  best to pick a new directory in which to save the new data files
		
		
		
		
		
		
		
		
		
		