		#  Survival file conversion
		#  Josh Nowak
		#  07/2015
#################################################################################
		#  Required packages
		require(tidyr)
		require(dplyr)
		#  all females
#################################################################################
		read_convert <- function(fname, sp, aclass, sx){
			#  Takes a file path as input, file must be .csv,
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
			x <- read.csv(fname, as.is = T)

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
							Sex = sx,
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
													Cause_of_Death))
		return(out)
		}
#################################################################################
		combine_fun <- function(x){
			#  Takes output of read_convert
			#  Returns a data.frame with columns altered to reflect status and
			#  fate of animal
			out <- x %>% 
					arrange(Id) %>%
					group_by(Id) %>%
					mutate(Dead = any(!is.na(Cause_of_Death)),
							Censored = !Dead,
							Dead = substr(Dead, 1, 1),
							Censored = substr(Censored, 1, 1),
							CollectionType = c("Deer Capture", 
												rep("Deer Monitoring", n() - 1)),
							CollectionType = ifelse(Status == 0, 
													"Deer Mortality", 
													CollectionType)) %>%
					filter(!is.na(CollectionType)) %>%
					mutate(Status = ifelse(grepl("harv", Cause_of_Death) &
											CollectionType == "Deer Mortality", 
											2, 
											Status),
							Status = ifelse(grepl("ther", Cause_of_Death) &
											CollectionType == "Deer Mortality", 
											3, 
											Status))
		return(out)		
		}
#################################################################################
		wrapper_fun <- function(file_dir, 
								sp_input, 
								ageclass_input, 
								sex_input,
								save_file = NULL){
			#  Takes a directory where files are stored, species, ageclass and
			#  sex of the animals in the directory
			#  Returns formatted data as data.frame
			require(dplyr)
			require(tidyr)
			
			#  Get file names
			fnames <- file.path(file_dir, list.files(file_dir, pattern = "csv"))
			
			#  Read in data and make long 
			tmp <- do.call(rbind, sapply(fnames, read_convert, 
							sp_input, ageclass_input, sex_input, simplify = F))
			
			#  Morph individual summary information across occassions
			out <- combine_fun(tmp)
			
			#  Save if desired
			if(!is.null(save_file)){
				if(!grepl("csv", save_file)){
					fname <- paste(save_file, "csv", sep = ".")
				}
				write.csv(out, file = save_file, row.names = F)
			}
		return(out)		
		}
#################################################################################
		#  Example call
		new_data <- wrapper_fun(file_dir = "C:/tmp/sdsurv", 
								sp_input = "White-tailed Deer",
								ageclass_input = "adult",
								sex_input = "f",
								save_file = "C:/tmp/bestdataever.csv")

		#  If you write the output to the directory with the raw data list.files
		#  will try to run the function on the new output and fail, so it is
		#  best to pick a new directory in which to save the new data files
		
		
		
		
		
		
		
		
		
		