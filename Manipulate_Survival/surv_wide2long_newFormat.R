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
			print(fname)
			#  Read file
			x <- read.csv(fname, as.is = T)

			#  Convert wide to long format
			out <- x %>%
					gather(date, state, -Animal_ID, -Freq, -Unit, 
							-CaptureDate, -COD, -Latitude, -Longitude) %>%
					mutate(UniqueID = Animal_ID,
							ID = Animal_ID,
							Date1 = gsub("X", "", date),
							Date1 = format(as.Date(Date1, "%m.%d.%Y"), "%m/%d/%Y"),
							Species = sp,
							Id1 = Animal_ID,
							Frequency = Freq,
							Status = state,
							Fate = COD,
							MortSigHeard = NA,
							Capture_Unit = Unit,
							Monitoring_Unit = NA,
							Ageclass = aclass,
							Sex = sx,
							Age = NA,
							Censored = NA,
							Dead = NA,
							Fate_Unit = NA,
							Weight = NA,
							HoofGrowth = NA,
							Observer = NA,
							DAU = NA,
							DeathEvidence = NA,
							Comments = NA,
							Latitude = Latitude,
							Longitude = Longitude,
							CollectionType = NA,
							Source = NA) %>%
					select(-Animal_ID, -Freq, -Unit, -COD, -state, 
							-date) %>%
					mutate(Status = ifelse(Status == 0, NA, Status),
							Status = ifelse(Status == 11, 0, Status),
							Status = ifelse(Status == 10, 1, Status),
							Fate = ifelse(Fate == "", NA, 
													tolower(Fate))) %>%
					group_by(Id1) %>%
					filter(!all(is.na(Status)))
					
			del <- unique(x$Animal_ID)[which(!unique(x$Animal_ID) %in% unique(out$Id))]
			if(length(del) > 0){
				cat("\n\n", "Animal(s)", 
				del,
				"have no data and were deleted (all 0 encounter history?)")
			}
			
		return(out)
		}
#################################################################################
		combine_fun <- function(x){
			#  Takes output of read_convert, one animal at a time
			#  Returns a data.frame with columns altered to reflect status and
			#  fate of animal
			
			dts <- as.Date(x$Date, "%m/%d/%Y")
			
			tmp <- x %>% 
					arrange(dts) %>%
					distinct(Id1, Date1) %>%
					select(UniqueID, ID, Date1, Species, Id1, Frequency, Status, Fate, MortSigHeard,
							Capture_Unit, Monitoring_Unit, Ageclass, Sex, Age, Censored, Dead, 
							Fate_Unit, Weight, HoofGrowth, Observer, DAU, DeathEvidence, Comments,
							Latitude, Longitude, CollectionType, Source) %>%
					filter(!is.na(Status))
			
			cap <- x %>%
					mutate(Date1 = CaptureDate, 
							CollectionType = "Deer Capture") %>%
					distinct() %>%
					select(UniqueID, ID, Date1, Species, Id1, Frequency, Status, Fate, MortSigHeard,
							Capture_Unit, Monitoring_Unit, Ageclass, Sex, Age, Censored, Dead, 
							Fate_Unit, Weight, HoofGrowth, Observer, DAU, DeathEvidence, Comments,
							Latitude, Longitude, CollectionType, Source)
			#  Some animals die in the same month as capture, put an entry for
			#  Capture and repeat the information for mortality
			if(nrow(tmp) == 1){
				tmp <- data.frame(rbind(cap, tmp))
			}
			if(nrow(tmp) > 2){
				tmp$CollectionType[1:(nrow(tmp)-1)] <- "Deer Monitoring"			
			}
			tmp$CollectionType[nrow(tmp)] <- "Deer Mortality"
			
			#  Set lat and long to NA because they are only known at cap and death
			tmp$Latitude <- NA
			tmp$Longitude <- NA

			#  Get unique of COD
			cod <- unique(x$Fate)
			#  Use COD column to decide if animal is dead
			tmp$Dead[nrow(tmp)] <- ifelse(all(is.na(cod)), F, T)
			#  If not dead then censored
			tmp$Censored[nrow(tmp)] <- !tmp$Dead[nrow(tmp)]
			#  Insert COD in last row...when you would know the animal's fate
			tmp$Fate[nrow(tmp)] <- cod[!is.na(cod)][1]
			tmp$Fate[is.na(tmp$Fate[nrow(tmp)]) & tmp$Status[nrow(tmp)] == 1] <- "Alive"
			tmp$Fate[1:(nrow(tmp)-1)] <- NA
			#  Convert Status to reflect COD		
			tmp$Status[grepl("harv", tmp$Fate, ignore.case = T) & tmp$Dead] <- 2
			tmp$Status[grepl("ther", tmp$Fate, ignore.case = T) & tmp$Dead] <- 3
			
			#  Combine capture information and main data
			out <- bind_rows(cap, tmp) %>%
					arrange(Id1)
						
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
							
			#  Create a list with one entry per individual
			id_lst <- split(tmp, tmp$Id1)
			
			#  Morph individual summary information across occassions
			out <- bind_rows(lapply(id_lst, combine_fun))
			
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
		new_data <- wrapper_fun(file_dir = "C:/Users/josh.nowak/Downloads/sd_surv", 
								sp_input = "White-tailed Deer",
								ageclass_input = "adult",
								sex_input = "f",
								save_file = "C:/Temp/bestdataever.csv")

		#  If you write the output to the directory with the raw data list.files
		#  will try to run the function on the new output and fail, so it is
		#  best to pick a new directory in which to save the new data files
		
		
		
		
		
		
		
		
		
		