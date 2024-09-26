# - reading HOBO data
# - calculating water level (removing atmospheric pressure from water pressure)
# - plot corrected water level
# -------------------------------------
# logger time GMT+02:00
# start at Aug 20, 04:00 GMT+02:00 == 08:00 local time GMT+06:00
# (Kyrgyzstan does not observe daylight saving time)
# --->
# 2023-12-13: Removed the need for time shift. 
# Instead EXPORT AS GMT+6 or whatever timezone you are in, in the HOBOware
time_difference_hours = 0

# Arguments
path = "/Users/pohle/Dropbox/Central_Asia/HOBO/summer_school_Suyek/data/"
# For Windows it will be something like this:
# path = "C:/summer_school_Suyek/data/"

# Input files
filename_atmo_pressure = 'HOBO_atmo_suyek_21411027.csv'
filename_water_pressure_clean = 'HOBO-clean_water_suyek_21411023.csv'
filename_water_pressure_original = 'HOBO_water_suyek_21411023.csv'   # Excel is messing up the date! Take origninal dates from original file

# Output files
filename_output = 'HOBO_water-corrected_suyek_2.csv'

# Constant:
kPa_to_cm <- 1/0.0980665  # to transform kPa to cm water level




# ------------ NO NEED TO CHANGE ANYTHING HEREAFTER --------------- #
setwd(path)
source('../R/FUN_install_packages.R')   # this checks if packages are installed

# This reads all the data
dat_atmo = read.csv(filename_atmo_pressure, skip=2, header=F, stringsAsFactors = F)
dat_wl_clean = read.csv(filename_water_pressure_clean, skip=2, header=F, stringsAsFactors = F)
dat_wl_orig = read.csv(filename_water_pressure_original, skip=2, header=F, stringsAsFactors = F)

range(date_at)
date_at = as.POSIXct(dat_atmo$V2, format='%m/%d/%Y %I:%M:%S %p', tz='GMT')
date_wl = as.POSIXct(dat_wl_orig$V2, format='%m/%d/%Y %I:%M:%S %p', tz='GMT')
# adjust time GMT+2 to GMT+6
# 2023-12-13: Not needed anymore !
# Export HOBO file from HOBOware in correct time zone!
# date_at + hours(time_difference_hours)
# date_wl + hours(time_difference_hours)

wl = dat_wl_clean$V3   # water + atmospheric pressure
at = dat_atmo$V3       # atmospheric pressure
ta = dat_atmo$V4       # atmospheric temperature
tw = dat_wl_clean$V4   # water temperature

# merge and align
df_wl = data.frame(date= date_wl, level=wl, water_temperature=tw)
df_at = data.frame(date= date_at, atmo=at, atmo_temperature=ta)
wl_at = merge(df_wl, df_at, by='date')

# na_ind = which(wl_at$level>75)
# wl_at[na_ind,] = NA
plot(wl_at$date, wl_at$level, type='l')

wl_at$wl_clean = wl_at$level-wl_at$atmo
plot(wl_at$date,wl_at$wl_clean, type='l', xlab = 'Time', ylab='Water level [dm]')
plot(wl_at$date[1:30],wl_at$wl_clean[1:30], type='l', xlab = 'Time', ylab='Water level [dm]')

write.csv(wl_at,file = filename_output,quote = F,row.names = F )


