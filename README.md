# pacific-cod-2023

Status update for WCVI Pacific Cod stock

## Steps

1. Pull in the data to the end of 2022

2. Run the glm to get the mean weight index

3. Make the iscam input files

The above three steps are all called in the file **get-iscam-inputs.R**

Then:

4. Run the seven models with the input files and drop the resulting folders into the models folder 

5. Make the draft SR

For now type this into the command line:

setwd("report")
bookdown::render_book("index.Rmd")

knitting index.Rmd is currently not working due to csasdown changes.




