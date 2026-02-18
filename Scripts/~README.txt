~README

BEFORE RUNNING:
Step 1: Go into the "01 Run.R" file in the "02 Scripts" folder and uncomment line #6 and change the working directory within the setwd() function to map to the scripts folder. I included my own working directory as an example. 

Step 2: If you do not have the package "renv" downloaded in R, before running through the scripts for the first time, go into the "00 Header.R" file and uncomment line 11. After the package is downloaded, make sure to uncomment out this line of code if running again. 
 
Step 3: Confirm you are on R Version 4.4.1. You can check one of two ways, the first is going to the ribbon at the top and finding Tools -> Global Options -> General. The other is entering 'R.version' into the console which should return the following result: 

platform       x86_64-w64-mingw32               
arch           x86_64                           
os             mingw32                          
crt            ucrt                             
system         x86_64, mingw32                  
status                                          
major          4                                
minor          4.1                              
year           2024                             
month          06                               
day            14                               
svn rev        86737                            
language       R                                
version.string R version 4.4.1 (2024-06-14 ucrt)
nickname       Race for Your Life     

FIRST TIME RUNNING:

- I left line #23 uncommented in the "00 Header.R" script. For the first time you run the script, you need to activate the renv lockfile to restore the necessary packages. It will then prompt you on the console page to restore to the current lock file (select option 1). You only need to do this once. 
 
- Confirm the raw input ("iowa_liquor_sales_2018_2021.csv") is in the input folder. If not, then download it into the "01 Input" folder. 

- YOU CAN RUN ENTIRE CODE BASE THROUGH "01 RUN.R"
