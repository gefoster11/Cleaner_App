# Cleaner_App
Used for the cleaning of breath-by-breath and beat-by-beat data files

# Introduction
This application can be used to remove outlying data points to clean human breath-by-breath and beat-by-beat data files for further analysis

# Getting set up
To run this application on your system you must first install [R](https://cran.r-project.org/) and [R studio](https://www.rstudio.com/) both freely available and open source. 

In order to run directly from github you must first install 'shiny'

`install.packages('shiny')`

# Running the application
After installing R, Rstudio, and shiny, you can run this application without the need to download it directly from github by running this command from the console in R studio.

`shiny::runGitHub('Cleaner_App', 'gefoster11', ref="main")`

The application should run in your default browser. Using this approach will ensure you are always using the latest source code available within this github repository.

# Preparing your input data
Input data files can be loaded as comma, semicolon, or tab delimited text files. Files should have 'Time' in the first column followed by numeric input columns with the first row serving as the row heading. Text based columns should not be included. Example files can be found in the example data directory.

# Workflow within the App
Load the desired input data file by drag and drop. Adjust delimiter type to acheive the correct format of the parsing table. Then click the 'Data Loaded' input. The 'Selected Data' and 'Plot' tabs should populate. If you have many variables and the plot is too squished you can adjust the 'Plot Height'. The plot displays the value associated with each breath or beat as a function of time and the 30s average (black line) ± SD (green ribbon). To select exact start and end times for data processing provide the start and end time.  Clicking on data points will toggle points (on/off). Erroneuous data points set for removal by the user will be marked as red.  Click and drag to select a number of data points and click 'remove points' to toggle points. Alternatively, the user can use an autoclean function which will identify outliers from a moving time window. The time window is set by the user ('AutoClean Time Interval'). Within each time window data points that are less than Q1 - IQR X 1.5 and greater than Q3 + IQR X 1.5 are labelled as outliers. To reset toggle points, click 'Reset'. Click 'Download' To save selected data. Note data points marked for exclusion are replaced by NA and the output data frame is returned in wide format.

# Conclusion
This application provide a simple solution for data cleaning. Be aware that not all removed data points are abherrent data points and inspection of the raw data is recommended to avoid removing breaths and beats that are in fact legitimate.

# Referencing
If you use this application in your research, please reference this github repository in your publications.
