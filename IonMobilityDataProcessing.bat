@ECHO OFF
ECHO Ion Mobility Data Processing Script
:: In the next line, enter R path in the line below after the set command
set RSCRIPT="C:\Program Files\R\R-4.2.1\bin\Rscript.exe"

:: In the next line, enter the path to the CommandLineInterface R file
set CLI="C:\Users\austi\Source\Repos\IonMobilityDataProcessing\R\CommandLineInterface.R"

:: In the next line, enter the path to the local copy of the IonMobilityDataProcessing code
set RCODE="C:\Users\austi\Source\Repos\IonMobilityDataProcessing"

:: In the next line, enter the path to the input directory
set INPUTDIR="C:\Users\austi\Source\Repos\IonMobilityDataProcessing\Data"

:: In the next line, enter the path to the output directory 
set OUTPUTDIR="C:\Users\austi\Source\Repos\IonMobilityDataProcessing\Data"

%RSCRIPT% %CLI% -r %RCODE% -i %INPUTDIR% -o %OUTPUTDIR% 
PAUSE 