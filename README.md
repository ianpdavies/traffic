# Downloading and formatting Bing Traffic REST API

Download RGB images of Bing traffic congestion data, georeference them, and classify them into categorical rasters where categories indicate the flow of traffic compared to the speed limits on the roads. 

**SCREENSHOT OF EXAMPLE OUTPUT WITH BING MAP LOGO**

## Terms of Use
This project was executed using the Bing Maps REST Services API for the purpose of academic research and publication with the authorization of the Bing Maps team. It is subject to the Bing Maps Platform API Terms of Use, including the Print Rights under the Microsoft Bing Maps Platform APIs Terms of Use. The exception to this relates to Section 2.3(c) of the Print Rights, which normally limits users to making no more than 10 copies of any “Road Map” or “Aerial Print”, for the purpose of academic publishing.  

Any third party user of this project's code is responsible for obtaining the authorization of the Bing Maps team for their own use and do not benefit from our agreement.


## Getting Started  
1. Get a Bing Maps API key https://msdn.microsoft.com/en-us/library/ff428642.aspx  
2. Create a text file whose content is: 
```
BING_KEY = 'your API key'
```
name it .Renviron and place it in your normalizePath(Sys.getenv("HOME")) directory.  
3. Place codes and accompanying files in your project directory (rootdir)/src/traffic  
4. Create a 'results' folder in your project directory  
5. Run run_script.R (update rootdir variable at the beginning of the script and )  
6. Create a text file (e.g. in notepad), containing (adapted to your set-up):
```
@echo off
"C:\Program Files\R\R-3.4.2\bin\x64\R.exe" -e source('F:/stormwater/src/traffic/traffic_api.R')
```
save it as 'scheduletask_source.bat'
7. In Windows (10) Task Scheduler, create a task which executes the .bat file every 1 hour (whether logged on or not with the highest privilege). If the task is already running, set it to 'Run a new instance in parallel' (feels weird? reply there: https://stackoverflow.com/questions/50921257/schedule-task-to-run-multiple-overlapping-instances-of-the-same-r-script/50952918#50952918). 
**ADD SCREENSHOT**
8. More open-source code to come for post-processing. Currently only developed with arcpy in Python. 

### Prerequisites

- Windows OS
- Bing Maps REST Services API key
- R
- Lots of storage space

## Explanations
- API output format (pixel vs. coordinates)
- Tiling
- Logo
- Classification
- Post-processing
- Estimate how much storage space is needed

## To-do
- Include arcpy code for post-processing
- Develop open-source code for post-processing
- Further clean up and improve code structure
- Make it faster (e.g. parallelize API call)

## Contributing

Please read [CODE_OF_CONDUCT.md] for details on our code of conduct, and the process for submitting pull requests.

## Authors

* **Ian Davies** - *Initial work* - https://github.com/ianpdavies
* **Mathis Messager** - *Current developer* - https://github.com/messamat

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments
* Thanks to the Bing Maps team for authorizing this project
* Thanks to Tostes, A. I. J., de LP Duarte-Figueiredo, F., Assunção, R., Salles, J., & Loureiro, A. A. (2013, August). From data to knowledge: city-wide traffic flows analysis and prediction using bing maps. In Proceedings of the 2nd ACM SIGKDD International Workshop on Urban Computing (p. 12). ACM. for the inspiration
* Hat tip to anyone whose code was used


--