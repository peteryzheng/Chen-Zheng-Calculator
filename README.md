# Chen-Zheng-Calculator

## Ways of running software 
- Launch this shiny app from RStudio
- [Download standalone application from our paper](https://scholarblogs.emory.edu/zhengjiachen/2017/10/23/chen-zheng-calculator-for-operating-characteristics-of-phase-i-clinical-trials/) 

## Launching from RStudio
### Installation
1. Install rStudio [here](https://rstudio.com/products/rstudio/download/)
2. Install R [here](https://www.r-project.org)
3. Open rStudio to install R packages on terminal
    ```{r}
    install.packages("ggplot")
    install.packages("shiny")
    install.packages("shinyjs")
    ```
### Obtaining and running code from github
1. Clone this repository through [commandline git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) OR [github](https://desktop.github.com)
2. Open [Server.R](https://github.com/peteryzheng/Chen-Zheng-Calculator/blob/master/server.R) script in your Rstudio and click `Run App` on the upper right hand corner of the window

## Using the app from the download link
1. Download the application (This might take a while)
2. Unzip the downloaded file
3. Double click `Run`

## User Manual
To use our software, first input parameters such as dose de-escalation (with vs. without), dose levels, true probability of DLT at each dose level as well as corresponding dosages (optional). Table and graphic outputs will be produced and made downloadable for further editing purposes.

## Citation
Chen, Zhengjia, Zheng, Youyun, et al. "Interactive calculator for operating characteristics of phase I cancer clinical trials using standard 3+ 3 designs." Contemporary clinical trials communications 12 (2018): 145-153.
