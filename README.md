OpenCPU App: charts
---------------------

Simple OpenCPU Application. 

To access online, R is not needed on your computer, browse to https://public.opencpu.org/ocpu/github/qitools/charts/www/

To install in R on your computer:

    library(devtools)
    install_github("opencpu", "jeroenooms")
    install_github("charts", "qitools")

    library(opencpu)
    opencpu$browse("library/charts/www")

Use the same function using a local webserver on your computer:

    library(charts)
    charts()
    ?charts

For more information about OpenCPU apps, see [opencpu.js](https://github.com/jeroenooms/opencpu.js#readme)
