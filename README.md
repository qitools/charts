OpenCPU App: QItools
---------------------

Simple OpenCPU Application. 

To access online, R is not needed on your computer, browse to https://public.opencpu.org/ocpu/github/badgettrg/qitools/www/

To install in R on your computer:

    library(devtools)
    install_github("opencpu", "jeroenooms")
    install_github("qitools", "badgettrg")

    library(opencpu)
    opencpu$browse("library/qitools/www")

Use the same function using a local webserver on your computer:

    library(qitools)
    qitools()
    ?qitools

For more information about OpenCPU apps, see [opencpu.js](https://github.com/jeroenooms/opencpu.js#readme)
