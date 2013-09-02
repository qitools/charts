OpenCPU App: QItools
---------------------

Simple OpenCPU Application. To install in R:

    library(devtools)
    install_github("opencpu", "jeroenooms")
    install_github("qitools", "opencpu")

    library(opencpu)
    opencpu$browse("library/qitools/www")

Use the same function locally:

    library(qitools)
    qitools()
    ?qitools

For more information about OpenCPU apps, see [opencpu.js](https://github.com/jeroenooms/opencpu.js#readme)
