# Fits run in the background on mirai daemons, started by the first fit (see
# start_daemons()). The daemons stop with the app.
# jarl-ignore undesirable_operator: the app directory belongs to the package.
shiny::onStop(kelpbioshiny:::stop_daemons)

# A fit bundle holds every fit's posterior draws, which with many sites and
# years, or reference data appended, outgrow Shiny's 5 MB upload limit.
options(shiny.maxRequestSize = 100 * 1024^2)
