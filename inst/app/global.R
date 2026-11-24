# Fits run in the background on mirai daemons, started by the first fit (see
# start_daemons()). The daemons stop with the app.
# jarl-ignore undesirable_operator: the app directory belongs to the package.
shiny::onStop(kelpbioshiny:::stop_daemons)
