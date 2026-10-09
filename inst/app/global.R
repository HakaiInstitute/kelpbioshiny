# Fits run in the background on mirai daemons, started by the first fit (see
# start_daemons()). The daemons stop with the app.
shiny::onStop(function() if (mirai::daemons_set()) mirai::daemons(0))
