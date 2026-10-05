# Fits run in the background on mirai daemons, with a dispatcher so that a
# running fit can be cancelled. The daemons stop with the app.
if (!mirai::daemons_set()) mirai::daemons(2)
shiny::onStop(function() mirai::daemons(0))
