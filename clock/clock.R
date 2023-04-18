create <- function(hours, minutes) {
  total_minutes <- hours * 60 + minutes
  normalized_hours <- (total_minutes %/% 60) %% 24
  normalized_minutes <- total_minutes %% 60
  list(hours = normalized_hours, minutes = normalized_minutes)
}

add <- function(minutes, clock) {

}

subtract <- function(minutes, clock) {

}

display <- function(clock) {
  sprintf("%02d:%02d", clock$hours, clock$minutes)
}
