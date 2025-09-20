#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' To determine participant's door selection.
#' 
#' @description
#' `select_door()` returns door participant's choice
#' 
#' @details
#' Once the game is set, the participant proceed to select one door.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return  The function returns a random number between 1 and 3, which indicates participant
#' @examples 
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a goat door given the contestant's pick.
#'
#' @description
#' `open_goat_door()` reveals one door with a goat, according 
#' to the rules of the Monty Hall problem.
#'
#' @details
#' If the contestant initially picked the car, the function 
#' randomly selects one of the two goat doors to open. 
#' If the contestant initially picked a goat, the function 
#' opens the other goat door (not the contestant’s pick).
#'
#' @param game A character vector returned by `create_game()`.
#' @param a.pick An integer (1–3) indicating the contestant’s first pick.
#' 
#' @return An integer (1–3) corresponding to the door that is opened.
#'
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Decide whether to stay or switch doors.
#'
#' @description
#' `change_door()` determines the contestant’s final pick 
#' depending on the chosen strategy (stay or switch).
#'
#' @details
#' If `stay = TRUE`, the contestant keeps the original pick.  
#' If `stay = FALSE`, the contestant switches to the only 
#' unopened door.
#'
#' @param stay Logical. If `TRUE`, the contestant stays; if `FALSE`, switches.
#' @param opened.door The door number opened by the host.
#' @param a.pick The contestant’s initial door choice.
#'
#' @return An integer (1–3) indicating the final pick.
#'
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   opened <- open_goat_door(game, pick)
#'   change_door(stay = FALSE, opened.door = opened, a.pick = pick)
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the contestant wins the game.
#'
#' @description
#' `determine_winner()` checks if the contestant’s final pick 
#' corresponds to the car or a goat.
#'
#' @details
#' Uses the final pick and the game setup to return whether 
#' the contestant won or lost.
#'
#' @param final.pick Integer (1–3). The contestant’s final choice.
#' @param game A character vector with "car" and "goat" values 
#'   created by `create_game()`.
#'
#' @return A string, either `"WIN"` or `"LOSE"`.
#'
#' @examples
#'   game <- create_game()
#'   pick <- select_door()
#'   opened <- open_goat_door(game, pick)
#'   final <- change_door(FALSE, opened, pick)
#'   determine_winner(final, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play one full Monty Hall game.
#'
#' @description
#' `play_game()` runs the entire Monty Hall problem sequence, 
#' returning the outcome for both staying and switching strategies.
#'
#' @details
#' This function creates a game, makes an initial pick, opens a goat door, 
#' and evaluates the results of both strategies ("stay" and "switch").
#'
#' @param ... No arguments are used.
#'
#' @return A data frame with two rows: one for "stay" and one for "switch", 
#' showing whether each strategy resulted in "WIN" or "LOSE".
#'
#' @examples
#'   play_game()
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}




#' @title
#' Play multiple Monty Hall games.
#' 
#' @description
#' `play_n_games()` simulates the Monty Hall problem across 
#' many repetitions and summarizes the outcomes.
#'
#' @details
#' For each repetition, the function calls `play_game()`. 
#' Results are combined into a data frame, and the proportions 
#' of wins and losses are displayed for both strategies.
#'
#' @param n Number of games to simulate. Default is 100.
#'
#' @return A data frame with the results of all games played.
#' 
#' @examples
#'   play_n_games(230)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
