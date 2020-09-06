

#' @title
#' Montyball
#'
#' @description
#' `play_game(x,y,z)` returns the strategy and outcome for x number of doors, in which y number of goats and z number of
#' goats. x=y+z.
#'
#' @details
#' This function is a combination of a few different subfunctions:
#' create_game, select_door, open_goat_door, change_door and determin_winner.
#' create_game:    It starts up the game and returns x number doors, with randomly assigned positions for
#'                 y number of goats and z number of cars.
#' select_door:    It retruns the first selected door number which is randomly created by the function???
#' open_goat_door: It reveals a goat for the host.
#' change_door:    makes your final pick depending upon if you intend to stay or switch.
#' determin_winner: decides if you win the car or not.
#'
#' @param x Numeric. Number of doors in the game.
#' @param y Numeric. Number of goats behind the doors. There are at least 2 goats behind the doors.
#' @param z Numeric. Number of cars behind the doors. y+z = x and there is at least on car in the game.
#' @return strategy and Outcome for the game. The function returns a dataframe which includes strings and numbers.
#'
#' @examples
#' play_game(10,1,9)
#' play_game(10,8,2)
#'
#' \dontrun{
#' play_game("x",'y','z')
#' }


create_game <- function(doornumber, goatNum, carNum)
{
  goat<-rep(c("goat"),times=goatNum)
  car<-rep(c("car"),times=carNum)

  a.game <- sample( x=c(goat,car), size=doornumber, replace=F )
  return( a.game )
}


select_door <- function(doornumber )
{

  doors <- c(1:doornumber)
  a.pick <- sample( x=doors, size=1, replace=F) # YOUR CODE HERE...
  return( a.pick )  # number between 1 and 5

}

open_goat_door <- function( game, a.pick, doornumber )
{

  v <- c(1:doornumber)
  for(i in v) {
    if (i != a.pick)  {
      if (game[i] == "goat") { return (i) }
    }
  }
}


change_door <- function( stay=T, opened.door, a.pick, doornumber )
{
  select<-vector()

  if (stay == T)
  {
    return (a.pick)
  }
  else
  {
    v <- c(1:doornumber)
    existing <- c(opened.door,a.pick)
    for(i in v) {

      if ((i %in% existing ) ==F) {select<-c(select,i) }
    }
  }
  final.select<-sample( x=select, size=1, replace=F)
  return (final.select)
}


determine_winner <- function( final.pick, game )
{

  if( game[final.pick] ==  "car" )
  {
    return( "WIN" )
  }
  else{
    return( "LOSE" )
  }

}

play_game <- function(doornum, goatNum, carNum )
{

  new.game <- create_game(doornum,goatNum,carNum)
  first.pick <- select_door(doornum)
  opened.door <- open_goat_door( new.game, first.pick,doornum)
  final.pick.stay <- change_door( stay=T, opened.door, first.pick,doornum )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick,doornum)
  outcome.stay <- determine_winner( final.pick.stay, new.game )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

#play_game(5,4,1)
