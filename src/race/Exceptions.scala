package race
// for when there are less players saved than the user wants to play with
class TooFewPlayersException extends Exception
// for when the track dosen't have starting squares for all the players
class TooFewStartingSquaresException extends Exception