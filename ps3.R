# Jonathan Gross
# Pol Sci 5625
#PS 3

# S3 Methodology
rm(list = ls())

PickDoor = structure(c(doorVal = floor(runif(1,1,4))),class="door").PickDoor

setValue.Door = function(door,x){ # If want to manually change value
  door[[1]] = x
}

setValue.Door(PickDoor,1)


playGame = function(x){
  UseMethod("playGame",x)
}

playGame.door = function(x){
  pickedDoor = x[[1]]
  CarDoor = floor(runif(1,1,4))
  if(pickedDoor == CarDoor){
    return("Congratulations You've Won A Car")
  }else{
    return("I'm Sorry You Didn't Win")
  }
}
