# Jonathan Gross
# Pol Sci 5625
#PS 3

rm(list = ls())




# S3 Methodology
#Create Door Object w/ Random Number 1 2 or 3
#Could alternatively directly set to a value via doorVal = c if want to manually pick
PickDoor = structure(c(doorVal = floor(runif(1,1,4))),class="door").PickDoor

# If want to manually change value instead of random
setValue.Door = function(door,x){ 
  door[[1]] = x
}

setValue.Door(PickDoor,1) #Change door to 1 for test purpose

#Create Generic PlayGame Function
playGame = function(x){
  UseMethod("playGame",x)
}
#Set method for class door
playGame.door = function(x){
  pickedDoor = x[[1]]
  #Create random winner door
  CarDoor = floor(runif(1,1,4))
  #Display appropriate output based on if won
  if(pickedDoor == CarDoor){
    return("Congratulations You've Won A Car")
  }else{
    return("I'm Sorry You Didn't Win")
  }
}


# S4 Methodology

#Create door class, that accepts numeric as input
setClass(
  Class = "Door",
  representation = representation (
    DoorNumber = "numeric"
  ),
  prototype = prototype(
    DoorNumber = c()
 
  )
)
#Check User has properly created door, by checking class of input
setValidity("Door",function(object){
  if (class(object@DoorNumber) != "numeric"){
    return("@Door is not a valid value")
  }
})

#Check validity when a new door is created
setMethod("initialize","Door", function(.Object,...){
  value = callNextMethod()
  validObject(value)
  return(value)
})

#Test - create a working door and a not working door
goodDoor = new("Door",DoorNumber = 2)
badDoor = new("Door", DoorNumber = "test")

#Create Generic PlayGame Function that take's users door as input
setGeneric("PlayGame", 
           function(object = "Door"){
             standardGeneric("PlayGame")
           })

#Create plyGame method -- randomly picks value 1 2 or 3 and compares against user value
setMethod("PlayGame","Door",
          function(object){
            #Create the winning door with random value
            WinningDoor = new("Door",DoorNumber = floor(runif(1,1,4)))
            #Print Appropriate Output
            if(WinningDoor@DoorNumber == object@DoorNumber){
              return("Congratulations You've Won A Car")
            }else{
              return("I'm Sorry You Didn't Win")
            }
          })

#Test PlayGame Method multiple times to ensure you win sometimes
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)
