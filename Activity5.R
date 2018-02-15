# Jonathan Gross
# 435133
# Pol Sci 5625
# Activity 5
rm(list = ls())

#Create Student Function 
#Input: @name = string to name student
#Output: object of class student that has a name and 4 integer values for name courage intellegence and ambition
createStudent = function(name){
  # Attributes are randomly assigned from 1 -100
  courage = floor(runif(1,1,101))
  intellegence = floor(runif(1,1,101))
  ambition = floor(runif(1,1,101))
  effort = floor(runif(1,1,101))
  return(structure(list("name" =name,"courage" = courage,"intellgence" = intellegence,"ambition" = ambition,"effort" = effort), class = "student"))
}

# Test Student: Create example student
j = createStudent("jon")
j

#Sorter Method for Generic Sort Function
#Inputs: @stud = student object to be sorted
#     @x, a 4x4 matrix correlating attributes to houses, defaults to identity matrix
#Output: House which student is sorted into
sort.student = function(stud, x= diag(4)){
  # Get 4 attributes
  a = c(stud[[2]],stud[[3]],stud[[4]],stud[[5]])
  # Matrix multiply transpose(x)*t
  vect = (t(x)%*%a)
  index = which.max(vect)

  #Return Appropriate Sorting
  if (index == 1){
    return("GRYFFINDOR")
  }  else if (index == 2){
    return("SLYTHERIN")
  }else if (index == 3){
    return("RAVENCLAW")
  }else if (index == 4){
    return("HUFFLEPUFF")
  }
}

# Test
sort(j)
a = diag(4) 
sort.student(j,a)





