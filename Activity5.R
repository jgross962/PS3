# Jonathan Gross
# 435133
# Pol Sci 5625
# Activity 5
rm(list = ls())

#Create Student Function 
#Input: 
#   @name = string to name student
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
#Inputs:
#     @stud = student object to be sorted
#     @x, a 4x4 matrix correlating attributes to houses, defaults to identity matrix
#Output: House which student is sorted into
sort.student = function(stud, x= diag(4)){
  # Get 4 attributes
  a = c(stud[[2]],stud[[3]],stud[[4]],stud[[5]])
  # Matrix multiply transpose(x)*t
  vect = (t(x)%*%a)
  #Get index of highest value
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


#Modify Sorter Object to Return the student object instead of a string
#Inputs:
#     @stud = student object to be sorted
#     @x, a 4x4 matrix correlating attributes to houses, defaults to identity matrix
#Output: Student Object with 
sort.student = function(stud, x= diag(4)){
  # Get 4 attributes
  a = c(stud[[2]],stud[[3]],stud[[4]],stud[[5]])
  # Matrix multiply transpose(x)*t
  vect = (t(x)%*%a)
  # Get Index of Highest Value
  index = which.max(vect)
  
  #Return Appropriate Sorting
  if (index == 1){
    class(stud) = c(class(j),"GRYFFINDOR")
    return(stud)
  }  else if (index == 2){
    class(stud) = c(class(j),"SLYTHERIN")
    return(stud)
  }else if (index == 3){
    class(stud) = c(class(j),"RAVENCLAW")
    return(stud)
  }else if (index == 4){
    class(stud) = c(class(j),"HUFFLEPUFF")
    return(stud)
  }
}

# Test
k = createStudent("John")
k
k = sort(k)
k
class(stud)
a = diag(4) 
sort.student(j,a)






