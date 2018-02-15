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
    class(stud) = c(class(stud),"GRYFFINDOR")
    return(stud)
  }  else if (index == 2){
    class(stud) = c(class(stud),"SLYTHERIN")
    return(stud)
  }else if (index == 3){
    class(stud) = c(class(stud),"RAVENCLAW")
    return(stud)
  }else if (index == 4){
    class(stud) = c(class(stud),"HUFFLEPUFF")
    return(stud)
  }
}

# Test
k = createStudent("John")
k
k = sort(k)
k
a = diag(4) 
sort.student(j,a)


# Create 4 Enviornments, each representing a dorm for each house
Gryffindor_Tower = new.env()
Black_lake = new.env()
Ravenclaw_Tower = new.env()
Basement = new.env()


#Crate Generic Curfew Function
curfew = function (x){
  UseMethod("curfew",x)
}

#Create Curfew Method for each house that assigns student to appropriate environment/dorm
#Input: Student Object to be assigned to a class
# No output
#Note: assigning based on name of student to ensure students are not overwritten in environment
curfew.GRYFFINDOR = function(stud){
  assign(stud$name,stud,envir =Gryffindor_Tower )
  
}

curfew.SLYTHERIN = function(stud){
  assign(stud$name,stud,envir =Black_lake )
}

curfew.RAVENCLAW = function(stud){
  assign(stud$name,stud,envir =Ravenclaw_Tower )

}

curfew.HUFFLEPUFF = function(stud){
  assign(stud$name,stud,envir =Basement )
 
}

#Test
a = sort(createStudent("amy"))
b = sort(createStudent("bob"))
c = sort(createStudent("charles"))
d = sort(createStudent("dan"))
curfew(a)
curfew(b)
curfew(c)
curfew(d)
ls(Gryffindor_Tower)
ls(Black_lake)
ls(Ravenclaw_Tower)
ls(Basement)

