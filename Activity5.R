#Activity 5

#Create Student
createStudent = function(name){
  courage = floor(runif(1,1,101))
  intellegence = floor(runif(1,1,101))
  ambition = floor(runif(1,1,101))
  effort = floor(runif(1,1,101))
  return(structure(list("name" =name,"courage" = courage,"intellgence" = intellegence,"ambition" = ambition,"effort" = effort), class = "student"))
}

j = createStudent("jon")

#Sorter
x <- c(1:100)
index = x[which(x==max(x))]

sort.student = function(stud, x){
  a = c(stud[[2]],stud[[3]],stud[[4]],stud[[5]])
  vect = (t(x)%*%a)
  firstElement = vect[1]
  vect = sort(vect,decreasing=T)
  index = 0
  for (i in 1:4){
    if (firstElement == vect[i]){
      index = i
    }
  }
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

a = diag(4)
a
sort(j,a)
