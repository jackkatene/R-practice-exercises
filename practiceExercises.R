
#-DONE AND CHECKED-1. Perform a function that defines two matrices, the matrix x and its transpose; the function has to sum both matrices 
exercise1 = function()
{
  mat1 <- matrix(c(1,3,2,4),nrow=2,ncol=2,byrow=TRUE)
  mat2 <- matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE)
  mat3 = mat1 + mat2
  print (mat1)
  print (mat2)
  print (mat3)
}


#-DONE AND CHECKED-2. Perform a function that defines two matrices, matrix A and B; check if there is any common number. If so, replace it with the value -1.
exercise2 = function()
{
  mat1 <- matrix(c(1,3,2,4),nrow=2,ncol=2,byrow=TRUE)
  mat2 <- matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE)
  for (i in 1:nrow(mat1))
  {
    for (j in 1:ncol(mat1))
    {
      if (mat1[i,j] == mat2[i,j])
      {
        mat1[i,j] = -1
      }
    }
  }
  print (mat1)
}

#3. Create a function called "sumMatrix". The function has to sum the matrix by row and insert the result into a vector called "RowSum"; and the sum by columns and it will insert in a vector that is called “SumColumns”
exercise3 = function()
{
  mat1 <- matrix(c(1:9),nrow=3,ncol=3,byrow=TRUE)
  totalRowSums = c()
  totalColSums = c()
  rowSum = 0
  colSum = 0
  for (x in (1:3))
  {
    for (i in 1:nrow(mat1))
    {
      rowSum = rowSum + mat1[i]
      for (j in 1:ncol(mat1))
      {
        colSum = colSum + mat1[j]
      }
      totalColSums = c(totalColSums,colSum)
    }
    totalRowSums = c(totalRowSums,rowSum)
  }
  print (mat1)
  print (totalRowSums)
  print (totalColSums)
}


#3.
exercise3b = function()
{
  mat1 <- matrix(c(1:10),nrow=2,ncol=5)
  rowSums = c()
  colSums = c()
  print (mat1)
  for (r in 1:nrow(mat1))
  {
    for (c in 1:ncol(mat1))
    {
      sumr = mat1[r,c]+ mat1[r,c+1]
      print (sumr)
      #print(paste("Row", r, "and column",c, "have values of", mat1[r,c]))  
    }
  }
}

#3.
sumMatrix = function()
{
  A = matrix(data = 1:10, nrow = 2, ncol = 5)
  rawsum = c()
  colsum = c()
  for (i in 1:nrow(A))
  {
    for (j in 1:ncol(A))
    {
      sumr = A[i,j] + A[i,j+1]
      rawsum = c(rawsum,sumr)
      sumc = A[i,j] + A[i+1,j]
      colsum = c(colsum, sumc)
    }
  }
  print (rawsum)
  print (colsum)
}


#-DONE AND CHECKED-4a. Define a function that creates a vector A, for example, A = (2,4,5,7); you create another vector B, for example B = (1,2,3,6,8). The function must create two resulting vectors, one vector that returns the matching numbers and in others the non-matching numbers. Both vectors must be ordered from the least to the greatest. There should be no repeated numbers in the vectors. 
exercise4a = function()
{
  A = c(2,4,5,7)
  B = c(1,2,3,6,8)
  matched=c()
  no_match=c(A,B)
  i = 1
  while (i<= length(A))
  {
    for (j in 1:length(B))
    {
      if (A[i] == B[j])
      {
        matched = c(matched,A[i])
      }
    }
    i = i + 1
  }
  for (x in 1:length(no_match))
  {
    if (no_match[x] %in% matched)
    {
      no_match = no_match[-x]
    }
  }
  matched = sort(matched)
  no_match = sort(no_match)
  print (matched)
  print (no_match)
}


#-DONE AND CHECKED-4b. Define a function that creates a vector A, for example, A = (2,4,5,7); you create another vector B, for example B = (1,2,3,6,8). The function must create two resulting vectors, one vector that returns the matching numbers and in others the non-matching numbers. Both vectors must be ordered from the least to the greatest. There should be no repeated numbers in the vectors. 
exercise4b = function()
{
  A = c(2,4,5,7)
  B = c(1,2,3,6,8)
  matched=c()
  no_match=c(A,B)
  i = 1
  while (i<= length(A))
  {
    for (j in 1:length(B))
    {
      if (A[i] == B[j])
      {
        matched = c(matched,A[i])
      }
    }
    i = i + 1
  }
  for (x in 1:length(no_match))
  {
    if (no_match[x] %in% matched)
    {
      no_match = no_match[-x]
    }
  }
  for(t in 1:(length(no_match)-1)) #from 1 to the length - 1 as to not get out of range error
  {
    for(u in (t+1):length(no_match)) #comparing each number to the one after
    {
      if(no_match[t] > no_match[u])
      {
        #if element after is greater than element before, swap the element after for element before
        no_match[c(t,u)] = no_match[c(u,t)]
      }
    }
  }
  print (matched)
  print (no_match)
}
exercise4b()



#5. Define a function that creates a vector A, for example, A = (2,4,5,7); you create another vector B, for example B = (1,2,3,6,8). The function must create two resulting vectors, one vector that returns the even numbers and in others the odd numbers. Both vectors must be ordered from the highest to the lowest. If there are matching numbers, it will only appear once in the resulting vector. Res1 = (8,6,4,2); Res = (7,5,3,1)
exercse5 = function()
{
  A = c(2,4,5,7)
  B = c(1,2,3,6,8)
  even = c()
  odd = c()
  for (i in 1:length(A))
  {
    if (A[i] %% 2 == 1) #find odd numbers
    {
      odd = c(odd,A[i])
    }
    else 
    {
      even = c(even,A[i])
    }
  }
  for (i in 1:length(B))
  {
    if (B[i] %% 2 == 1) #find odd numbers
    {
      odd = c(odd,B[i])
    }
    else 
    {
      even = c(even,B[i])
    }
  }
  for(t in 1:(length(even)-1)) #from 1 to the length - 1 as to not get out of range error
  {
    for(u in (t+1):length(even)) #comparing each number to the one after
    {
      if(even[t] > even[u])
      {
        #if element after is greater than element before, swap the element after for element before
        even[c(t,u)] = even[c(u,t)]
      }
    }
  }
  for(t in 1:(length(odd)-1)) #from 1 to the length - 1 as to not get out of range error
  {
    for(u in (t+1):length(odd)) #comparing each number to the one after
    {
      if(odd[t] > odd[u])
      {
        #if element after is greater than element before, swap the element after for element before
        odd[c(t,u)] = odd[c(u,t)]
      }
    }
  }
  #delete duplicates odd
  dupe_count = length(odd)
  while (dupe_count >= 1)
  {
    for (y in length(odd):2)
    {
      if (odd[dupe_count]==odd[y])
      {
        odd = odd[-y]
      }
    }
    dupe_count = dupe_count + 1
  }
  #delete duplicates even
  dupe_count_2 = length(even)
  while (dupe_count_2 >=1)
  {
    for (y in length(even):1)
    {
      if (even[dupe_count_2]==even[y])
      {
        even = even[-y]
      }
    }
    dupe_count_2 = dupe_count_2 + 1
  }
  print (even)
  print (odd)
}
exercse5()

practice = function()
{
  X = c(1,5,7,5)
  count = 1
  RUNNING =TRUE
  while (count<=length(X) & RUNNING == TRUE)
  {
    if (count<length(X))
    {
      for (i in 2:length(X))
      {
        if (X[count] == X[i])
        {
          X = c(X,X[-i])
        }
      }
    }
    else
    {
      RUNNING = FALSE
    }
    count = count+1
  }
  print (X)
}


#6a. WORKING AND CHECKED Define a function that creates a vector A ordered from the least to the greatest, for example, A =(2,4,5,7); ask for a number by keyboard that is between the minimum number of the vector and the maximum; in the example it would be 2 and 7; check if the number keyboard input exists in vector; if it does not exist you must insert it in the correct position, in order from the least to the greatest: a) using the sort (). b) without using the sort ().

exercise6a = function()
{
  mysort = function (A)
  {
    for(t in 1:(length(A)-1)) #from 1 to the length - 1 as to not get out of range error
    {
      for(u in (t+1):length(A)) #comparing each number to the one after
      {
        if(A[t] > A[u])
        {
          #if element after is greater than element before, swap the element after for element before
          A[c(t,u)] = A[c(u,t)]
        }
      }
    }
    return (A)
  }
  
  A =c(2,14,8,9)
  #find minimum number WORKING
  min_num = A[1]
  for (w in 2:length(A))
  {
    if (A[w] < min_num)
    {
      min_num = A[w]
    }
  }
  
  #find max number WORKING 
  max_num = A[1]
  for (q in 2:length(A))
  {
    if (A[q] > max_num)
    {
      max_num = A[q]
    }
  }
  
  #check if num is in vector 
  RUNNING = TRUE
  while (RUNNING == TRUE)
  {
    A = mysort(A)
    print (A)
    cat("Enter a number between ", min_num, " and ",max_num)
    num = scan(,,1)
    signal = 0
    for (e in 1:length(A))
    {
      if (num == A[e])
      {
        signal = 1
      }
    }
    if(signal == 0)
    {
      A = c(A,num)
    }
    else if (signal == 1)
    {
      RUNNING = FALSE 
      cat("Finished vector is:")
      print (A)
    }
  }
}


#6b. WORKING AND CHECKED Define a function that creates a vector A ordered from the least to the greatest, for example, A =(2,4,5,7); ask for a number by keyboard that is between the minimum number of the vector and the maximum; in the example it would be 2 and 7; check if the number keyboard input exists in vector; if it does not exist you must insert it in the correct position, in order from the least to the greatest: a) using the sort (). b) without using the sort ().

exercise6b = function()
{
  A =c(2,14,8,9)
  
  #find minimum number WORKING
  min_num = A[1]
  for (w in 2:length(A))
  {
    if (A[w] < min_num)
    {
      min_num = A[w]
    }
  }
  
  #find max number WORKING 
  max_num = A[1]
  for (q in 2:length(A))
  {
    if (A[q] > max_num)
    {
      max_num = A[q]
    }
  }
  
  #check if num is in vector 
  RUNNING = TRUE
  while (RUNNING == TRUE)
  {
    A = sort(A)
    print(A)
    cat("Enter a number between ", min_num, " and ",max_num)
    num = scan(,,1)
    signal = 0
    for (e in 1:length(A))
    {
      if (num == A[e])
      {
        signal = 1
      }
    }
    if(signal == 0)
    {
      A = c(A,num)
    }
    else if (signal == 1)
    {
      RUNNING = FALSE 
      cat("Finished vector is:")
      print (A)
    }
  }
}




#8a. WORKING AND CHECKED Create a "Hit" function. The function must create a vector, for example A =(2,3,6,1,7,1). Next
#you must create a matrix with “*”, as many numbers as in the vector A. The function must
#request numbers (between 1 and 10) from the user by keyboard. It must check if that number
#exists in the vector, if it does exists you must put it in the place of the number an "*" and insert
#the number in the matrix.


exercise8a = function ()
{
  A = c(2, 3, 6, 1, 7, 1)
  mymat = matrix(c(A), nrow = 3, byrow = TRUE)
  hits = 0
  target = length(mymat)
  turn_max = length(mymat)
  turn_count = 0
  RUNNING = TRUE  
  while (turn_count< turn_max & hits < target)
  {
    cat("-----\n")
    cat(turn_max-turn_count,"turns remaining\n")
    cat((target-hits),"targets remaining\n")
    print ("Insert a number between 1 and 10")
    num = scan (, , 1)
    for (i in 1:nrow(mymat))
    {
      for (j in 1:ncol(mymat))
      {
        if (num == mymat[i, j])
        {
          print ("target hit")
          mymat[i, j] = "*" 
          hits = hits + 1
        }
      }
    }
    turn_count = turn_count+1
  }
  print (mymat)
  if (hits == target)
  {
    print ("All targets hit. Game over")
  }
  else
  {
    cat("Turns exceeded vector length\nYou have failed.\nGame over")
  }
}



#8b. WORKING AND CHECKED Create a "Hit" function. The function must create a vector, for example A =(2,3,6,1,7,1). Next
#you must create a matrix with “*”, as many numbers as in the vector A. The function must
#request numbers (between 1 and 10) from the user by keyboard. It must check if that number
#exists in the vector, if it does exists you must put it in the place of the number an "*" and insert
#the number in the matrix.


exercise8b = function ()
{
  A = c(2, 3, 6, 1, 7, 1)
  hits = 0
  target = length(mymat)
  mymat = matrix(c(A), nrow = 3, byrow = TRUE)
  RUNNING = TRUE  
  while (RUNNING == TRUE & hits < target)
  {
    cat((target-hits)," targets remaining\n")
    print ("Insert a num between 1 and 10")
    num = scan (, , 1)
    for (i in 1:nrow(mymat))
    {
      for (j in 1:ncol(mymat))
      {
        if (num == mymat[i, j])
        {
          print ("target hit")
          mymat[i, j] = "*" 
          hits = hits + 1
        }
      }
    }
  }
  print (mymat)
  print ("All targets hit. Game over")
}



#10. WORKING AND CHECKED Write a function in order to add two vectors. If the result is over nine you must use carry
#arithmetic. You must use a matrix to place the vectors and the result.

exercise10 = function()
{
  A = c(1,7,6,9)
  B = c(4,8,7)
  sum_vec = c(0,0,0,0,0,0,0,0,0,0,0,0,0)
  active_num = c(0)
  carry_num= c(0)
  
  #make them the same length
  while (length(sum_vec)>length(B))
  {
    B = c(0,B)
  }
  while (length(sum_vec)>length(A))
  {
    A = c(0,A)
  }
  cat (A,"\n")
  cat (B,"\n")
  for (i in length(sum_vec):1)
  {
    active_num = A[i] + B[i]
    sum_vec[i] = sum_vec[i] + active_num
    if(sum_vec[i] > 9)
    {
      sum_vec[i] = sum_vec[i] - 10
      sum_vec[i-1] = sum_vec[i-1] + 1
    }
  }
  
  while (sum_vec[1]==0)
  {
    sum_vec = c(sum_vec[-1])
  }
  cat("-----\n")
  cat (sum_vec)
}







