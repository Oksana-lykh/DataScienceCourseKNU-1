В лабораторній роботі необхідно написати наступні функції на мові R та вивести результат роботи цих функцій на довільних даних:

```r
x<-sample(1:20,10)
17  1  7  9 18 14  6 11  5 15
y<-sample(1:20,10)
 y
 [1]  8 18 14  9 12 10  7 20 16 17
```

1. Функція add2(x, y), яка повертає суму двох чисел.

	```r
	add2<-function(x,y)
	  { x+y}
	  
	  add2(x,y)
	 [1] 25 19 21 18 30 24 13 31 21 32
	```
2. Функція above(x, n), яка приймає вектор та число n, та повертає всі елементі вектору, які більше n. По замовчуванню n = 10.

	```r
	 above<-function(x, n=10)
	  { x[x>n]}
	  
	  above(x,3)
	[1] 17  7  9 18 14  6 11  5 15
	
	above(x)
	[1] 17 18 14 11 15
	```
3. Функція my_ifelse(x, exp, n), яка приймає вектор x, порівнює всі його елементи за допомогою exp з n, та повертає елементи вектору, 
які відповідають умові expression. Наприклад, my_ifelse(x, “>”, 0) повертає всі елементи x, які більші 0. Exp може дорівнювати “<”, “>”,
“<=”, “>=”, “==”.

	```r
	my_ifelse<-function(x,exp,n){
	  
	  
	  if (exp == "<"){
	    return(x[x<n])
	  }
	  else if (exp==">"){
	    return(x[x>n])
	  }
	  else if (exp=="<="){
	    return(x[x<=n])
	  }
	  else if (exp==">="){
	    return(x[x>=n])
	  }
	  else if (exp=="=="){
	    return(x[x==n])
	  }
	  else x
	}
	
	my_ifelse(x,"<",10)
	[1] 4 8 9 7 2
	
	my_ifelse(x,">",3)
	[1]  4  8 11  9  7 20 19 14 10
	```
4. Функція columnmean(x, removeNA), яка розраховує середнє значення (mean) по кожному стовпцю матриці, або data frame. Логічний параметр
removeNA вказує, чи видаляти NA значення. По замовчуванню він дорівнює TRUE.

	```r
	columnmean<-function(x,removeNA=TRUE){
	  apply(x,2,mean,na.rm=removeNA)
	}
	
	columnmean(df)
	   x    y 
	2.75 6.50 
	> columnmean(df, FALSE)
	 x  y 
	NA NA 
	> 
	```

