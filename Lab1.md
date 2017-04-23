1. Створити змінні базових (atomic) типів. Базові типи: character, numeric, integer, complex, logical.

```r
t1 <-"hello world"
t2 <- 8.5
t3 <- 5L
t4 <- TRUE
t5 <- 1+1i
```

2. Створити вектори, які: містить послідовність з 5 до 75; містить числа 3.14, 2.71, 0, 13; 100 значень TRUE.

```r
x1<-5:75
x1
 [1]  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
[28] 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58
[55] 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75

x2<-c(3.14,2.71,0,13)
[1]  3.14  2.71  0.00 13.00

x3<-rep(TRUE, times = 100)
 [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [17] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [33] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [49] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [65] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [81] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [97] TRUE TRUE TRUE TRUE
```

3. Створити наступну матрицю за допомогою matrix, та за допомогою cbind

```r
x1<-c(0.5,3.9,0,2)
x2<-c(1.3,131,2.2,7)
x3<-c(3.5,2.8,4.6,5.1)
cbind(x1,x2,x3)

      x1    x2  x3
[1,] 0.5   1.3 3.5
[2,] 3.9 131.0 2.8
[3,] 0.0   2.2 4.6
[4,] 2.0   7.0 5.1
```

4. Створити довільний список (list), в який включити всі базові типи.

```r
l <-list(1.1,1L,"hello",TRUE)

[[1]]
[1] 1.1

[[2]]
[1] 1

[[3]]
[1] "hello"

[[4]]
[1] TRUE
```

5. Створити фактор з трьома рівнями «baby», «child», «adult».

```r
x<-c("baby", "baby", "baby", "child", "child", "adult")
factor(x)

[1] baby  child adult
Levels: adult baby child
```

6. Знайти індекс першого значення NA в векторі 1, 2, 3, 4, NA, 6, 7, NA, 9, NA,11. 
Знайти кількість значень NA.

```r
x<-c(1:4,NA,6,7,NA,9,NA,11)
match(NA,x)
[1] 5

sum(is.na(x))
[1] 3
```

7. Створити довільний data frame та вивести в консоль.

```r
x<-data.frame(c1=c("Ivan","Vasyl","Olena"),c2=c("Ivanov","Petrov","Sidorova"))
     c1       c2
1  Ivan   Ivanov
2 Vasyl   Petrov
3 Olena Sidorova
```
8. Змінити імена стовпців цього data frame.

```r
names(x)<-c("name","surname")

  name  surname
1  Ivan   Ivanov
2 Vasyl   Petrov
3 Olena Sidorova
```


