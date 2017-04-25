Lab5

1. Написати функцію pmean, яка обчислює середнє значення (mean) забруднення сульфатами або нітратами серед заданого переліка моніторів. Ця функція приймає три аргументи: «directory», «pollutant», «id». Directory – папка, в якій розміщені дані, pollutant – вид забруднення, id – перелік моніторів. Аргумент id має значення за замовчуванням 1:332. Функція повинна ігнорувати NA значення. Приклад роботи функції:


	```r
	d <- "D:\\specdata"
	
	pmean<-function(directory, pollutant, id){
	  all_pollutants <- list()
	  
	  for(i in id) {
	    path <- sprintf("%s\\%03d.csv", directory, i)
	    df <- read.csv(path)
	    all_pollutants <- append(all_pollutants, df[pollutant])
	  }
	  
	  mean(unlist(all_pollutants), na.rm = TRUE)
	}
	
	pmean(d, "sulfate", 1:10)
	[1] 4.064128
	```
2. Написати функцію complete, яка виводить кількість повних спостережень (the number of completely observed cases) для кожного файлу. Функція приймає два аргументи: «Directory» та «id» та повертає data frame, в якому перший стовпчик – ім’я файлу, а другий – кількість повних спостережень. Приклад роботи функції:


	```r
	complete<-function(directory, id){
	  observed_cases<-c()
	  for(i in id) {
	    path <- sprintf("%s\\%03d.csv", directory, i)
	    df <- read.csv(path)
	    observed_cases<-append(observed_cases, nrow(subset(df, complete.cases(df))))
	  }
	  data.frame(id, observed_cases)
	}
	
	complete(d, 1)
	  id observed_cases
	1  1            117
	> complete(d, c(2, 4, 8, 10, 12))
	  id observed_cases
	1  2           1041
	2  4            474
	3  8            192
	4 10            148
	5 12             96
	```
3. Написати функцію corr, яка приймає два аргументи: directory (папка, де знаходяться файли спостережень) та threshold (порогове значення, за замовчуванням дорівнює 0) та обчислює кореляцію між сульфатами та нітратами для моніторів, кількість повних спостережень для яких більше порогового значення. Функція повинна повернути вектор значень кореляцій. Якщо ні один монітор не перевищує порогового значення, функція повинна повернути numeric вектор довжиною 0. Для обчислення кореляції між сульфатами та нітратами використовуйте вбудовану функцію «cor» з параметрами за замовчуванням.

	```r
	corr<-function(directory, threshold = 0) {
	    dfcases <- complete(directory)
	    ids <- subset(dfcases, dfcases$observed_cases > threshold)$id
	    
	    result <- c()
	    for(i in ids) {
	      path <- sprintf("%s\\%03d.csv", directory, i)
	      df <- read.csv(path)
	      df <- subset(df, complete.cases(df))
	      result <- append(result, cor(df$nitrate, df$sulfate))
	    }
	    unlist(result)
	}
	
	cr <- corr(d, 150)
	> head(cr); summary(cr)
	[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
	    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
	-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
	```

