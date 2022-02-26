# 1. VDD - WebApp

- [1. VDD - WebApp](#1-vdd---webapp)
  - [1.1. usefull links](#11-usefull-links)
  - [1.2. Code](#12-code)
    - [1.2.1. Read wine data](#121-read-wine-data)
    - [1.2.2. Read Student data](#122-read-student-data)

## 1.1. usefull links
- [Arche page](https://arche.univ-lorraine.fr/course/view.php?id=47500)
- [Shiny-Dashboard](https://archive.ics.uci.edu/ml/index.php)
- [Student Data](https://archive.ics.uci.edu/ml/datasets/Student+Performance)

## 1.2. Code

### 1.2.1. Read wine data
```R
wine = read.table( "data/wine/wine.data",sep=";",header=T)
winequality = read.table( "data/winequality/winequality-red.csv",sep=";",header=T)
summary(wine)
```

### 1.2.2. Read Student data
```R
mat=read.table("data/student/student-mat.csv",sep=";",header=TRUE)
por=read.table("data/student/student-por.csv",sep=";",header=TRUE)

student=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
summary(student)
```


