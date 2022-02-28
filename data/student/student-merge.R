mat=read.table("data/student/student-mat.csv",sep=";",header=TRUE)
por=read.table("data/student/student-por.csv",sep=";",header=TRUE)
student=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

print(nrow(student)) # 382 students




