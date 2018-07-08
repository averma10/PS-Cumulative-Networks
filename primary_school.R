
#-----C1------
library(intergraph)
library(mcclust)
library(GGally)
library(network)
library(latticeExtra)
library(dplyr)
library(ergm)
library(sand)
library(ggplot2)

#set default theme for all plots
theme_set(theme_classic()) #set theme for plots

#----- C2-----
path <- "/Users/Gurpreet/Documents/Coding practices/R"
setwd(path)
ps1 <- read.graph("sp_data_school_day_1.graphml", format = "graphml")
ps2 <- read.graph("sp_data_school_day_2.graphml", format = "graphml")

#------C3------
summary(ps1)
summary(ps2)

#------C4------
#ps1
#create grades 1,2,3,4,5 for students and grade 6 for teachers 
classtable <- data.frame(section = c('1A','1B','2A','2B','3A','3B','4A','4B','5A','5B','Teachers'),
                         grade = c(1,1,2,2,3,3,4,4,5,5,6))
ps1.df <- data.frame(id = V(ps1)$id,section=V(ps1)$classname)

#inner join
ps1.df <- inner_join(ps1.df,classtable, by = 'section')
#create a vertex attribute 'grade' for graph
ps1 <- set_vertex_attr(ps1,'grade',index = V(ps1),value = ps1.df$grade)

summary(ps1)


#ps2
#create grades 1,2,3,4,5 for students and grade 6 for teachers 
ps2.df <- data.frame(id = V(ps2)$id,section=V(ps2)$classname)

#inner join
ps2.df <- inner_join(ps2.df,classtable, by = 'section')

#create a vertex attribute 'grade' for graph
ps2 <- set_vertex_attr(ps2,'grade',index = V(ps2),value = ps2.df$grade)

summary(ps2)

#--------C5--------------
#Distribution for contact duration
#day 1
duration1 <- as.integer((E(ps1)$duration)/60)

t1 <- tabulate(duration1)
t1.df <- data.frame(duration = seq(1,length(t1)),count = t1, Day='1')
t1.df <- t1.df[t1.df$count!=0,]

#day 2
duration2 <- as.integer((E(ps2)$duration)/60)

t2 <- tabulate(duration2)
t2.df <- data.frame(duration = seq(1,length(t2)),count=t2, Day = '2')
t2.df <- t2.df[t2.df$count!=0,]

t.df <- rbind(t1.df,t2.df)

p <- ggplot(data=t.df,aes(x=duration, y=count,color = Day))
p <- p + geom_point(aes(shape = Day)) + geom_vline(xintercept = 2, linetype = 'dotted', color='blue') 
p <- p + geom_density_2d(alpha = 0.5)
p <- p + scale_x_log10('Contact Duration (minutes)') + scale_y_log10() + theme_gray() 
p <- p + ggtitle('Face-Face Contact duration of students', subtitle = 'Log-Log plot')
plot(p)

#------------C6-----------
#Distribution for number of contacts in a Day
contact1 <- E(ps1)$count

t1 <- tabulate(contact1)
t1.df <- data.frame(contact = seq(1,length(t1)),count = t1, Day='1')
t1.df <- t1.df[t1.df$count!=0,]

#day 2
contact2 <- E(ps2)$count

t2 <- tabulate(contact2)
t2.df <- data.frame(contact = seq(1,length(t2)),count=t2, Day = '2')
t2.df <- t2.df[t2.df$count!=0,]

t.df <- rbind(t1.df,t2.df)

p <- ggplot(data=t.df,aes(x=contact, y=count,color = Day))
p <- p + geom_point(aes(shape=Day)) + geom_vline(xintercept = 2, linetype = 'dotted', color='blue') 
p <- p + geom_density_2d(alpha = 0.4)
p <- p + scale_x_log10('Number of Contacts') + scale_y_log10() + theme_gray()
p <- p + ggtitle('Number of Contacts made by students',subtitle = 'Log-Log plot')
plot(p)



#Filter the graph ps1
#--------C7-----------
#Delete edges with contact duration less than 2mins/120sec
ps1a <- delete_edges(ps1,E(ps1)[E(ps1)$duration <= 120])

#Delete edges with number of contacts less than 2
ps1b <- delete_edges(ps1a,E(ps1a)[E(ps1a)$count <=2 ])

#make contact duration as new edge weight
E(ps1b)$weight <- E(ps1b)$duration

#remove Teachers from our network
day1 <- delete_vertices(ps1b,V(ps1b)[V(ps1b)$classname =='Teachers'])
day1 <- delete_vertices(day1,V(day1)[V(day1)$gender =='Unknown'])

summary(day1) #final graph


#Filter the graph ps2
#Delete edges with contact duration less than 2mins/120sec
ps2a <- delete_edges(ps2,E(ps2)[E(ps2)$duration <= 120])

#Delete edges with number of contacts less than 2
ps2b <- delete_edges(ps2a,E(ps2a)[E(ps2a)$count <=2 ])

#make contact duration as new edge weight
E(ps2b)$weight <- E(ps2b)$duration

#remove Teachers from our network
day2 <- delete_vertices(ps2b,V(ps2b)[V(ps2b)$classname =='Teachers'])
day2 <- delete_vertices(day2,V(day2)[V(day2)$gender =='Unknown'])
summary(day2) #final graph


#--------C8---------
#save graphs
write_graph(day1,'day1_filtered.graphml',format = 'graphml')
write_graph(day1,'day2_filtered.graphml',format = 'graphml')

#--------C9-------
#Calculate assortativity by grades and plot it. day1 vs day2
#extract different grade networks
#day-1
g1 <- which(V(day1)$grade == 1)
day1.g1 <- induced_subgraph(day1,g1)

g2 <- which(V(day1)$grade == 2)
day1.g2 <- induced_subgraph(day1,g2)

g3 <- which(V(day1)$grade == 3)
day1.g3 <- induced_subgraph(day1,g3)

g4 <- which(V(day1)$grade == 4)
day1.g4 <- induced_subgraph(day1,g4)

g5 <- which(V(day1)$grade == 5)
day1.g5 <- induced_subgraph(day1,g5)

#day-2
g1 <- which(V(day2)$grade == 1)
day2.g1 <- induced_subgraph(day2,g1)

g2 <- which(V(day2)$grade == 2)
day2.g2 <- induced_subgraph(day2,g2)

g3 <- which(V(day2)$grade == 3)
day2.g3 <- induced_subgraph(day2,g3)

g4 <- which(V(day2)$grade == 4)
day2.g4 <- induced_subgraph(day2,g4)

g5 <- which(V(day2)$grade == 5)
day2.g5 <- induced_subgraph(day2,g5)

#assortativity by class sections
day1.assor_sect <- c(assortativity_nominal(day1.g1,types = factor(V(day1.g1)$classname)),
                      assortativity_nominal(day1.g2,types = factor(V(day1.g2)$classname)),
                      assortativity_nominal(day1.g3,types = factor(V(day1.g3)$classname)),
                      assortativity_nominal(day1.g4,types = factor(V(day1.g4)$classname)),
                      assortativity_nominal(day1.g5,types = factor(V(day1.g5)$classname)))

day2.assor_sect <- c(assortativity_nominal(day2.g1,types = factor(V(day2.g1)$classname)),
                      assortativity_nominal(day2.g2,types = factor(V(day2.g2)$classname)),
                      assortativity_nominal(day2.g3,types = factor(V(day2.g3)$classname)),
                      assortativity_nominal(day2.g4,types = factor(V(day2.g4)$classname)),
                      assortativity_nominal(day2.g5,types = factor(V(day2.g5)$classname)))

assor_sect.dist1 <- data.frame(grade = c('1st','2nd','3rd','4th','5th'),
                                assortativity= day1.assor_sect, Day='Day 1')
assor_sect.dist2 <- data.frame(grade = c('1st','2nd','3rd','4th','5th'),
                                assortativity= day2.assor_sect, Day='Day 2')

assor_sect.dist <- rbind(assor_sect.dist1,assor_sect.dist2)
#plot
p <- ggplot(data = assor_sect.dist, aes(x = grade,y=assortativity,fill=Day)) +
      geom_bar(stat='identity',position = 'dodge')+
      ggtitle('Assortativity within individual grades')
plot(p)

#---------C10----------
#Inter-grade assortativity      
#extract subgraphs from day1
day1.gr12 <- induced_subgraph(day1,which(V(day1)$grade == 1 | V(day1)$grade ==2))
day1.gr13 <- induced_subgraph(day1,which(V(day1)$grade == 1 | V(day1)$grade ==3))
day1.gr14 <- induced_subgraph(day1,which(V(day1)$grade == 1 | V(day1)$grade ==4))
day1.gr15 <- induced_subgraph(day1,which(V(day1)$grade == 1 | V(day1)$grade ==5))
day1.gr23 <- induced_subgraph(day1,which(V(day1)$grade == 2 | V(day1)$grade ==3))
day1.gr24 <- induced_subgraph(day1,which(V(day1)$grade == 2 | V(day1)$grade ==4))
day1.gr25 <- induced_subgraph(day1,which(V(day1)$grade == 2 | V(day1)$grade ==5))
day1.gr34 <- induced_subgraph(day1,which(V(day1)$grade == 3 | V(day1)$grade ==4))
day1.gr35 <- induced_subgraph(day1,which(V(day1)$grade == 3 | V(day1)$grade ==5))
day1.gr45 <- induced_subgraph(day1,which(V(day1)$grade == 4 | V(day1)$grade ==5))

#extract subgraphs from day2
day2.gr12 <- induced_subgraph(day2,which(V(day2)$grade == 1 | V(day2)$grade ==2))
day2.gr13 <- induced_subgraph(day2,which(V(day2)$grade == 1 | V(day2)$grade ==3))
day2.gr14 <- induced_subgraph(day2,which(V(day2)$grade == 1 | V(day2)$grade ==4))
day2.gr15 <- induced_subgraph(day2,which(V(day2)$grade == 1 | V(day2)$grade ==5))
day2.gr23 <- induced_subgraph(day2,which(V(day2)$grade == 2 | V(day2)$grade ==3))
day2.gr24 <- induced_subgraph(day2,which(V(day2)$grade == 2 | V(day2)$grade ==4))
day2.gr25 <- induced_subgraph(day2,which(V(day2)$grade == 2 | V(day2)$grade ==5))
day2.gr34 <- induced_subgraph(day2,which(V(day2)$grade == 3 | V(day2)$grade ==4))
day2.gr35 <- induced_subgraph(day2,which(V(day2)$grade == 3 | V(day2)$grade ==5))
day2.gr45 <- induced_subgraph(day2,which(V(day2)$grade == 4 | V(day2)$grade ==5))

#assortativity by grades
day1.assor_grade <- c(assortativity_nominal(day1.gr12,types = factor(V(day1.gr12)$grade)),
                      assortativity_nominal(day1.gr13,types = factor(V(day1.gr13)$grade)),
                      assortativity_nominal(day1.gr14,types = factor(V(day1.gr14)$grade)),
                      assortativity_nominal(day1.gr15,types = factor(V(day1.gr15)$grade)),
                      assortativity_nominal(day1.gr23,types = factor(V(day1.gr23)$grade)),
                      assortativity_nominal(day1.gr24,types = factor(V(day1.gr24)$grade)),
                      assortativity_nominal(day1.gr25,types = factor(V(day1.gr25)$grade)),
                      assortativity_nominal(day1.gr34,types = factor(V(day1.gr34)$grade)),
                      assortativity_nominal(day1.gr35,types = factor(V(day1.gr35)$grade)),
                      assortativity_nominal(day1.gr45,types = factor(V(day1.gr45)$grade)))

day2.assor_grade <- c(assortativity_nominal(day2.gr12,types = factor(V(day2.gr12)$grade)),
                      assortativity_nominal(day2.gr13,types = factor(V(day2.gr13)$grade)),
                      assortativity_nominal(day2.gr14,types = factor(V(day2.gr14)$grade)),
                      assortativity_nominal(day2.gr15,types = factor(V(day2.gr15)$grade)),
                      assortativity_nominal(day2.gr23,types = factor(V(day2.gr23)$grade)),
                      assortativity_nominal(day2.gr24,types = factor(V(day2.gr24)$grade)),
                      assortativity_nominal(day2.gr25,types = factor(V(day2.gr25)$grade)),
                      assortativity_nominal(day2.gr34,types = factor(V(day2.gr34)$grade)),
                      assortativity_nominal(day2.gr35,types = factor(V(day2.gr35)$grade)),
                      assortativity_nominal(day2.gr45,types = factor(V(day2.gr45)$grade)))

assor_grade.dist1 <- data.frame(grade = c('1st-2nd','1st-3rd','1st-4th','1st-5th','2nd-3rd','2nd-4th','2nd-5th','3rd-4th','3rd-5th','4th-5th'),
                               assortativity= day1.assor_grade, Day='Day 1')
assor_grade.dist2 <- data.frame(grade = c('1st-2nd','1st-3rd','1st-4th','1st-5th','2nd-3rd','2nd-4th','2nd-5th','3rd-4th','3rd-5th','4th-5th'),
                               assortativity= day2.assor_sect, Day='Day 2')

assor_grade.dist <- rbind(assor_grade.dist1,assor_grade.dist2)
#plot
p <- ggplot(data = assor_grade.dist, aes(x = grade,y=assortativity,fill=Day)) +
  geom_bar(stat='identity',position = 'dodge')+
  ggtitle('Assortativity in different grades')
plot(p)

#---------C11a----------
#set seed value
set.seed(111089)
#CUG Test of assortativity by gender
source('mycugtest.r')
day1.cug_gendr <- mycugtest(day1,assortativity_nominal,cmode = 'edges',
                            directed = F, types = factor(V(day1)$gender))
print.cug.test(day1.cug_gendr)
plot.cug.test(day1.cug_gendr)

#----------C11b-------
#CUG Test of assortativity by grade
day1.cug_grade <- mycugtest(day1,assortativity_nominal,cmode = 'edges',
                            directed = F,types=factor(V(day1)$grade))
print.cug.test(day1.cug_grade)
plot.cug.test(day1.cug_grade)

#---------C12a-------
#QAP test
source('myqaptest.r')
day1.qap_gender <- myqaptest(day1,assortativity_nominal, directed=F,
                             types = factor(V(day1)$gender))
summary.qaptest(day1.qap_gender)
plot.qaptest(day1.qap_gender)

#--------C12b-------
day1.qap_grade <- myqaptest(day1,assortativity_nominal, directed=F,
                             types = factor(V(day1)$grade))
summary.qaptest(day1.qap_grade)
plot.qaptest(day1.qap_grade)

#------C13-------
#ergm models

#First try on very simple network
#grade1-day1
gr1 <- asNetwork(day1.g1)
#grade 1 - day 2
gr1.d2 <- asNetwork(day2.g1)


#------C14save-------
#All Models
#classname
gr1.m1 <- ergm(gr1~edges + nodemix('classname',base=3))
gr1.m1a <- ergm(gr1~edges + gwesp) #failed
gr1.m2 <- ergm(gr1~edges + nodemix('classname',base=3) + degree(10)) #good model
gr1.m2a <- ergm(gr1~edges + nodemix('classname',base = 3) + degree(10) + degree(12)) #good model
gr1.m2b <- ergm(gr1~edges + nodemix('classname',base = 3) + degree(10),
                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048))
gr1.m3 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'gender'))
gr1.m3a <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'gender'),
                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048)) #Good model
gr1.m4 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'classname')) #failed
gr1.m5 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10) + gwesp) #see later

#gender
gr1.m6 <- ergm(gr1~edges + nodemix('gender', base=c(3)))
gr1.m7 <- ergm(gr1~edges + nodemix('gender', base=c(1,3))+ degree(10,'gender'))
gr1.m8 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(cutoff=20)) #failed
gr1.m9 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(3:7)) #failed
gr1.m10 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(12)) #failed
gr1.m11 <- ergm(gr1~edges + nodemix('gender', base = c(1,3))+ degree(10) + concurrent('gender')) #failed
gr1.m12 <- ergm(gr1~edges + sociality('gender',base=1)) #too many coefficients

#both
gr1.m13 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)))
gr1.m14 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)) + 
                  degree(10,'gender')) #good model
gr1.m15 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)) + degree(10,'gender'),
                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048)) #Failed

gr1.m16 <- ergm(gr1~edges + nodemix('classname',base=c(3))+ nodematch('gender',diff=T) + degree(10))

gr1.m16a <- ergm(gr1~edges + nodemix('classname',base=c(3))+ nodematch('gender',diff=T) + degree(10),
                 control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 15000,MCMC.samplesize = 2048))

#save outputs
save(gr1.m1,gr1.m2,gr1.m2a,gr1.m2b,gr1.m3,gr1.m3a,
     gr1.m5,gr1.m6,gr1.m7,gr1.m13,gr1.m14,gr1.m16,gr1.m16a,file = 'grade1-day1-models.Rdata')

#------C14load--------
#Different models
#gr1.m1 <- ergm(gr1~edges + nodemix('classname',base=3))
#gr1.m1a <- ergm(gr1~edges + gwesp) #failed
#gr1.m2 <- ergm(gr1~edges + nodemix('classname',base=3) + degree(10)) #good model
#gr1.m2a <- ergm(gr1~edges + nodemix('classname',base = 3) + degree(10) + degree(12)) #good model
#gr1.m2b <- ergm(gr1~edges + nodemix('classname',base = 3) + degree(10),
#                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048))
#gr1.m3 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'gender'))
#gr1.m3a <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'gender'),
#                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048)) #Good model
#gr1.m4 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'classname')) #failed
#gr1.m5 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10) + gwesp) #see later

#gender
#gr1.m6 <- ergm(gr1~edges + nodemix('gender', base=c(3)))
#gr1.m7 <- ergm(gr1~edges + nodemix('gender', base=c(1,3))+ degree(10,'gender'))
#gr1.m8 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(cutoff=20)) #failed
#gr1.m9 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(3:7)) #failed
#gr1.m10 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(12)) #failed
#gr1.m11 <- ergm(gr1~edges + nodemix('gender', base = c(1,3))+ degree(10) + concurrent('gender')) #failed
#gr1.m12 <- ergm(gr1~edges + sociality('gender',base=1)) #too many coefficients

#both
#gr1.m13 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)))
#gr1.m14 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)) + 
#                  degree(10,'gender')) #good model
#gr1.m15 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)) + degree(10,'gender'),
#                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048)) #Failed

#gr1.m16 <- ergm(gr1~edges + nodemix('classname',base=c(3))+ nodematch('gender',diff=T) + degree(10))
#gr1.m16a <- ergm(gr1~edges + nodemix('classname',base=c(3))+ nodematch('gender',diff=T) + degree(10),
#                 control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 15000,MCMC.samplesize = 2048))

load('grade1-day1-models.Rdata')

#---------C15--------
#Model1
#gr1.m1 <- ergm(gr1~edges + nodemix('classname',base=3))
summary(gr1.m1)
#-------C15a----
gr1.m1.gof <- gof(gr1.m1)
plot(gr1.m1.gof)

#---------C16------
#Model2
#gr1.m2 <- ergm(gr1~edges + nodemix('classname',base=3) + degree(10))
summary(gr1.m2)

#------C16a-------
gr1.m2.gof <- gof(gr1.m2)
plot(gr1.m2.gof)
#-------C16b--------
mcmc.diagnostics(gr1.m2,center = F)

#-----C17----
#M2b
#gr1.m2b <- ergm(gr1~edges + nodemix('classname',base = 3) + degree(10),
#                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048))
summary(gr1.m2b)
#------C17a----
gr1.m2b.gof <- gof(gr1.m2b)
plot(gr1.m2b.gof)

#-------C17b------
mcmc.diagnostics(gr1.m2b,center = F)

#-----------C18------
#M3
#gr1.m3 <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'gender'))
summary(gr1.m3)
#------C18a---------
plot(gof(gr1.m3))
#-----C18b------
mcmc.diagnostics(gr1.m3,center = F)


#--------C19------
#M3a
#gr1.m3a <- ergm(gr1~edges + nodemix('classname',base = c(1,3)) + degree(10,'gender'),
#                control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 10000,MCMC.samplesize = 2048))
summary(gr1.m3a)

#-------C19a--------
plot(gof(gr1.m3a))

#-------C19b---------
mcmc.diagnostics(gr1.m3a,center = F)

#-----C20------
#M6
#gr1.m6 <- ergm(gr1~edges + nodemix('gender', base=c(3)))
summary(gr1.m6)
#-----C20a-----
plot(gof(gr1.m6))

#--------C21-----
#M7
#gr1.m7 <- ergm(gr1~edges + nodemix('gender', base=c(1,3))+ degree(10,'gender'))
summary(gr1.m7)
#-------C21a--------
plot(gof(gr1.m7))
#---------C21b-----
mcmc.diagnostics(gr1.m7)

#------C22------
#gr1.m8 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(cutoff=20)) #failed
#gr1.m9 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(3:7)) #failed
#gr1.m10 <- ergm(gr1~edges + nodemix('gender',base=c(1,3)) + degree(10,'gender') + gwesp(12)) #failed
#gr1.m11 <- ergm(gr1~edges + nodemix('gender', base = c(1,3))+ degree(10) + concurrent('gender')) #failed
#gr1.m12 <- ergm(gr1~edges + sociality('gender',base=1)) #too many coefficients

#------C23-----
#M14
#gr1.m14 <- ergm(gr1~edges + nodemix('gender',base = c(1,3)) + nodemix('classname',base = c(1,3)) + 
#                  degree(10,'gender'))
summary(gr1.m14)
#------C23a-----
plot(gof(gr1.m14))
#------C23b-----
mcmc.diagnostics(gr1.m14,center = F)

#-------C24------
#M16
#gr1.m16 <- ergm(gr1~edges + nodemix('classname',base=c(1,3))+ nodematch('gender',diff=T) + degree(10))
summary(gr1.m16)
#------C24a-------
plot(gof(gr1.m16))
#------C24b-------
mcmc.diagnostics(gr1.m16,center = F)

#-----C25------
#M16a
#gr1.m16a <- ergm(gr1~edges + nodemix('classname',base=c(3))+ nodematch('gender',diff=T) + degree(10),
#                 control = control.ergm(MCMC.burnin = 100000,MCMC.interval = 15000,MCMC.samplesize = 2048))
summary(gr1.m16a)
#-----C25a------
plot(gof(gr1.m16a))

#-----C25b------
mcmc.diagnostics(gr1.m16a,center = F)


#--------C26save-----
gr1.d2.m16 <- ergm(gr1.d2~edges + nodemix('classname',base=c(3))+ 
                     nodematch('gender',diff=T) + degree(10))

gr1.d2.m16a <- ergm(gr1.d2~edges + nodemix('classname',base=c(3))+ 
                     nodematch('gender',diff=T) + degree(10),
                    control = control.ergm(MCMC.burnin = 150000,MCMC.interval = 25000))

save(gr1.d2.m16,gr1.d2.m16a,file='grade1-day2-models.Rdata')

#-------C26load------

load('grade1-day2-models.Rdata')

#-------C27-------
#M16a
#gr1.d2.m16a <- ergm(gr1.d2~edges + nodemix('classname',base=c(3))+ 
#                      nodematch('gender',diff=T) + degree(10),
#                    control = control.ergm(MCMC.burnin = 150000,MCMC.interval = 25000))
summary(gr1.d2.m16a)
#------C27a--------
plot(gof(gr1.d2.m16a))
#------C27b--------
mcmc.diagnostics(gr1.d2.m16a,center=F)


#--------C28------
#Visualize the models for both days
source('project-utils.R')
#visualization for grade1-day1 model
par(mfrow=c(1,2))
gaplot(asIgraph(simulate(gr1.m16)),names = F)
#visualization for grade1-day2 model
gaplot(asIgraph(simulate(gr1.d2.m16)),names = F)
par(mfrow=c(1,1))


#----------C29-------

#Calculate probability of F-F edge vs M-M edge
cases <- c( 'F-F edge','M-M edge')
edges <- c(1)
FF <- c(1,0)
MM <- c(0,1)
cases.df1 <- data.frame(Case = cases,edges=edges,FF,MM,Day = 'day1')

cases.df1$logodds <- gr1.m16$coef[1]*cases.df1$edges + gr1.m16$coef[4] * cases.df1$FF + gr1.m16$coef[5]*cases.df1$MM
cases.df1$cond_prob <- invlogit(cases.df1$logodds)

cases.df2 <- data.frame(Case = cases, edges,FF,MM,Day='day2')

cases.df2$logodds <- gr1.d2.m16$coef[1]*cases.df2$edges + gr1.d2.m16$coef[4] * cases.df2$FF + gr1.d2.m16$coef[5]*cases.df2$MM
cases.df2$cond_prob <- invlogit(cases.df2$logodds)

cases.df <- rbind(cases.df1,cases.df2)

cases.df[order(cases.df$cond_prob),]

#Comparison of edge conditional probability for day 1-2
g  <- ggplot(data = cases.df, aes(x=Case,y=cond_prob,fill=Day))+
  geom_bar(stat = 'identity',position = 'dodge') +
  scale_x_discrete('Cases') + scale_y_continuous('Edge existance probability') +
  ggtitle('Edge probability Comparison', subtitle = 'Grade 1 only')
plot(g)
