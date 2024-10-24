


third = c("Cameron Payne","Terquavion Smith","Jordan Nwora", "Nicolas Batum", "")
bench = c("Markelle Fultz", "Gary Trent", "Kelly Oubre", "Trendon Watford", "Andre Drummond")
starters = c("Tyrese Maxey", "Ricky Council", "Alex Caruso", "Trendon Watford", "Joel Embiid")


roster.stats.guesses <- function(starter,bench,third){

  library(stringr)
  library(tidyr)
  
  nba2024 = read.csv("C:\\Users\\DavetteUser\\Documents\\nba stuff\\archive(5)\\2023-2024 NBA Player Stats - Regular.csv", sep = ";")
  nba2024
  
  nba2023 = read.csv("C:\\Users\\DavetteUser\\Documents\\nba stuff\\archive(5) 2023\\2022-2023 NBA Player Stats - Regular.csv", sep = ";")
  nba2023
  
  nba2024permin = nba2024
  
  nba2024permin = nba2024permin[!duplicated(nba2024permin[2]), ]
  
  for (i in 1:length(nba2024permin$Rk)){
    a=nba2024permin[i,c("FG","FGA","X3PA", "X2PA","FTA", "X3P", "X2P","FT","ORB","DRB","AST","STL","BLK","TOV", "PF", "PTS")]
    a=a/(nba2024permin[i, 8])
    nba2024permin$Mincheck = (nba2024permin[i, 8])/(nba2024permin$MP[i])
  }
  
  nba2023permin = nba2023
  nba2023permin = nba2023permin[!duplicated(nba2023permin[2]), ]
  
  
  for (i in 1:length(nba2023permin$Rk)){
    a=nba2023permin[i,c("FG","FGA","X3PA", "X2PA","FTA", "X3P", "X2P","FT","ORB","DRB","AST","STL","BLK","TOV", "PF", "PTS")]
    a=a/(nba2023permin[i, 8])
    nba2023permin$Mincheck = (nba2023permin[i, 8])/(nba2023permin$MP[i])
  }
  
  nba2023permin$Player=iconv(nba2023permin$Player, "UTF-8", "UTF-8",sub='')
  
  nba2023permin=separate(data=nba2023permin, Player, sep = " ", into = c("first","last"), extra="drop")
  
  nba2024permin$Player=iconv(nba2024permin$Player, "UTF-8", "UTF-8",sub='')
  
  nba2024permin=separate(data=nba2024permin, Player, sep = " ", into = c("first","last"), extra="drop")
  nba2024permin[which(nba2024permin$first=="Kelly" & nba2024permin$last=="Oubre"),]
  
  #starters = c("Tyrese Maxey", "Ricky Council", "Alex Caruso", "Kyle Kuzma", "Joel Embiid")
  starterstats =data.frame(nba2024permin[which(nba2024permin$first=="Kelly" & nba2024permin$last=="Oubre"),])
  #bench = c("Markelle Fultz", "Gary Trent", "Kelly Oubre", "Trendon Watford", "Jonas Valan?i?nas")
  benchstats = starterstats
  dfloop=0
  
 # third = c("Cameron Payne","Terquavion Smith","Jordan Nwora", "Nicolas Batum", "Andre Drummond")
  thirdstats=starterstats
  
  for (i in starters){
    dfloop=dfloop+1
    b=as.vector(str_split_fixed(i, pattern = " ", n = nchar(i)))
    c=nba2024permin[which(nba2024permin$first==(b[1]) & nba2024permin$last==(b[2])),]
    starterstats[dfloop,]= c
    d=nba2024permin[which(nba2024permin$first==(b[1]) & nba2024permin$last==(b[2])),]
    e=nba2023permin[which(nba2023permin$first==(b[1]) & nba2023permin$last==(b[2])),]
    f= nba2023permin[which(nba2023permin$first==("Booga") & nba2023permin$last==("Ogga")),]
    starterstats$Expect[dfloop]=ifelse(dim(e) != dim(f),((as.numeric(d[9]) + as.numeric(e[9]))/2),36)
    x= as.numeric(starterstats$Expect[dfloop])
    starterstats$ExpectMPG[dfloop] = sample(((x-10):45),1)
  }
  
  starterstats
  dfloop1=0
  
  for (i in bench){
    dfloop1=dfloop1+1
    b=as.vector(str_split_fixed(i, pattern = " ", n = nchar(i)))
    c=nba2024permin[which(nba2024permin$first==(b[1]) & nba2024permin$last==(b[2])),]
    benchstats[dfloop1,]= c
    d=nba2024permin[which(nba2024permin$first==(b[1]) & nba2024permin$last==(b[2])),]
    e=nba2023permin[which(nba2023permin$first==(b[1]) & nba2023permin$last==(b[2])),]
    f= nba2023permin[which(nba2023permin$first==("Booga") & nba2023permin$last==("Ogga")),]
    benchstats$Expect[dfloop1]=ifelse(dim(e) != dim(f),((as.numeric(d[9]) + as.numeric(e[9]))/2),36)
  }
  benchstats
  
  dfloop2=0
  
  for (i in third){
    dfloop2=dfloop2+1
    b=as.vector(str_split_fixed(i, pattern = " ", n = nchar(i)))
    c=nba2024permin[which(nba2024permin$first==(b[1]) & nba2024permin$last==(b[2])),]
    thirdstats[dfloop2,]= c
  }
  
  thirdstats
  
  
  #Subtract starter minutes from total minutes to get bench minutes
  starterstats$ExpectMPG = as.numeric(starterstats$ExpectMPG)
  benchstats$ExpectMPG=48 - as.numeric(starterstats$ExpectMPG)
  
  #Determine the number of games everyone will play in
  
  
  GPstarters = c(sample(65:80,1), sample(65:80,1),sample(65:80,1),sample(65:80,1),sample(65:80,1))
  
  GSbench = 82-GPstarters
  
  GPbench = c((sample(45:70,1)), (sample(65:80,1)),(sample(65:75,1)),(sample(60:80,1)),(sample(70:80,1)))
  
  Offbench = abs(GSbench-GPbench)
  
  GPthirds = (82-GPstarters)+(82-GPbench)
  GPthirds
  
  #determine the total minute starters wil play over a season
  
  starter_total_mins=(starterstats$ExpectMPG * GPstarters)
  
  Projections = c()
  
  Projections$Names = as.data.frame(starters) #use the starter names to make a column
  
  Projections$TotalMins = as.data.frame(starter_total_mins) #add the total minutes as a column
  
  Projections=data.frame(Projections) #restructure as data frame
  
  Projections
  for (i in (10:31)){
    j=i-7
    Projections[j]= (starterstats[i]/starterstats$MP * Projections$starter_total_mins) #per minute stats time total minutes to get total stats
  }
  
  h=c("FG.","X3P.", "X2P.","eFG.","FT.")
  
  Projections[h] = starterstats[h] #keep percentages
  
  BenchEstimate = c()
  
  BenchEstimate$Names = as.data.frame(bench) #use the bench names to make a column
  
  bench_minutes=benchstats$ExpectMPG*GPbench #use this to determine the total minutes the bench will play
  BenchEstimate$totalminutes = bench_minutes #put the total minutes in the BenchEstimate data frame
  
  BenchEstimate = data.frame(BenchEstimate) #formatting
  
  for (i in (10:31)){
    j=i-7
    BenchEstimate[j]= (benchstats[i]/benchstats$MP * BenchEstimate$totalminutes) #using per minute stats to get total stats
  }
  
  BenchEstimate[h]=benchstats[h]
  
  ThirdsGuess = c() #dataframe for total stats for third string
  ThirdsGuess$names = data.frame(third)
  ThirdsGuess$minutes = benchstats$ExpectMPG*GPthirds
  
  ThirdsGuess = data.frame(ThirdsGuess)
  
  for (i in (10:31)){
    j=i-7
    ThirdsGuess[j]= (thirdstats[i]/thirdstats$MP * ThirdsGuess$minutes) #use per min stats to guess total stats
  }
  
  ThirdsGuess[h] = thirdstats[h]
  
  starterAvgs = c()
  starterAvgs = Projections[1,] #the starter names
  
  Projections[1,2]
  Projections[1]
  Projections[2]/GPstarters
  
  totalAvgs2 = starterAvgs
  
  for (i in 2:length(Projections)){ #skip first column for names
    starterAvgs[i,2]=sum((Projections[i])/(GPstarters)) #get the starters average production for each 
    starterAvgs[i,1]=colnames(Projections)[i]
    totalAvgs2[i,1]=colnames(Projections)[i]
    totalAvgs2[i,2]=sum(Projections[i])
  }
  
  starterAvgs = data.frame(starterAvgs)
  
  starterAvgs=starterAvgs[c(1,2)]
  
  starterAvgs[5,2]=starterAvgs[3,2]/starterAvgs[4,2]
  starterAvgs[8,2]=starterAvgs[6,2]/starterAvgs[7,2]
  starterAvgs[11,2]=starterAvgs[9,2]/starterAvgs[10,2]
  starterAvgs[15,2]=starterAvgs[13,2]/starterAvgs[14,2]
  
  starterAvgs
  
  benchAvgs = c()
  benchAvgs$names = data.frame(bench,bench)
  benchAvgs = data.frame(benchAvgs)
  
  benchstarting = (GSbench * starterstats$ExpectMPG)
  
  for (i in 2:length(BenchEstimate)){
    benchAvgs[i,2]= benchstarting * BenchEstimate[i]/BenchEstimate$totalminutes
    benchAvgs[i,1]=colnames(BenchEstimate)[i]
    totalAvgs2[i,2] = totalAvgs2[i,2] + sum(benchstarting * BenchEstimate[i]/BenchEstimate$totalminutes)
  }
  
  totalAvgs2[2]/sum(GPstarters,GSbench)
  
  benchAvgs=data.frame(list(benchAvgs[1]),as.numeric(unlist(benchAvgs[2])))
  
  
  benchAvgs[5,2]=benchAvgs[3,2]/benchAvgs[4,2]
  benchAvgs[8,2]=benchAvgs[6,2]/benchAvgs[7,2]
  benchAvgs[11,2]=benchAvgs[9,2]/benchAvgs[10,2]
  benchAvgs[15,2]=benchAvgs[13,2]/benchAvgs[14,2]
  
  benchAvgs_starting = benchAvgs
  
  
  benchoffbench = (Offbench*benchstats$ExpectMPG)
  
  for (i in 2:length(BenchEstimate)){
    benchAvgs[i,2]= benchoffbench * BenchEstimate[i] /BenchEstimate$totalminutes
    benchAvgs[i,1]=colnames(BenchEstimate)[i]
    totalAvgs2[i,2] = totalAvgs2[i,2] + sum(benchoffbench * BenchEstimate[i]/BenchEstimate$totalminutes)
  }
  
  
  benchAvgs=data.frame(list(benchAvgs[1]),as.numeric(unlist(benchAvgs[2])))
  
  
  benchAvgs[5,2]=benchAvgs[3,2]/benchAvgs[4,2]
  benchAvgs[8,2]=benchAvgs[6,2]/benchAvgs[7,2]
  benchAvgs[11,2]=benchAvgs[9,2]/benchAvgs[10,2]
  benchAvgs[15,2]=benchAvgs[13,2]/benchAvgs[14,2]
  
  benchAvgs_offbench = benchAvgs
  
  benchAvgs_total=data.frame(benchAvgs$names.bench, "total" = benchAvgs_offbench$as.numeric.unlist.benchAvgs.2... + benchAvgs_starting$as.numeric.unlist.benchAvgs.2...)
  
  benchAvgs= data.frame(benchAvgs_total[1] , benchAvgs_total[2]/mean(c(GSbench,Offbench)))
  
  bench_starterAvgs=data.frame(starterAvgs[1],benchAvgs[2]+starterAvgs[2])
  
  ThirdsGuess
  thirdstats
  
  thirdAvgs = c()
  thirdAvgs$names = data.frame(bench,bench)
  thirdAvgs = data.frame(benchAvgs)
  
  for (i in 2:length(BenchEstimate)){
    thirdAvgs[i,2]= benchstats$ExpectMPG* GPthirds * ThirdsGuess[i] /ThirdsGuess$minutes
    thirdAvgs[i,1]=colnames(BenchEstimate)[i]
    totalAvgs2[i,2] = totalAvgs2[i,2] + sum(GPthirds * benchstats$ExpectMPG * ThirdsGuess[i] /ThirdsGuess$minutes)
  }
  
  
  thirdAvgs=data.frame(list(thirdAvgs[1]),as.numeric(unlist(thirdAvgs[2])))
  
  thirdAvgs[5,2]=thirdAvgs[3,2]/thirdAvgs[4,2]
  thirdAvgs[8,2]=thirdAvgs[6,2]/thirdAvgs[7,2]
  thirdAvgs[11,2]=thirdAvgs[9,2]/thirdAvgs[10,2]
  thirdAvgs[15,2]=thirdAvgs[13,2]/thirdAvgs[14,2]
  
  totalAvgs2[5,2]=totalAvgs2[3,2]/totalAvgs2[4,2]
  totalAvgs2[8,2]=totalAvgs2[6,2]/totalAvgs2[7,2]
  totalAvgs2[11,2]=totalAvgs2[9,2]/totalAvgs2[10,2]
  totalAvgs2[15,2]=totalAvgs2[13,2]/totalAvgs2[14,2]
  
  totalAvgs=totalAvgs2[1:2]
  totalAvgs[2]=totalAvgs2[2]/82
  totalAvgs[c(5,8,11,15),2] = totalAvgs2[c(5,8,11,15),2]
  
  #print(c("Total Team Averages"))
  #print(totalAvgs)
  #print(c("Total Starter Averages"))
  #print(starterAvgs)
  #print(c("Total third String Averages"))
  #print(thirdAvgs)
  
  print(data.frame("stat"= totalAvgs[1],
                   "Tot Team Avg"=totalAvgs[2],
                   "Tot Starter Output"=starterAvgs[2],
                   "Tot 3rds Output"=thirdAvgs[2]))
}

x= c("Tyrese Maxey", "Ricky Council", "Malik Monk", "Trendon Watford", "Joel Embiid")
y=c("T.J. McConnell", "Alex Caruso", "Kelly Oubre", "Taurean Prince", "Andre Drummond")
z=c("Cameron Payne","Jeff Dowtin","Jordan Nwora", "Nicolas Batum", "Daniel Theis")

alpha=roster.stats.guesses(x,y,z)

a= c("Tyrese Maxey", "Ricky Council", "Alex Caruso", "Trendon Watford", "Joel Embiid")
b=c("Markelle Fultz", "Gary Trent", "Kelly Oubre", "Taurean Prince", "Andre Drummond")
c=c("Cameron Payne","Jeff Dowtin","Jordan Nwora", "Nicolas Batum", "Daniel Theis")


beta = roster.stats.guesses(a,b,c)
alpha[1:2]

roster.comparison <- function(alpha){
  teams = read.csv("C:\\Users\\DavetteUser\\Documents\\nba stuff\\nba_team_stats_00_to_23.csv")
  
  colnames(teams)
  
  teams=teams[1:length(teams)][which(teams$season=="2023-24"),]
  
  teamsavg=data.frame(colnames(teams))
  
  for (i in 3:(length(teams)-1)){
    j=i
    teamsavg[j,2]=sum(teams[i])/30
  }
  
  skips = c(6,11,14,17)
  
  teamsavg$V2 = teamsavg$V2/82
  teamsavg$V2[skips] = teamsavg$V2[skips]*82
  
  teamsavg
  beta=data.frame(alpha[2], row.names=c(alpha$starters))
  plottable =  data.frame(teamsavg[6:29,], row.names = c(teamsavg$colnames.teams.[6:29]))
  plottable$team_performance = alpha[,2]
  plottable["field_goals_made",][1]  = beta["FG",]
  plottable["field_goals_attempted",][1]=beta["FGA",]
  plottable["field_goal_percentage",][1]=beta["FG.",]*100
  plottable["three_pointers_made" ,][1]=beta["X3P",]
  plottable[ "three_pointers_attempted" ,][1]=beta["X3PA",]
  plottable["three_point_percentage" ,][1]=beta["X3P.",]*100
  plottable["free_throws_made" ,][1]=beta["FT",]
  plottable["free_throw_attempted",][1]=beta["FTA",]
  plottable["free_throw_percentage" ,][1]=beta["FT.",]*100
  plottable["offensive_rebounds"  ,][1]=beta["ORB",]
  plottable["defensive_rebounds" ,][1]=beta["DRB",]
  plottable["rebounds" ,][1]=beta["TRB",]
  plottable["assists" ,][1]=beta["AST",]
  plottable["steals" ,][1]=beta["STL",]
  plottable["personal_fouls" ,][1]=beta["PF",]
  plottable["blocks" ,][1]=beta["BLK",]
  plottable["turnovers" ,][1]=beta["TOV",]
  plottable["points" ,][1]=beta["PTS",]
  
  colnames(plottable) = c("Estimated_Perform", "League_Avg")
  
  row.names(plottable)[22:24] = c("two_pointers_made", "two_pointers_attempted",
                                    "two_pointer_percentage")
  plottable[c("two_pointers_made", "two_pointers_attempted",
              "two_pointer_percentage"),][1] = beta[c("X2P","X2PA", "X2P."),]
  plottable["two_pointers_made",][2] = (plottable["field_goals_made",][2]-plottable["three_pointers_made",][2])
  plottable[c("two_pointers_attempted"),][2] =  -(plottable["three_pointers_attempted",][2] - plottable["field_goals_attempted",][2])
  
  plottable[24,2] = plottable[22,2]/plottable[23,2] *100
  plottable[24,1] = as.numeric(plottable[24,1])*100
  
  plottable$Estimated_Perform = as.numeric(plottable$Estimated_Perform)
  
  plottable=plottable[-c(1,2,20),]
  plottable=data.frame(plottable)
  plottable[3]= -(plottable$League_Avg - plottable$Estimated_Perform)/plottable$League_Avg
  plottable[4] = -(plottable$League_Avg - plottable$Estimated_Perform)
  
  colnames(plottable)[c(3,4)] = c("EstPerform_LeagueAvg_Diff_percentage", "EstPerform_LeagueAvg_Diff")
  return(na.omit(plottable))
  
  conffinals = teams[1,]
  champloop = 0
  for (i in teams$Team){
    champloop = champloop + 1
    if (i %in% c("Boston Celtics", "Indiana Pacers", "Minnesota Timberwolves", "Dallas Mavericks")){
      conffinals[champloop,] = teams[i]
    }
  }
  
  conffinals = teams[which(teams$Team %in% c("Boston Celtics", "Indiana Pacers", "Minnesota Timberwolves", "Dallas Mavericks")),]
  print(conffinals)
}

starters1 = c("Malcolm Brogdon","Jalen Suggs", "Franz Wagner", "Paolo Banchero", "Wendell Carter")
bench1 = c("Anthony Black", "Cole Anthony", "Royce O'Neale", "Jonathan Isaac", "Moritz Wagner")
third1 = c("Cameron Payne", "Jett Howard", "Caleb Houstan", "Trendon Watford", "Goga Bitadze")

omega=roster.stats.guesses(starters1,bench1,third1)
oso.roster=roster.comparison(omega)
teej=roster.comparison(alpha)
fultz = roster.comparison(beta)
alpha


