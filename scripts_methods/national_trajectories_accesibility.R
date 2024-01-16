#########loops to extract data from Condatis runs in nature recovery project##########


cfolder<-"C:/Users/cgra/OneDrive - The University of Liverpool/My files/Condatis4Naturerecovery/Rproj/spatial_data/derived"



scale<-"national"
habitat<-"heathland"
dispt<-"3k"
direction<-"NWSE"


##test with one landscape


power<-read.csv(file.path(cfolder,scale,habitat,dispt,"Nat_1k_heathland_3k_NWSE_power.csv"))

summary(power)

flow<-read.csv(file.path(cfolder,scale,habitat,dispt,"Nat_1k_heathland_3k_NWSE_flow.csv"),row.names=1)

sumstat<-read.csv(file.path(cfolder,scale,habitat,dispt,"Nat_1k_heathland_3k_NWSE_speed_power.csv"),row.names=1)

runlabel<-"Nat_1k_heathland_3k_NWSE"

rlist<-c(runlabel=runlabel,as.list(sumstat),ncells=dim(flow)[1],
  npows=dim(power)[1],maxpowperc=max(power$perc),cpmax= max(power$cumsum_perc))

natresults<-as.data.frame(rlist)[0,]


##########loop for all national######



for(habitat in c("heathland","grassland","wetland")){
for(direction in c("NWSE","NESW","NS","EW")){

runlabel<-paste("Nat_1k",habitat,dispt,direction,sep="_")

err<-try({
power<-read.csv(file.path(cfolder,scale,habitat,dispt,paste0(runlabel,"_power.csv")))

flow<-read.csv(file.path(cfolder,scale,habitat,dispt,paste0(runlabel,"_flow.csv")),row.names=1)

sumstat<-read.csv(file.path(cfolder,scale,habitat,dispt,paste0(runlabel,"_speed_power.csv")),row.names=1)
})

if(!inherits(err,"try-error")){
rlist<-c(runlabel=runlabel,as.list(sumstat),ncells=dim(flow)[1],
         npows=dim(power)[1],maxpowperc=max(power$perc),cpmax= max(power$cumsum_perc))

natresults<-rbind(natresults,rlist)

assign(paste0(runlabel,"_traj"),as.vector(power$perc))
}
}}



#####################local scales##################

scale<-"local"

if(scale=="local"){prefix<-"Loc_250m"}else{prefix<-"Nat_1k"}

for(dispt in c("3k","1k")){
for(k in 1:3){
  for(j in 1:4){
    habitat <- c("heathland","grassland","wetland")[k]
    landscape <- c("Surrey","SussexKent","LostWetlands")[k]
    direction <- c("NWSE","NESW","NS","EW") [j] 

      runlabel<-paste(prefix,habitat,dispt,direction,sep="_")
      
      err<-try({
        power<-read.csv(file.path(cfolder,scale,landscape,dispt,paste0(runlabel,"_power.csv")))
        
        flow<-read.csv(file.path(cfolder,scale,landscape,dispt,paste0(runlabel,"_flow.csv")),row.names=1)
        
        sumstat<-read.csv(file.path(cfolder,scale,landscape,dispt,paste0(runlabel,"_speed_power.csv")),row.names=1)
      })
      
      if(!inherits(err,"try-error")){
        rlist<-c(runlabel=runlabel,as.list(sumstat),ncells=dim(flow)[1],
                 npows=dim(power)[1],maxpowperc=max(power$perc),cpmax= max(power$cumsum_perc))
        
        natresults<-rbind(natresults,rlist)
        
        assign(paste0(runlabel,"_traj"),as.vector(power$perc))
      }
  }}}#end 3 loops around run options


##

natresults$maxscore<-with(natresults,ncells*maxpowperc/100)

###plot###

with(natresults,summary(maxscore))

par(mfrow=c(1,3),mex=0.6)
#plot national only#
scale<-"national"
if(scale=="local"){prefix<-"Loc_250m"}else{prefix<-"Nat_1k"}
dispt<-"3k"

line_colors <- c("#000000", "#0000cc", "#FF0000")
line_styles <- c(2, 3, 4, 6)

plot(1,1,xlim=c(1,10000),ylim=c(1/1000,3300),log="xy",type="n",xlab="rank",ylab="score",main="national")

for(k in 1:3){
  for(j in 1:4){
habitat <- c("heathland","grassland","wetland")[k]
direction <- c("NWSE","NESW","NS","EW") [j]       
    runlabel<-paste(prefix,habitat,dispt,direction,sep="_")
    
traj<-get(paste0(runlabel,"_traj"))

traj<-traj*natresults[natresults$runlabel==runlabel,"ncells"]/100

lenx<-natresults[natresults$runlabel==runlabel,"npows"]

lines(1:lenx,traj,lwd=1.5,lty=line_styles[j],col = line_colors[k])  
  }}

abline(h=c(1,5,50),col=c("#70752f","#4c2c92","#5b616b"), lwd = 1.5)

legend("bottom",c("heathland","grassland","wetland","NWSE","NESW","NS","EW"),lty=c(1,1,1,2,3,4,6),       col=c('#000000','#0000cc','#ff0000',1,1,1,1),lwd=2, cex = 1.5)

#plot local 1k#

scale<-"local"
if(scale=="local"){prefix<-"Loc_250m"}else{prefix<-"Nat_1k"}
dispt<-"1k"


plot(1,1,xlim=c(1,10000),ylim=c(1/1000,30),log="xy",type="n",xlab="rank",ylab="score",main="local, 1km dispersal")


for(k in 1:3){
  for(j in 1:4){
    habitat <- c("heathland","grassland","wetland")[k]
    direction <- c("NWSE","NESW","NS","EW") [j]       
    runlabel<-paste(prefix,habitat,dispt,direction,sep="_")
    
    traj<-get(paste0(runlabel,"_traj"))
    
    traj<-traj*natresults[natresults$runlabel==runlabel,"ncells"]/100
    
    lenx<-natresults[natresults$runlabel==runlabel,"npows"]
    
    lines(1:lenx,traj,lwd=1.5,lty=line_styles[j],col = line_colors[k])  
  }}

abline(h=c(1,5,50),col=c("#70752f","#4c2c92","#5b616b"), lwd = 1.5)


#plot local 3k#

scale<-"local"
if(scale=="local"){prefix<-"Loc_250m"}else{prefix<-"Nat_1k"}#http://127.0.0.1:26493/graphics/plot_zoom_png?width=1017&height=900
dispt<-"3k"


plot(1,1,xlim=c(1,10000),ylim=c(1/1000,30),log="xy",type="n",xlab="rank",ylab="score",main="local, 3.4km dispersal")


for(k in 1:3){
  for(j in 1:4){
    habitat <- c("heathland","grassland","wetland")[k]
    direction <- c("NWSE","NESW","NS","EW") [j]       
    runlabel<-paste(prefix,habitat,dispt,direction,sep="_")
    
    traj<-get(paste0(runlabel,"_traj"))
    
    traj<-traj*natresults[natresults$runlabel==runlabel,"ncells"]/100
    
    lenx<-natresults[natresults$runlabel==runlabel,"npows"]
    
    lines(1:lenx,traj,lwd=1.5,lty=line_styles[j],col = line_colors[k])  
  }}

abline(h=c(1,5,50),col=c("#70752f","#4c2c92","#5b616b"), lwd = 1.5)

##tidy up, save data##
onames<-ls()
save(list=c("natresults",onames[grepl("_traj",onames)]),file="study_trajectories.Rdata")
#"study_trajectories.Rdata" is manually moved out of Rproject folder to save space on Github
rm(list=c("natresults",onames[grepl("_traj",onames)]))
rm(power,flow,sumstat,rlist,traj)

ls()
save.image()

##if want to re-load data to plot graphs etc ##

load("J:/r_github/largefiles/study_trajectories.Rdata")
