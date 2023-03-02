#########loops to extract data from Condatis runs in nature recovery project##########

cfolder<-"C:/Users/jhodgson/OneDrive - The University of Liverpool/Condatis4Naturerecovery/Rproj/spatial_data/derived"



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

natresults<-as.data.frame(rlist)[0,]#data frame to store summary statistics


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

#assign(paste0(runlabel,"_traj"),as.vector(power$perc))#I plan to save and plot bottleneck trajectories but commented out to save time
}
}}

#####run again for grassland because names include "__Inf"#######

habitat<-"grassland"

  for(direction in c("NWSE","NESW","NS","EW")){
    
    runlabel<-paste("Nat_1k",habitat,dispt,direction,sep="_")
    
    err<-try({
      power<-read.csv(file.path(cfolder,scale,habitat,dispt,paste0(runlabel,"__Inf_power.csv")))
      
      flow<-read.csv(file.path(cfolder,scale,habitat,dispt,paste0(runlabel,"__Inf_flow.csv")),row.names=1)
      
      sumstat<-read.csv(file.path(cfolder,scale,habitat,dispt,paste0(runlabel,"__Inf_speed_power.csv")),row.names=1)
    })
    
    if(!inherits(err,"try-error")){
      rlist<-c(runlabel=runlabel,as.list(sumstat),ncells=dim(flow)[1],
               npows=dim(power)[1],maxpowperc=max(power$perc),cpmax= max(power$cumsum_perc))
      
      natresults<-rbind(natresults,rlist)
      
   #   assign(paste0(runlabel,"_traj"),as.vector(power$perc))
    }
  }

#####################local scales##################

scale<-"local"

if(scale=="local"){prefix<-"Loc_250m"}else{prefix<-"Nat_1k"}

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
        
   #     assign(paste0(runlabel,"_traj"),as.vector(power$perc))
      }
    }}


