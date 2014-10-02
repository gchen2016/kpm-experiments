#!/bin/bash

envname=empty-unbounded-r0-d113-c180
instancesetname="empty-unbounded-r150-sgoverlap"
denvxml="d-envs/$envname.xml"
instancefolder="instances/$instancesetname"
maxtime=10000
timeout=1200000

radius=150
timestep=20

mkdir -p $instancefolder
rm $instancefolder/*
cp prepare.sh $instancefolder/

instance=0
for nagents in "1" "2" "3" "4" "5" "6" "7" 
do
    for seed in {1..25}
    do
        let instance=instance+1
	    # create a problem instance file
	    instancename="$instance"
	    instancefile=$instancefolder/$instancename.xml

        ## ConflictGenerator
        java -XX:+UseSerialGC -cp solver.jar -Dlog4j.configuration="file:$PWD/log4j.custom" tt.jointeuclid2ni.probleminstance.generator.GenerateInstance -env $denvxml -nagents $nagents -radius $radius -seed $seed -outfile $instancefile  -onecluster -missionarea 1000x1000
        
        # add instance to data.in
        for alg in "PP" "ORCA" "ODCN"
        do
		    summaryprefix="$envname;$instance;$nagents;$radius;$seed;$timestep;$maxtime;$alg;NA;"
	        echo -method $alg -heuristic PERFECT -problemfile $instancefile -timestep $timestep -maxtime $maxtime -timeout $timeout -summary -summaryprefix "$summaryprefix" >> $instancefolder/data.in           
        done
        
		alg="KDPMD"
		for k in "1" "2" "3" "4" "5" "10" "20" "50" "100"
		do
			summaryprefix="$envname;$instance;$nagents;$radius;$seed;$timestep;$maxtime;$alg;$k;"
			echo -method $alg -k $k -heuristic PERFECT -problemfile $instancefile -timestep $timestep -maxtime $maxtime -timeout $timeout -summary -summaryprefix "$summaryprefix" >> $instancefolder/data.in           
		done

	    echo Finished instance no $instance. Agents: $nagents. Seed: $seed.
    done        
done
echo "env;instance;nagents;radius;seed;timestep;maxtime;alg;k;cost;runtime;expansions;status;" > $instancefolder/head
echo Done. Created $instance instances at $envname environment. Instances stored in $instancefolder.

