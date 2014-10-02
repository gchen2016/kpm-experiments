#!/bin/bash

envname=empty-unbounded-r0-d113-c180
instancesetname="empty-unbounded-r75"
denvxml="d-envs/$envname.xml"
instancefolder="instances/$instancesetname"
maxtime=4000
timeout=3600000

radius=75
timestep=20

mkdir -p $instancefolder
rm $instancefolder/*
cp prepare.sh $instancefolder/

instance=0
for nagents in "1" "2" "3" "4" "5" "10" "15" "20" "25"
do
    for seed in {1..25}
    do
        let instance=instance+1
	    # create a problem instance file
	    instancename="$instance"
	    instancefile=$instancefolder/$instancename.xml

        ## ConflictGenerator
        java -XX:+UseSerialGC -cp solver.jar -Dlog4j.configuration="file:$PWD/log4j.custom" tt.jointeuclid2ni.probleminstance.generator.GenerateInstance -env $denvxml -nagents $nagents -radius $radius -seed $seed -outfile $instancefile -onecluster -missionarea 1000x1000
               
        
        # add instance to data.in
        # Compute OD only for lower number of agents
        if [ "$nagents" -lt "6" ]; then
            algs="PP ORCA ODCN"
        else
            algs="PP ORCA"
        fi    
        
        for alg in $algs
        do
		    if [ "$alg" == "ORCA"  ]; then
		        maxtimearg=10000
		    else
		        maxtimearg="$maxtime"
		    fi
		    
		    summaryprefix="$envname;$instance;$nagents;$radius;$seed;$timestep;$maxtime;$alg;NA;"
	        echo -method $alg -heuristic PERFECT -problemfile $instancefile -timestep $timestep -maxtime $maxtimearg -timeout $timeout -summary -summaryprefix "$summaryprefix" >> $instancefolder/data.in           
        done
        
		alg="KDPMD"
		for k in "1" "2" "3" "4" "5" "10" "20" "50" "100" "200"
		do
			summaryprefix="$envname;$instance;$nagents;$radius;$seed;$timestep;$maxtime;$alg;$k;"
			echo -method $alg -k $k -heuristic PERFECT -problemfile $instancefile -timestep $timestep -maxtime $maxtime -timeout $timeout -summary -summaryprefix "$summaryprefix" >> $instancefolder/data.in           
		done

	    echo Finished instance no $instance. Agents: $nagents. Seed: $seed.
    done        
done
echo "env;instance;nagents;radius;seed;timestep;maxtime;alg;k;cost;runtime;expansions;status;" > $instancefolder/head
echo Done. Created $instance instances at $envname environment. Instances stored in $instancefolder.

