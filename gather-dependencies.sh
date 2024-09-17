NOW=$(date +"%Y-%m-%d-%H-%M-%S")
LOGFILE="log/log-$NOW.txt"
#echo "$LOGFILE"
java -Xmx12g -Xms2g -Xmn1g -Xss2m -XX:+UseG1GC -cp "lib/*" com.turandot.lang.cobol.standalone.gather.dependencies.GatherDependencies $1 > log/gather-dependencies.log 2> log/gather-dependencies-err.log 
