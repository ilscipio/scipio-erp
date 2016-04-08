#!/bin/sh

# set the parent directory as OFBiz Home
OFBIZ_HOME="$( cd -P "$( dirname "$0" )" && pwd )"
OFBIZ_ENTITY_ENGINE_PATH="$OFBIZ_HOME/framework/entity/config/entityengine.xml"
OFBIZ_DEFAULT_FS_RDBMS="localderby"
OFBIZ_DEFAULT_FS_RDBMS_PATH="$OFBIZ_HOME/runtime/data/derby"
check_task () { 
    for e in "${1}"; do 
        if [ "$e" = "$2" ]; then
            return 0;
        fi
    done
    return 1
}
check_rdbms () {
    if [ -e "$OFBIZ_ENTITY_ENGINE_PATH" ] && [ -f "$OFBIZ_ENTITY_ENGINE_PATH" ] && [ -r "$OFBIZ_ENTITY_ENGINE_PATH" ]; then
        echo "$OFBIZ_ENTITY_ENGINE_PATH is a valid file"
        DEFAULT_DATASOURCES="$(sed -n -r '/<delegator name="default".*>/,/<\/delegator>/ s/.*/&/p' $OFBIZ_ENTITY_ENGINE_PATH | sed -n -e 's/^.*datasource-name="\(.*\)".*/\1/p')"
        for ds in ${DEFAULT_DATASOURCES}; do 
            if [ "$ds" = "$OFBIZ_DEFAULT_FS_RDBMS" ]; then return 0; fi;
        done
        return 1
    else 
        echo "$OFBIZ_ENTITY_ENGINE is an INVALID file"
    fi
}

BUILD_TASKS="$(sh ant -p | grep -oe '^[[:space:]][a-zA-Z-]+')"

if [ -n $1 ] && [ $1 = "-h" -o $1 = "-help" ]
then
    echo "Cato-Commerce: Help"
    echo "Cato-Commerce: Usage ./start.sh [TARGET]"
    echo "Cato-Commerce: If no TARGET is passed, 'load-demo' and afterwards 'start' targets will be automatically executed. The former only if the configured datasource in $OFBIZ_ENTITY_ENGINE_PATH is $OFBIZ_DEFAULT_FS_RDBMS and no data under $OFBIZ_ENTITY_ENGINE_PATH is present."
    echo "$(sh ant -p)"
elif [ -n $1 ]; then
    if [ "$1" = "build" ]; then
        echo "Cato-Commerce: Specific build task found: [build]. Launching..."
        sh ant
    else
        check_task ${BUILD_TASKS} $1
        if [ $? -eq 0 ]; then
            echo "Cato-Commerce: Specific build task found: [$1]. Launching..."
            sh ant $1
        else
            echo "Cato-Commerce: Specific build task not found: [$1]. Aborting..."
        fi
    fi
elif [ -z $1 ]; then
        echo "Cato-Commerce: Welcome to Cato-Commerce"
        check_rdbms
        if [ $? -eq 0 ]; then    
            echo "Cato-Commerce: Default RDBMS has been detected [$OFBIZ_DEFAULT_FS_RDBMS]"
            if [ -e "$OFBIZ_DEFAULT_FS_RDBMS_PATH" ] && [ -d "$OFBIZ_DEFAULT_FS_RDBMS_PATH" ] && [ -s "$OFBIZ_DEFAULT_FS_RDBMS_PATH" ]; then
                echo "Cato-Commerce: No data found. Compile and loading demo data"
                sh ant load-demo
            fi            
        fi
        echo "Cato-Commerce: Starting framework..."
        sh ant start

fi