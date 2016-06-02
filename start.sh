#!/bin/sh

# set OFBIZ's relevant paths
OFBIZ_HOME="$( cd -P "$( dirname "$0" )" && pwd )"
OFBIZ_ENTITY_ENGINE_PATH="$OFBIZ_HOME/framework/entity/config/entityengine.xml"
OFBIZ_DEFAULT_FS_RDBMS_PATH="$OFBIZ_HOME/runtime/data/derby"
# other OFBIZ's variables
OFBIZ_DEFAULT_FS_RDBMS="localderby"
# set argument values to proper variables
ANT_VERBOSE=0
ANT_TARGET=

#----- BEGIN CUSTOM FUNCTIONS -----#
check_task () { 
    BUILD_TASKS="$(echo build | sh ant -p | grep -Eo '^[[:space:]][a-zA-Z-]+')"    
    #echo "build tasks ==============> $BUILD_TASKS"
    for e in "$BUILD_TASKS"; do 
        task = echo -e "${e}" | tr -d '[[:space:]]'
        $(echo -e ${task} \r)
        if [ "$task" = "$2" ]; then
            return 0;
        fi
    done
    return 1
}

check_arguments () {
    # If '-h' or 'help' is found either in $1 or $2, display the help message, don't care what the other argument is.
    if [ "$#" -lt 3 ]; then
        if [ -n $1 ] || [ -n $2 ]; then
            if [ "$1" = "-help" -o "$1" = "-h" ] || [ "$2" = "-help" -o "$2" = "-h" ]; then        
                return 1
            fi
        fi
    fi

    # If more than 2 arguments have been passed, show error and display the help message 
    if [ "$#" -gt 2 ]; then    
        echo "Scipio-Commerce: Error: Invalid number of arguments: $#"
        sh start.sh -h
        exit 1
    # If no argument found, proceed with the default behavior.  
    elif [ "$#" -eq 0 ]; then
        return 3
    # If 1 argument has been passed, check if a target has been passed or just the verbose flag, otherwise send and error.
    elif [ "$#" -eq 1 ]; then
        if [ $1 = "-v" ]; then
            ANT_VERBOSE=1
            return 3
        else
            check_task $ANT_TARGET
            if [ $? -eq 0 ]; then
                echo "Scipio-Commerce: Specific build task found: [$1]. Launching..."
                ANT_TARGET=$1
                return 2
            else
                echo "Scipio-Commerce: Specific build task not found: [$1]. Aborting..."
                exit 1
            fi
        fi
    # If 2 arguments have been passed, expect the verbose flag to be in the first place, otherwise display error. Also if it isn't found
    elif [ "$#" -eq 2 ]; then
        if [ $1 = "-v" ]; then
            ANT_VERBOSE=1
            ANT_TARGET=$2
            return 2
        else
            echo "Scipio-Commerce: Error: Passed arguments are invalid."
            sh start.sh -h
            exit 1
        fi
    fi
}
#----- END CUSTOM FUNCTIONS -----#

check_arguments $@
START_ACTION=$?

if [ $START_ACTION -eq 1 ]
then    
    sh ant scipio-help -logger com.ilscipio.scipio.util.ant.logger.ScipioLogger -lib ./framework/base/lib/ant
elif [ $START_ACTION -eq 2 ]; then    
    #echo "Scipio-Commerce: Specific build task found: [$ANT_TARGET]. Launching..."
    if [ $ANT_VERBOSE -eq 1 ]; then
        sh ant $ANT_TARGET -logger com.ilscipio.scipio.util.ant.logger.ScipioLogger -lib ./framework/base/lib/ant
    else
        sh ant $ANT_TARGET > /dev/null 2>&1
    fi   
elif [ $START_ACTION -eq 3 ]; then
    sh ant scipio-default-start -logger com.ilscipio.scipio.util.ant.logger.ScipioLogger -lib ./framework/base/lib/ant
fi