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
    BUILD_TASKS="$(sh ant -p | grep -oe '^[[:space:]][a-zA-Z-]+' | echo build)"    
    echo "build tasks $BUILD_TASKS"
    for e in "$BUILD_TASKS"; do 
        if [ "$e" = "$2" ]; then
            return 0;
        fi
    done
    return 1
}
check_rdbms () {
    if [ -e "$OFBIZ_ENTITY_ENGINE_PATH" ] && [ -f "$OFBIZ_ENTITY_ENGINE_PATH" ] && [ -r "$OFBIZ_ENTITY_ENGINE_PATH" ]; then        
        DEFAULT_DATASOURCES="$(sed -n -r '/<delegator name="default".*>/,/<\/delegator>/ s/.*/&/p' $OFBIZ_ENTITY_ENGINE_PATH | sed -n -e 's/^.*datasource-name="\(.*\)".*/\1/p')"
        for ds in ${DEFAULT_DATASOURCES}; do 
            if [ "$ds" = "$OFBIZ_DEFAULT_FS_RDBMS" ]; then return 0; fi;
        done
        return 1
    else 
        echo "$OFBIZ_ENTITY_ENGINE is an INVALID file"
    fi
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
        echo "Cato-Commerce: Error: Invalid number of arguments: $#"
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
                echo "Cato-Commerce: Specific build task found: [$1]. Launching..."
                ANT_TARGET=$1
                return 2
            else
                echo "Cato-Commerce: Specific build task not found: [$1]. Aborting..."
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
            echo "Cato-Commerce: Error: Passed arguments are invalid."
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
    sh ant cato-help -S -e -q
elif [ $START_ACTION -eq 2 ]; then    
    echo "Cato-Commerce: Specific build task found: [$ANT_TARGET]. Launching..."
    if [ $ANT_VERBOSE -eq 1 ]; then
        sh ant $ANT_TARGET
    else
        sh ant $ANT_TARGET > /dev/null 2>&1
    fi   
elif [ $START_ACTION -eq 3 ]; then
    echo "Cato-Commerce: Welcome to Cato-Commerce"
    check_rdbms
    if [ $? -eq 0 ]; then    
        echo "Cato-Commerce: Default RDBMS has been detected [$OFBIZ_DEFAULT_FS_RDBMS]"
        if [ ! -e "$OFBIZ_DEFAULT_FS_RDBMS_PATH" ]; then
            echo "Cato-Commerce: No demo data found. Compiling and loading demo data. Please wait, this may take a few minutes..."
            if [ $ANT_VERBOSE -eq 1 ]; then
                sh ant load-demo
            else
                sh ant load-demo > /dev/null 2>&1
            fi
        fi            
    fi
    echo "Cato-Commerce: Starting framework..."
    if [ $ANT_VERBOSE -eq 1 ]; then        
        sh ant start &
    else
        sh ant start > /dev/null 2>&1 &
    fi
    echo "Cato-Commerce: Framework successfully started. Goto https://localhost:8443/webtoools to access the admin application."
fi