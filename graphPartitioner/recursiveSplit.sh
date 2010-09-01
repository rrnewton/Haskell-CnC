PARTITIONER=metis/pmetis
IMPORTER=./importer
CREATE_PARTITIONS=./createPartitions
ANNOTATE_PARTITIONS=./pathPerNode
SORT_ALGORITHM=./topologicalOrdering

function printHelp {
    cat << EOF
    Usage: ${0##*/} <command_line_arguments>
    command_line_arguments:

    -h -help --help                 
        print help message

    -isProfile
        set your input file type to Profile graph input, default input is *NOT* assumed Profile

    --i <input> --input <input>
        set your input file to <input>

    --c <cutoff> --cutoff <cutoff>
        set your cutoff size for your partitioning to <cutoff>, default is 1

    --k <branchBy> --branchingFactor<branchBy>
        set the branching factor for the partitioning factor to <cutoff>, default to 2

    --resultDir <dir> 
        set the results directory to <dir>, default value is ./outputs

    -no_sort
        turn off sorting partitions with respect to dependencies, default is true

    -intermediate
        set leaving intermediate results not deleted, default is false

    -v -verbose
        set verbose mode on to print what the script is doing, default is false
EOF
    exit
}

if [ $# -eq 0 ] ; then
    printHelp
fi

if [ ! -e $IMPORTER ]; then
    echo "build profile to metis importer"; exit
fi

if [ ! -e $CREATE_PARTITIONS ]; then
    echo "build create partitions"; exit
fi

if [ ! -e $ANNOTATE_PARTITIONS ]; then
    echo "build annotate partitions"; exit
fi



IS_PROFILE_FILE=""
INPUT_FILE=""
INPUT_FILE_NAME="" #same as basename
INPUT_PATH="" #same as dirname

BRANCHING_FACTOR="2"
CUT_OFF_SIZE="1"
RESULT_DIR="outputs"
SORT="do"
INTERMEDIATE=""
VERBOSE=""

while [ -n "$1" ]; do
    case "$1" in
    -h|-help|--help) printHelp; break;;
    -isProfile) IS_PROFILE_FILE="true";;
    --i|--input) shift; INPUT_FILE="$1"; INPUT_FILE_NAME="${1##*/}"; INPUT_PATH="${INPUT_FILE%/*}";;
    --c|--cutoff) shift; CUT_OFF_SIZE="$1";;
    --k|--branchingFactor) shift; BRANCHING_FACTOR="$1";;
    --resultDir) shift; RESULT_DIR="$1";;
    -no_sort) SORT=;;
    -intermediate) INTERMEDIATE="do";;
    -v|-verbose) VERBOSE="do";;
    esac
    shift
done

if [ -z "$INPUT_FILE_NAME" ] ; then
    echo "need to set an input file"; exit
fi

if [ -n "${SORT}" ] ; then
    if [ ! -e "$SORT_ALGORITHM" ]; then
        echo "build sort algorithm"; exit
    fi
fi

IMPORTED_FILE="$INPUT_FILE_NAME.imported.0"


mkdir -p $RESULT_DIR
if [ -n "$VERBOSE" ] ; then
    echo "create directory ${RESULT_DIR}"
fi

cp ${INPUT_FILE} $RESULT_DIR/
if [ -n "$VERBOSE" ] ; then
    echo "copy input file ${INPUT_FILE} to result directory ${RESULT_DIR}/"
fi


if [ -z "$IS_PROFILE_FILE" ] ; then
    if [ -n "$VERBOSE" ] ; then
        echo "create a copy of the metis file with suffix .imported.0, where 0 is the index for the partition"
    fi
    cp ${INPUT_FILE} $RESULT_DIR/${IMPORTED_FILE}
else 
    if [ -n "$VERBOSE" ] ; then
        echo "importing Profile graph ${INPUT_FILE_NAME} to Metis graph ${IMPORTED_FILE}"
    fi
    # if you want node names instead of node indices, use the commented out line below
    ${IMPORTER} $RESULT_DIR/$INPUT_FILE_NAME  > $RESULT_DIR/$IMPORTED_FILE
#   ${IMPORTER} $RESULT_DIR/$INPUT_FILE_NAME -v > $RESULT_DIR/$IMPORTED_FILE
fi


function split {
	PARENT_FILE="$1"
	PARTITION_FILE="$PARENT_FILE.part.$BRANCHING_FACTOR"
	
	# split the whole graph via metis into branching factor subgraphs
	$PARTITIONER $RESULT_DIR/$PARENT_FILE $BRANCHING_FACTOR 
	# >& /dev/null
	
	# create the metis versions of partitions
    # verbosity hardcoded since we always want the comment line per vertice
	${CREATE_PARTITIONS} $RESULT_DIR/$PARENT_FILE $RESULT_DIR/$PARTITION_FILE -v
}


CURRENT_FILE_LIST="$IMPORTED_FILE"
listLength=1;
while [ $listLength -gt 0 ]; do
	for nodeToSplit in $CURRENT_FILE_LIST; do
        split $nodeToSplit
        listLength=`expr $listLength - 1`
		currentParentIndex=${nodeToSplit##*.}
		leafIndex=0
		while [ $leafIndex -lt $BRANCHING_FACTOR ]; do
			currentLeafIndex=`expr $currentParentIndex \* $BRANCHING_FACTOR + $leafIndex + 1`
            currentLeafName=$RESULT_DIR/${nodeToSplit%.*}.$currentLeafIndex
            if [ -s $currentLeafName ]; then
                numNodes=`head -n 1 $currentLeafName | sed 's/\([0-9]*\) [0-9]* [0-9]*/\1/'`
                if [ $numNodes -ge $CUT_OFF_SIZE ]; then
                    NEXT_FILE_LIST="${nodeToSplit%.*}.$currentLeafIndex $NEXT_FILE_LIST"
                    listLength=`expr 1 + $listLength`
                fi
            fi
			leafIndex=`expr 1 + $leafIndex `
		done
	done
	CURRENT_FILE_LIST=$NEXT_FILE_LIST
	NEXT_FILE_LIST=""
done

${ANNOTATE_PARTITIONS} $RESULT_DIR/${INPUT_FILE_NAME} ${BRANCHING_FACTOR} >$RESULT_DIR/${INPUT_FILE_NAME}.partitioned

if [ -z $INTERMEDIATE ]; then
    rm -Rf ${RESULT_DIR}/*imported*
fi

if [ -n $SORT ]; then
    ${SORT_ALGORITHM} ${RESULT_DIR}/${INPUT_FILE_NAME}.partitioned >$RESULT_DIR/${INPUT_FILE_NAME}.ordered.partitioned
fi
