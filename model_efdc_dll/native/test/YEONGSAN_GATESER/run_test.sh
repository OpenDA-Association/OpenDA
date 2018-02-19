#!/usr/bin/env sh
EXEC="../../efdc_fortran_dll_test/ensemble_simulation"
LD_LIBRARY_PATH="$LD_LIBRARY_PATH:../../../native_bin/linux64_gnu/lib" 
#export $LD_LIBRARY_PATH

PARENT_DIR="/v3/D06b_uitbreiding_zuid_korea/openda/public/model_efdc_dll/native/test/YEONGSAN_GATESER"
MODEL_DIR="$PARENT_DIR/model"
NR_INSTANCES=3

# clone model directory into three work directories
echo -e "\n----------------------\nSetting up directories\n----------------------\n"
echo "Model directory: $MODEL_DIR"

echo "Creating $NR_INSTANCES work directories:"
for (( i = 0 ; i <  NR_INSTANCES; i++ ))
do
    INSTANCE_DIR="$PARENT_DIR/work$i"
    if [ -f="$INSTANCE_DIR" ]
        then
        echo "Delete old work directory"
        rm -rf "$INSTANCE_DIR"
    fi
    echo "creating $INSTANCE_DIR"
    cp -r $MODEL_DIR "$INSTANCE_DIR"
done

# test differences
echo -e "\n-------------------\nRunning executable: $EXEC\n-------------------\n"
$EXEC $PARENT_DIR $MODEL_DIR $NR_INSTANCES |  grep HDMT

echo -e "\n-----------------\nComparing results\n-----------------\n"
for (( i = 1 ; i <  NR_INSTANCES; i++ ))
do 
    echo "Comparing directory work$i to work0"
    if [[ -n $(diff "work0" "work$i") ]]; then
        echo "Differences observed"
    else
        echo "No differences found"
    fi
    
done

