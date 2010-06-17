
# [2010.06.17] If I'm going to keep these multiple run_all_* scripts around I should factor out their commonalities:


function check_error() {
  CODE=$1
  MSG=$2
  # Error code 143 was a timeout
  if [ "$CODE" == "143" ]
  then echo " #      Return code $CODE Params: $CNC_VARIANT $CNC_SCHEDULER $FLAGS" | tee -a $RESULTS
       echo " #      Process TIMED OUT!!" | tee -a $RESULTS
  elif [ "$CODE" != "0" ]
  then echo " # $MSG" | tee -a $RESULTS
       echo " #      Error code $CODE Params: $CNC_VARIANT $CNC_SCHEDULER $FLAGS"  | tee -a $RESULTS
       echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
       if [ "$NONSTRICT" == "" ];
       then exit $CODE
       fi
  fi
}
