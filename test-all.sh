#! /bin/sh

TMPDIR="tmp"

rm -rf $TMPDIR

for i in ./* ; do
  if [ -d "$i" ]; then
    if [ -d "$i/tests" ]; then
	  echo $(basename $i)
	  for INDIR in $i/tests/in* ; do
	    BASE=$(basename $INDIR)
		ID=${BASE#in}
		OUTDIR="$(dirname $INDIR)/out$ID"
		
		for SCRIPT in "$i/*.R" ; do
		  mkdir $TMPDIR
		  if [ "$1" == "-s" ]; then
			R --slave --vanilla --args $INDIR $TMPDIR "<" $SCRIPT > /dev/null 2>/dev/null
		  else
			R --slave --vanilla --args $INDIR $TMPDIR "<" $SCRIPT
		  fi
		  
		  if [ $? -ne 0 ]; then
            echo "  test $ID: R SCRIPT EXECUTION FAILED"
          else
		    diff -Bqrw $OUTDIR $TMPDIR > /dev/null
			
			if [ $? -ne 0 ]; then
			  echo "  test $ID: FAILED"
			else
			  echo "  test $ID: SUCCESS"
			fi
          fi

		  rm -rf $TMPDIR
		done
      done
	fi
  fi
done

