ls public_samples | grep glsl > AllTest.txt
ls public_samples | grep out > OutTest.txt

while read -r LINE; do
   FILENAME=`echo $LINE | cut -d'.' -f1` 
   ./glc < public_samples/$FILENAME.glsl > public_samples/$FILENAME.bc;
   echo "========"
   ./gli public_samples/$FILENAME.bc > public_samples/$FILENAME.myoutput;
   echo "========"
   diff public_samples/$FILENAME.myoutput public_samples/$FILENAME.out 
   echo "========"
done < AllTest.txt
