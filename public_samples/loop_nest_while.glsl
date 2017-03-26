int x;
int loopnest()
{
   int sum;
   int i;
   int j;

   sum = 0;
   i = 0;
   while ( i < 10 ) {
     j = 0;
     while ( j < 2 ) {
       sum =sum + x;
       j = j + 1;
     }
     i = i+1;
   }
   return sum;
}

