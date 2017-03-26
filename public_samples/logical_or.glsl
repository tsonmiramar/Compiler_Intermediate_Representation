int a;
float b;

float logictest(int x, float y)
{
   if ( x > a || y > b )
     return y - b;
   else
     return b - y;
}
