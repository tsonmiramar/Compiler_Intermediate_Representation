
float v;
float arg;

float ifnested()
{
   float f;
   if ( arg > 1.0 ) {
      if ( v < 0.5 ) 
         f = v;
      else
         f = 5.0;
   } else {
      f = arg;
   } 
   return f;
}

