vec2 v2;
float x;

float vectfloatassign()
{
   vec2 tmp;

   tmp = v2;
   tmp += x;

   return tmp.x * tmp.y;
}

