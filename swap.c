#include <stdio.h>

int x, y, z;

void prxy() {printf("x:%d, y:%d\n", x, y); }
void init() {x = 1; y = 8; printf("Initial values: "); prxy(); }

int main()
{ 
  /* naive */
  init(); printf("Naive (non-)solution:\n");        
  y = x;
  x = y;
  prxy();
  
  /* orthodox */
  init(); printf("Orthodox solution:\n");  
  z = y;         
  y = x;
  x = z;
  prxy();

  /* hacker 1 */
  init(); printf("First hacker solution:\n");  
  y = x + 0*(x = y);
  prxy();

  /* hacker 2 */
  init(); printf("Second hacker (non-)solution:\n");  
  y = 0*(x = y) + x;
  prxy();
}
