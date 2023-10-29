/*

Sample program to show off an example for noob C compiler

*/


// Referencing an external Cpp module function
extern void printInt(int val);

// Simple Function with a return value
int doMath(int a) {
  int x = a * 5;

  // Conditional Statement
  if(x < 20) {
    printInt(100 + x);
  } else {
    printInt(x);
  }
  
  return x + 3;
}



int main() {
  /* Driver function for the program */
  int a = 10;
  echo(a * 20 + 2);
  echo(a < 20);
  echo(a > 20);
  echo(-a);
  printInt(10);
  printInt(doMath(a));
  printInt(doMath(4));

  // Loop statement
  int b = 0;
  while(b < 10) {
    printInt(b);
    b = b + 1;
  }
  return 0;
}



