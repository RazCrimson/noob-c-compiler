extern void printInt(int val);

int doMath(int a) {
  int x = a * 5;
  if(x < 20) {
    printInt(100 + x);
  } else {
    printInt(x);
  }
  return x + 3;
}

int main() {
  int a = 10;
  echo(a * 20 + 2);
  echo(a < 20);
  echo(a > 20);
  echo(-a);
  printInt(10);
  printInt(doMath(a));
  printInt(doMath(4));

  int b = 0;
  while(b < 10) {
    printInt(b);
    b = b + 1;
  }
  return 0;
}



