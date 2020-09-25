//incorrect code for fibonacci
main(){
  int n=10;
  fib(n);
}

int fib(int n){
    printf("%d ",n);
    return(fib(n-1)+fib(n-2));
}

int sub(int a, int b){
  return (a-b);
}


int div(int a,int b){
  return (a/b);
}

