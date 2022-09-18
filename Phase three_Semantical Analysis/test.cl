(*Bad feauture redefinition*)
Class C {
	b : Int;
  c(a: Int): Int {a};
};

Class B inherits C{
	b : Int;
  c(a: Int): Int {a};
  c(l: Int): String {"1"};
  c(l: Int): String {"1"};
  c(l: String): Int {1};
  c(l: Int, b:Int): Int {1};

};

(*Bad attribute*)
Class E {
	b : Int<- "String";
  b : Int;
  c : N; 
  d : SELF_TYPE;
};

(*Bad method*)
Class D {
	b : Int;
  c(a: Int): String {1};
  c(a: Int): String {1};
  e(a: N): N {1};

};

(*Bad formals*)
Class O {
  c(a: Int, a:Int, b:N, c:SELF_TYPE): Int {a};
};

Class Main1{
  main(): Int {1};
  a: Int;
  b: String;
  z: Int <- a+b;
};
