class C2 {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C2 {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class A0 {
	x:A0 <- self;
	foo(x:A0) : A0 {self <- self.copy()};
  };

class A1 {
	self:A1 <- new A1;
	foo(x:String) : String {x <- "a"};
  };

class A2 {
	x:String;
	foo(x:A2) : A2 {x};
  };
  
class B0 inherits A2 {
	foo(x:A2,y:A2) : A2 {self};
  };

class Main1 {
	main () :Int {let self:Int in 0};
	};

Class Main {
	main():C {
	 {
	  (new C2).init(1,1);
	  (new C2).init(1,true,3);
	  (new C2).iinit(1,true);
	  (new C2);

	 }
	};
};

Class B1 {
	i: Int;
	j: String;
  };
  Class C1 inherits B1{
	s: Int;
	e: String <- i ;
	l: Bool <- s;
  };

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

Class Main2{
  main(): Int {1};
  a: Int;
  b: String;
  z: Int <- a+b;
};

(*
(*
   Includes all possible errors in classes. 
   Are fatal so we keep them commented.
      1. Class Main is not defined.
      2. No 'main' method in class Main. X
      3. Class C was previously defined. 
      4. Class C, or an ancestor of C, is involved in an inheritance cycle.
      6. Class C cannot inherit class Int/Bool/String/SELF_TYPE. 
      7. Class C inherits from an undefined class A. 
      8. Redefinition of basic class Object/IO/Int/Bool/String/SELF_TYPE.
*)

Class C {
	b : Int;
};

Class C inherits E{ (*3*)
	b : Int;
};

Class X inherits Y{ (*4*)
	b : Int;
};

Class Y inherits Z{
	b : Int;
};

Class Z inherits X{
	b : Int;
};

Class D inherits A{ (*7*)
  a : Int;
  };


Class E inherits Bool{ (*6*)
    a : Int;
  };

Class String{ (*8*)
    a : Int;
  };

*)