(* bad block *)
class A {
    f() : Object { {
      5;
      A;
      3;
      +;
    } };
  };

(*4 bad dispatches *)
  class Test {
    test(x:Int):String {x(5,4,3,)};
    ok(x:Int):String {x(5,4,3)};

    test(x:Int):String {x(,5,4,)};

    test(x:Int):String {x(4;3;)};

    test(x:Int):String {x(,)};
 };

 (* bad expression list *)
 class A2 {
        f():Int{ {;} };
};

 (* bad feature *)
class A3 {
    x : int;
  };

(* if and no else *)
  class Test2 {
    foo:Test;
    bar(x:Int;y:Int):Int {if true then 4 fi};
  };

(* error in first binding of let *)
class Foo {
	bar():Int{{let a:Int<-Bork, b:Int<-5, c:Int<-6 in a + B;}};
};

(* error in second binding of let *)
class Foo2 {
	bar():Int{{let a:Int<-4, b:Int<-Bork, c:Int<-6 in a + B;}};
};

(* bad class name *)
  class test {
    foo:Test;
    bar():Int {5};
  };
  
  class recover {
  };
  