class Foo {
    x:Int;

	bar1():Int{let a:Int in a + let b:String in b};

    f():Int{{
        a+b-c;
        a-b+c;

        a+b*c;
        a*b+c;

        a+b/c;
        a/b+c;

        a-b*c;
        a*b-c;

        a-b/c;
        a/b-c;

        a*b/c;
        a/b*c;
        }};

    bar():Object{foo <- 3 };
    main():Object { {x <- y <- ~(5);}};

    f2(x : Int) : Object {
      let x : Int <- 3 in
        let y : Bool, z : Object in
          let a : Object, b : Object, c : Object in
            let d : Int <- 6 in
                d + 5
      };

    foo:Test;
    bar3():Int { case foo.bar() of y:Int => 3;
                                  z:String => 4;
                                  x:Test => 5; esac };

    baz():String { let x:Int <- 5, y:String <- "biddle", z:Int in x};
        
};


