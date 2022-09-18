class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class Main1 {
	i: Int; 
	
	main(): Object {
	  {
	  5.copy();
	  "test".copy();
	  true.copy();  
	  "test".length();
	  }
	};
   };

class Main3 inherits IO{
	x: Int;
	main(): Object {{ 
		  let x : String<- "test" in x.length(); 
				   }};
   };

class A1 {

	bar:Int <- foo(new SELF_TYPE);
  
	foo(a:A1):Int { ~7 };
  
  };

class A3 {
	inky(): SELF_TYPE { self };
};

class B2 inherits A3 {
	inky(): SELF_TYPE { new SELF_TYPE };
};

class A4 {
	moo:Object;
	boo():Object {
    { 
      case moo of i:Int => i;
			      b:Bool => b;
			      s:String => s;
		  esac; 
      }
  };
};

class Main4 {
	b:B3 <- new B3;
	main(): Object { 
	  b@A5.foo(5,b,b,b) 
	}; 
   };
   
   class A5 {
	   foo(a:Int, b: B3, c:A5, d:B3) : Int {
		  5
	   };
	 
   };
   
   class B3 inherits A5 {
	   foo(a:Int, b: B3, c:A5, d:B3) : Int {
		  6
	   };  
   };

class Main5 inherits IO{
	x:Bool;
	main(): Object {
	  {
	  isvoid "true";
	   
	isvoid false;
   isvoid x;
	  }
	};
  };
  
Class Main {
	main():C {
	  (new C).init(1,true)
	};
};

(*
class A {
a:Object<-0;
b:Object<-"";
c:Object<-true;
};
	*)