(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
	if char = "0" then 0 else
	if char = "1" then 1 else
	if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(*
    i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(*
    i2a_aux is an example using recursion.
*)		
    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};


(* //////////////////////////////////////////////////////
   The above code is the atoi file from ~examples that 
   is used to convert strings to integers.
  ///////////////////////////////////////////////////////
  The below code is my object oriented implementation 
  of a stack machine using string manipulation. 
  Zahra Rabbany, 610398124
  /////////////////////////////////////////////////////// *)
  

class StackCommand inherits A2I{
    -- This is the main parent class, the init function
    -- detects the command and calls the appropriate subclass.
	init(input: String, stack: String): String {
		if (input="e") then stack <-(new Eval).eval(stack) else
		if (input="d") then stack <-(new Disp).disp(stack) else
		stack <- (new Push).push(input, stack)
		fi fi
	}; 
};

class Push inherits StackCommand {
  	-- Pushes a '+' or 's' or an integer to the stack.
	push(char: String, stack: String): String {stack.concat(char.concat(","))};
};

class Eval inherits StackCommand {
  	-- The eval function evaluates the command and 
  	-- calls the method for '+' or 's' command.
  
  	cmd:  String; 
	top:  String; 
	pretop: String;
  	pointer: Int;
  	len: Int;
  
	eval(stack: String): String {{
		--the stack is empty!
		if (stack=",") then cmd <- "" else 
		cmd <- stack.substr(stack.length()-2,1)
      		fi;
      
		if (cmd="s") then stack<-swap(stack.substr(0,stack.length()-2)) else
		if (cmd="+") then stack<-sum(stack.substr(0,stack.length()-2))  else
		stack --top is an integer!
		fi fi;
      }
	};
	
	swap(stack: String): String {{
      -- Swaps the top two stack members.
      
      	-- find the top of stack
      	pointer <- stack.length()-2;
        len <- 0;
        while (not stack.substr(pointer,1)=",") loop{
          	   pointer <- pointer-1;
               len <- len+1;}
     	pool;
      	top    <- (stack.substr(pointer+1,len)).concat(","); 

      	-- find the second top of stack
      	pointer <- pointer-1;
        len <- 0;
        while (not stack.substr(pointer,1)=",") loop{
          	   pointer <- pointer-1;
               len <- len+1;
        } 
        pool;
		pretop <- (stack.substr(pointer+1,len)).concat(",");  
      	
      	-- do the swap
		stack <- ((stack.substr(0,pointer+1)).concat(top)).concat(pretop);
		}
	};
	
	sum(stack: String): String {{
       -- Adds the top two stack members and
       -- pushes it pack onto the stack.
      
        -- find the top of stack
      	pointer <- stack.length()-2;
        len <- 0;
        while (not stack.substr(pointer,1)=",") loop{
          	   pointer <- pointer-1;
               len <- len+1;}
     	pool;
      	top    <- stack.substr(pointer+1,len); 

      	-- find the second top of stack
      	pointer <- pointer-1;
        len <- 0;
        while (not stack.substr(pointer,1)=",") loop{
          	   pointer <- pointer-1;
               len <- len+1;
        } 
        pool;
		pretop <- stack.substr(pointer+1,len);  
      	
      	-- do the summation
		stack <- ((stack.substr(0,pointer+1)).concat((i2a(a2i(top)+a2i(pretop))))).concat(",");
		}
	};

};

class Disp inherits StackCommand {
   -- Displays the stack one member on each line.
  	pointer: Int;
  	len: Int;
  	 
	disp(stack: String): String {{
        
      	pointer <- stack.length()-2;
    
        while (0<pointer) loop{
          	len <- 0; 
          	    
            while (not stack.substr(pointer,1)=",") loop{
          	   pointer <- pointer-1;
               	   len <- len+1;
          } 
            pool;
	
	(new IO).out_string(stack.substr(pointer+1,len).concat("\n"));
	pointer <- pointer-1;}
	
        pool;
      
        stack;}
  };
};

class Main inherits IO{
  
	input: String;
	stack: String;
  
	main(): Object {{
      
      	  stack <- ",";
	  out_string(">");
	  input <- in_string();
      
      -- Main loop of the program, 
      -- would loop until "x" is received.
	  while (not (input="x")) loop {
	       stack <- (new StackCommand).init(input, stack);
	       out_string(">");
	       input <- in_string();}
	  pool;
		}
	};
};
