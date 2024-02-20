# Programming-languages

#|BNF
 The grammar:
   
   <PLANG>::=
          1){{poly <AEs>} <AEs>}

   <AEs>::=
        1){<AEs> <AE>}|
        2){<AE>}
        
   <AE> ::=
        1)   <num>|
        2)  {+ <AE> <AE>}|
        3)  {- <AE> <AE>}|
        4)  {* <AE> <AE>}|
        5)  {/ <AE> <AE>}|

         
          
|#
