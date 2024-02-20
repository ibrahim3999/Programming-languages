# Programming-languages

#|BNF
 The grammar:
   
   <PLANG>::=
          1){{poly <AEs>} <AEs>}

   <AEs>::=
        {<AEs> <AE>}|
       {<AE>}
        
   <AE> ::=<num>|{+ <AE> <AE>}|{- <AE> <AE>}|{* <AE> <AE>}|{/ <AE> <AE>}|
          
|#
