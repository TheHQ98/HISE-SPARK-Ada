package Utils with SPARK_Mode is
   function Is_Valid_PIN(Token : String) return Boolean with
   Post =>
      (Is_Valid_PIN'Result =
         (Token'Length = 4 and then
          (for all I in Token'Range => Token (I) in '0' .. '9')));

   -- for checking memory address
   function Is_Legal_Number(I : Integer) return Boolean with
    Post =>
      (Is_Legal_Number'Result = True  and then I in 1 .. 256)    or
      (Is_Legal_Number'Result = False and then I not in 1 .. 256);
end Utils;
