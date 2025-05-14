package Utils with SPARK_Mode is
   function Is_Valid_PIN(Token : String) return Boolean with
   Post =>
      (Is_Valid_PIN'Result =
         (Token'Length = 4 and then
          (for all I in Token'Range => Token (I) in '0' .. '9')));

   -- for checking memory address
   function Is_Legal_Number(I : Integer) return Boolean;
end Utils;
