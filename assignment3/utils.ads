package Utils with SPARK_Mode is
   function Is_Valid_PIN(Token : String) return Boolean;

   -- for checking memory address
   function Is_Legal_Number(I : Integer) return Boolean;
end Utils;