with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;

package body Utils with SPARK_Mode is
   function Is_Valid_PIN(Token : String) return Boolean is
   begin
      if Token'length = 4 and then
         (for all I in Token'Range => Token(I) >= '0' and Token(I) <= '9')
      then
         return True;
      else
         return False;
      end if;
   end Is_Valid_PIN;

   function Is_Legal_Number(I : Integer) return Boolean is
   begin
      if I <= 0 then
         return False;
      elsif I > 256 then
         return False;
      else
         return True;
      end if;
   end Is_Legal_Number;

end Utils;