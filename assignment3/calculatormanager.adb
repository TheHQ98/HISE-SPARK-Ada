with CalculatorManager;
with PIN;
with MemoryStore;

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;

package body CalculatorManager with SPARK_Mode is
   
   procedure Init(Calc : out Calculator; Master_PIN : in PIN.PIN) is
   begin
      Calc.Master_PIN := Master_PIN;
      Calc.Current_State := Locked;
      SS.Init(Calc.Stack);
      MemoryStore.Init(Calc.DB);
   end Init;
   
   procedure Set_Unlocked(Calc : in out Calculator) is
   begin
      Calc.Current_State := Unlocked;
   end Set_Unlocked;
   
   procedure Set_Locked(Calc : in out Calculator; Master_PIN : in PIN.PIN) is
   begin
      Calc.Master_PIN    := Master_PIN;
      Calc.Current_State := Locked;
   end Set_Locked;
   
   function Check_Stack_Size(Calc : in Calculator) return Boolean is
   begin
      if SS.Size(Calc.Stack) < 512 then
         return True;
      else
         return False;
      end if;
   end Check_Stack_Size;

   procedure Push(Calc : in out Calculator; I: in Integer) is
   begin
      SS.Push(Calc.Stack, I);
   end Push;

   function Check_Stack_Pop(Calc : in Calculator) return Boolean is
   begin
      if SS.Size(Calc.Stack) > 0 then
         return True;
      else
         return False;
      end if;
   end Check_Stack_Pop;

   procedure Pop(Calc : in out Calculator) is
   I : Integer;
   begin
      SS.Pop (Calc.Stack, I);
      pragma Unreferenced (I);
   end Pop;

   function Check_Stack_Operation(Calc : in Calculator) return Boolean is
   begin
      if SS.Size(Calc.Stack) >= 2 then
         return True;
      else
         return False;
      end if;
   end Check_Stack_Operation;

   procedure Add(Calc : in out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);

      if (I > 0 and then J > Integer'Last - I) or (I < 0 and then J < Integer'First - I) then
         Put_Line ("Overflow on addition, operation cancelled");
         SS.Push (Calc.Stack, J);
         SS.Push (Calc.Stack, I);
      else
         K := I + J;
         SS.Push(Calc.Stack, K);
      end if;
   end Add;

   procedure Subtract(Calc : in out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);

      if (J = Integer'First) then
         Put_Line ("Overflow on subtraction, operation cancelled");
         SS.Push (Calc.Stack, J);
         SS.Push (Calc.Stack, I);
         return;
      end if;

      declare
         J1 : constant Integer := -J;
      begin
         if (I > 0 and then J1 > Integer'Last - I) or (I < 0 and then J1 < Integer'First - I) then
            Put_Line ("Overflow on subtraction, operation cancelled");
            SS.Push (Calc.Stack, J);
            SS.Push (Calc.Stack, I);
            return;
         else
            K := I - J;
            SS.Push (Calc.Stack, K);
         end if;
      end;
   end Subtract;

   procedure Multiply(Calc : in out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);
      
      declare
         LongNumber : Long_Long_Integer :=
               Long_Long_Integer (I) * Long_Long_Integer (J);
      begin
         if LongNumber < Long_Long_Integer (Integer'First)
            or else LongNumber > Long_Long_Integer (Integer'Last)
         then
            Put_Line ("Overflow on multiplication, operation cancelled");
            SS.Push (Calc.Stack, J);
            SS.Push (Calc.Stack, I);
            return;
         else
            K := Integer (LongNumber);
            SS.Push (Calc.Stack, K);
         end if;
      end;
   end Multiply;
   
   procedure Divide(Calc : in out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);

      if (I > 0 and then J > Integer'Last - I) or (I < 0 and then J < Integer'First - I) then
         Put_Line ("Overflow on divide, operation cancelled");
         SS.Push (Calc.Stack, J);
         SS.Push (Calc.Stack, I);
      elsif
         J = 0 then
         Put_Line ("Can not divide by 0, operation cancelled");
         SS.Push(Calc.Stack, J);
         SS.Push(Calc.Stack, I);
         return;
      else
         K := I / J;
         SS.Push(Calc.Stack, K);
      end if;
   end Divide;

   procedure Store(Calc : in out Calculator; Address : in MemoryStore.Location_Index) is
   I : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      MemoryStore.Put (Calc.DB, Address, MemoryStore.Int32(I));
   end;

   procedure List(Calc : in Calculator) is
   begin
      MemoryStore.Print (Calc.DB);
   end List;

   procedure Remove(Calc : in out Calculator; Address : in MemoryStore.Location_Index) is 
   begin
      MemoryStore.Remove (Calc.DB, Address);
   end Remove;

   procedure Load(Calc : in out Calculator; Address : in MemoryStore.Location_Index) is
   I : MemoryStore.Int32;
   begin
      if MemoryStore.Has (Calc.DB, Address) = False then
         Put_Line ("Your input address cannot find any number");
      else
         I := MemoryStore.Get (Calc.DB, Address);
         SS.Push (Calc.Stack, Integer(I));
      end if;
   end Load;

end CalculatorManager;
