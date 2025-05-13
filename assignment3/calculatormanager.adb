with PIN;

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;

package body CalculatorManager with SPARK_Mode is
   
   procedure Init(Calc : out Calculator; Master_PIN : PIN.PIN) is
   begin
      Calc.Master_PIN := Master_PIN;
      Calc.Current_State := Locked;
      SS.Init(Calc.Stack);
   end Init;
   
   function Get_State(Calc : Calculator) return State_Type is
   begin
      return Calc.Current_State;
   end Get_State;
   
   function Get_Master_PIN(Calc : Calculator) return PIN.PIN is
   begin
      return Calc.Master_PIN;
   end Get_Master_PIN;
   
   procedure Set_State(Calc : out Calculator; State : State_Type) is
   begin
      Calc.Current_State := State;
   end Set_State;
   
   procedure Set_Locked(Calc : out Calculator; Master_PIN : PIN.PIN) is
   begin
      Calc.Master_PIN    := Master_PIN;
      Calc.Current_State := Locked;
   end Set_Locked;
   
   function Check_Stack_Size(Calc : Calculator) return Boolean is
   begin
      if SS.Size(Calc.Stack) < 512 then
         return True;
      else
         return False;
      end if;
   end Check_Stack_Size;

   procedure Push(Calc : out Calculator; I: Integer) is
   begin
      SS.Push(Calc.Stack, I);
   end Push;

   function Check_Stack_Pop(Calc : Calculator) return Boolean is
   begin
      if SS.Size(Calc.Stack) > 0 then
         return True;
      else
         return False;
      end if;
   end Check_Stack_Pop;

   procedure Pop(Calc : out Calculator) is
   I : Integer;
   begin
      SS.Pop (Calc.Stack, I);
   end Pop;

   function Check_Stack_Operation(Calc : Calculator) return Boolean is
   begin
      if SS.Size(Calc.Stack) >= 2 then
         return True;
      else
         return False;
      end if;
   end Check_Stack_Operation;

   procedure Add(Calc : out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);
      K := I + J;
      SS.Push(Calc.Stack, K);
   end Add;

   procedure Subtract(Calc : out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);
      K := I - J;
      SS.Push(Calc.Stack, K);
   end Subtract;

   procedure Multiply(Calc : out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);
      K := I * J;
      SS.Push(Calc.Stack, K);
   end Multiply;
   procedure Divide(Calc : out Calculator) is
   I : Integer;
   J : Integer;
   K : Integer;
   begin
      SS.Pop(Calc.Stack, I);
      SS.Pop(Calc.Stack, J);

      -- check J
      if J = 0 then
         Put_Line ("Can not divide by 0");
         SS.Push(Calc.Stack, J);
         SS.Push(Calc.Stack, I);
         return;
      end if;


      K := I / J;
      SS.Push(Calc.Stack, K);
   end Divide;

   -- TESTING ONLY
   procedure Print_Stack_Size(Calc : Calculator) is

   begin
      Put("Stack Size:");
      Ada.Integer_Text_IO.Put(SS.Size(Calc.Stack));
   end Print_Stack_Size;

end CalculatorManager;
