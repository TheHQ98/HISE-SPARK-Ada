with PIN;
with SimpleStack;

package CalculatorManager with SPARK_Mode is
   package SS is new SimpleStack(512, Integer, 0);

   type State_Type is (Locked, Unlocked);
   
   type Calculator is private;
   
   procedure Init(Calc : out Calculator; Master_PIN : PIN.PIN);
   
   function Get_State(Calc : Calculator) return State_Type;
   
   function Get_Master_PIN(Calc : Calculator) return PIN.PIN;
   
   procedure Set_State(Calc : out Calculator; State : State_Type);
   
   procedure Set_Locked(Calc : out Calculator; Master_PIN : PIN.PIN);

   function Check_Stack_Size(Calc : Calculator) return Boolean;
   
   procedure Push(Calc : out Calculator; I: Integer);


   -- TESTING ONLY
   procedure Print_Stack_Size(Calc : Calculator);
private
   type Calculator is record
      Master_PIN : PIN.PIN;
      Current_State : State_Type := Locked;
      Stack : SS.SimpleStack;
   end record;
end CalculatorManager;
