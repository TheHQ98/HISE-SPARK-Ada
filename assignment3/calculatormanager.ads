with PIN;

package CalculatorManager with SPARK_Mode is

   type State_Type is (Locked, Unlocked);
   
   type Calculator is private;
   
   procedure Init(Calc : out Calculator; Master_PIN : PIN.PIN);
   
   function Get_State(Calc : Calculator) return State_Type;
   
   function Get_Master_PIN(Calc : Calculator) return PIN.PIN;
   
   procedure Set_State(Calc : out Calculator; State : State_Type);
   
   procedure Set_Locked(Calc : out Calculator; Master_PIN : PIN.PIN);
   
private
   type Calculator is record
      Master_PIN : PIN.PIN;
      Current_State : State_Type := Locked;
   end record;
end CalculatorManager;
