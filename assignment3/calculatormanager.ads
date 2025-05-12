with PIN;
use PIN;

package CalculatorManager with SPARK_Mode is

   type State_Type is (Locked, Unlocked);
   
   type Calculator is record
      Master_PIN : PIN.PIN;
      Current_State : State_Type := Locked;
   end record;
   
   procedure Init(Calc : out Calculator; Master_PIN : PIN.PIN);
   
   function Get_State(Calc : Calculator) return State_Type;
   
end CalculatorManager;
