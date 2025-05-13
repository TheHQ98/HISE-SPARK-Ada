with PIN;
use PIN;

package body CalculatorManager with SPARK_Mode is
   
   procedure Init(Calc : out Calculator; Master_PIN : PIN.PIN) is
   begin
      Calc.Master_PIN := Master_PIN;
      Calc.Current_State := Locked;
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
   
end CalculatorManager;
