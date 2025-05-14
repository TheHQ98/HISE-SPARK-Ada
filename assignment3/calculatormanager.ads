with PIN;
with SimpleStack;
with MemoryStore;

package CalculatorManager with SPARK_Mode is
   package SS is new SimpleStack(512, Integer, 0);

   type State_Type is (Locked, Unlocked);
   
   type Calculator is private;
   
   procedure Init(Calc : out Calculator; Master_PIN : in PIN.PIN);
   
   function Get_State(Calc : in Calculator) return State_Type;
   
   function Get_Master_PIN(Calc : in Calculator) return PIN.PIN;
   
   procedure Set_State(Calc : in out Calculator; State : in State_Type);
   
   procedure Set_Locked(Calc : in out Calculator; Master_PIN : in PIN.PIN);

   -- Check for can push anymore token into the stack
   function Check_Stack_Size(Calc : in Calculator) return Boolean;
   
   procedure Push(Calc : in out Calculator; I: in Integer);

   -- Check for can pop from the stack
   function Check_Stack_Pop(Calc : in Calculator) return Boolean;

   procedure Pop(Calc : in out Calculator);

   -- Check for can operation from the stack
   function Check_Stack_Operation(Calc : in Calculator) return Boolean;
   
   procedure Add(Calc : in out Calculator);
   procedure Subtract(Calc : in out Calculator);
   procedure Multiply(Calc : in out Calculator);
   procedure Divide(Calc : in out Calculator);

   procedure Store(Calc : in out Calculator; Address : in MemoryStore.Location_Index);

   procedure List(Calc : in Calculator);
   
   procedure Remove(Calc : in out Calculator; Address : in MemoryStore.Location_Index);

   procedure Load(Calc : in out Calculator; Address : in MemoryStore.Location_Index);


   -- TESTING ONLY
   procedure Print_Stack_Size(Calc : Calculator);
private
   type Calculator is record
      Master_PIN : PIN.PIN;
      Current_State : State_Type := Locked;
      Stack : SS.SimpleStack;
      DB  : MemoryStore.Database;
   end record;
end CalculatorManager;
