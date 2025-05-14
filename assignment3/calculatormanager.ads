with PIN;
with SimpleStack;
with MemoryStore;

package CalculatorManager with SPARK_Mode is
   package SS is new SimpleStack(512, Integer, 0);

   type State_Type is (Locked, Unlocked);
   
   type Calculator is private;
   
   procedure Init(Calc : out Calculator; Master_PIN : in PIN.PIN) with
   Post => (
      CalculatorManager.Get_State(Calc) = Locked
      and
      PIN."="(CalculatorManager.Get_Master_PIN(Calc), Master_PIN)
   );
   
   procedure Set_Unlocked(Calc : in out Calculator) with
   Pre  => CalculatorManager.Get_State (Calc) = Locked,
   Post => (
      CalculatorManager.Get_State(Calc) = Unlocked
   );
   
   procedure Set_Locked(Calc : in out Calculator; Master_PIN : in PIN.PIN) with
   Pre  => CalculatorManager.Get_State (Calc) = Unlocked,
   Post => (
      CalculatorManager.Get_State(Calc) = Locked
      and
      PIN."="(CalculatorManager.Get_Master_PIN(Calc), Master_PIN)
   );

   -- Check for can push anymore token into the stack
   function Check_Stack_Size(Calc : in Calculator) return Boolean with
   Pre => CalculatorManager.Get_State (Calc) = Unlocked,
   Post => (
      Check_Stack_Size'Result = (SS.Size (Get_Stack (Calc)) < 512)
   );
   
   procedure Push(Calc : in out Calculator; I: in Integer) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked
      and
      SS.Size(CalculatorManager.Get_Stack(Calc)) < 512
      and 
      I >= Integer'First 
    	and 
      I <= Integer'Last
   ),
   Post => (
      SS.Size (Get_Stack (Calc)) = SS.Size (Get_Stack (Calc'Old)) + 1
      and
      SS.Storage(CalculatorManager.Get_Stack(Calc), SS.Size(CalculatorManager.Get_Stack(Calc))) = I
   );

   -- Check for can pop from the stack
   function Check_Stack_Pop(Calc : in Calculator) return Boolean with
   Post => (Check_Stack_Pop'Result =
               (SS.Size (Get_Stack (Calc)) > 0));

   procedure Pop(Calc : in out Calculator) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      SS.Size (Get_Stack (Calc)) > 0
   ),
   Post => (
      SS.Size (Get_Stack (Calc)) = SS.Size (Get_Stack (Calc'Old)) - 1
   );

   -- Check for can operation from the stack
   function Check_Stack_Operation(Calc : in Calculator) return Boolean with
   Post => (
      Check_Stack_Operation'Result = (SS.Size(Get_Stack(Calc))>= 2)
   );
   
   procedure Add(Calc : in out Calculator) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      SS.Size(Get_Stack (Calc)) >= 2
   ),
   Post => (
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old)) - 1
      or 
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old))
   );

   procedure Subtract(Calc : in out Calculator) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      SS.Size(Get_Stack (Calc)) >= 2
   ),
   Post => (
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old)) - 1
      or 
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old))
   );

   procedure Multiply(Calc : in out Calculator) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      SS.Size(Get_Stack (Calc)) >= 2
   ),
   Post => (
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old)) - 1
      or 
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old))
   );

   procedure Divide(Calc : in out Calculator) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      SS.Size(Get_Stack (Calc)) >= 2
   ),
   Post => (
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old)) - 1
      or 
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old))
   );

   procedure Store(Calc : in out Calculator; Address : in MemoryStore.Location_Index) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      MemoryStore.Length (CalculatorManager.Get_DB(Calc)) < MemoryStore.Max_Locations
      and
      SS.Size(Get_Stack (Calc)) > 0
   ),
   Post => (
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old)) - 1
   );

   procedure List(Calc : in Calculator) with  Depends => (null => Calc);
   
   procedure Remove(Calc : in out Calculator; Address : in MemoryStore.Location_Index) with
   pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      MemoryStore.Has (CalculatorManager.Get_DB(Calc), Address)
   );

   procedure Load(Calc : in out Calculator; Address : in MemoryStore.Location_Index) with
   Pre => (
      CalculatorManager.Get_State(Calc) = Unlocked 
      and
      MemoryStore.Has(CalculatorManager.Get_DB(Calc), Address)
      and
      SS.Size(Get_Stack (Calc)) < 512
   ),
   Post => (
      SS.Size(Get_Stack (Calc)) = SS.Size(Get_Stack (Calc'Old)) + 1
   );

   function Get_Stack(Calc : in Calculator) return SS.SimpleStack;

   function Get_DB(Calc : in Calculator) return MemoryStore.Database;

   function Get_State(Calc : in Calculator) return State_Type;
   
   function Get_Master_PIN(Calc : in Calculator) return PIN.PIN;

   function Get_Stack_Size(Calc : in Calculator) return Integer;

   -- TESTING ONLY
   procedure Print_Stack_Size(Calc : Calculator);

private
   type Calculator is record
      Master_PIN : PIN.PIN;
      Current_State : State_Type := Locked;
      Stack : SS.SimpleStack;
      DB  : MemoryStore.Database;
   end record;

   function Get_Stack(Calc : in Calculator) return SS.SimpleStack is
   (Calc.Stack);

   function Get_DB(Calc : in Calculator) return MemoryStore.Database is
   (Calc.DB);

   function Get_State(Calc : in Calculator) return State_Type is
   (Calc.Current_State);
   
   function Get_Master_PIN(Calc : in Calculator) return PIN.PIN is
   (Calc.Master_PIN);

   function Get_Stack_Size(Calc : in Calculator) return Integer is
   (SS.Size (Calc.Stack));

end CalculatorManager;
