-- Assignment Pair 45
-- Josh Feng 1266669
-- Justin Zhang 1153289

pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with MyStringTokeniser;
with StringToInteger;
with PIN;
with MemoryStore;
with CalculatorManager;
with Utils;

with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;


procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S    : Lines.MyString;
   
   -- Calculator Manager
   CM : CalculatorManager.Calculator;
   
   -- Commands
   CMD_ADD      : constant Lines.MyString := Lines.From_String("+");
   CMD_SUBTRACT : constant Lines.MyString := Lines.From_String("-");
   CMD_MULTIPLY : constant Lines.MyString := Lines.From_String("*");
   CMD_DIVIDE   : constant Lines.MyString := Lines.From_String("/");
   CMD_PUSH1    : constant Lines.MyString := Lines.From_String("push1");
   CMD_PUSH2    : constant Lines.MyString := Lines.From_String("push2");
   CMD_POP      : constant Lines.MyString := Lines.From_String("pop");
   CMD_LOADFROM : constant Lines.MyString := Lines.From_String("loadFrom");
   CMD_STORETO  : constant Lines.MyString := Lines.From_String("storeTo");
   CMD_REMOVE   : constant Lines.MyString := Lines.From_String("remove");
   CMD_LIST     : constant Lines.MyString := Lines.From_String("list");
   CMD_UNLOCK   : constant Lines.MyString := Lines.From_String("unlock");
   CMD_LOCK     : constant Lines.MyString := Lines.From_String("lock");

begin
   ------------------------------------------------------------------
   --  TESTING ONLY
   ------------------------------------------------------------------


   ------------------------------------------------------------------
   --  Programming Begin, init calculator manager, store the pin
   ------------------------------------------------------------------
   if MyCommandLine.Argument_Count = 1 then
      declare
         Arg : constant String := MyCommandLine.Argument(1);
         USER_PIN : PIN.PIN;
      begin
         if Arg'Length = 4 and
           (for all I in MyCommandLine.Argument(1)'Range => 
                MyCommandLine.Argument(1)(I) >= '0' and MyCommandLine.Argument(1)(I) <= '9')
         then
            USER_PIN := PIN.From_String (Arg);
            CalculatorManager.Init(CM, USER_PIN);
         else
            Put_Line("SYSTEM: You must provide a four-digit PIN.");
            return;
         end if;
      end;
   else
      Put_Line("SYSTEM: You must provide a four-digit PIN.");
      return;
   end if;
   
   ------------------------------------------------------------------
   --  Main Loop
   ------------------------------------------------------------------
   loop
      if CalculatorManager."="(CalculatorManager.Get_State(CM), CalculatorManager.Locked) then
         Put("locked> ");
      elsif CalculatorManager."="(CalculatorManager.Get_State(CM), CalculatorManager.Unlocked) then
         Put("unlocked> ");
      end if;
      
      Lines.Get_Line(S);
      
      -- Check invalid input
      if Lines.length(S) > 2048 then
         Put_Line("SYSTEM: You input lines longer than 2028 characters.");
         return;
      end if; 
      
      declare
         T         : MyStringTokeniser.TokenArray(1 .. 5) := (others => (Start => 1, Length => 0));
         NumTokens : Natural;
         Command   : Lines.MyString := Lines.From_String ("");
         Token1    : Lines.MyString := Lines.From_String ("");
         Token2    : Lines.MyString := Lines.From_String ("");
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(S), T, NumTokens);
         
         -- if token > 3, exit
         if NumTokens > 3 then
            Put_Line("SYSTEM: you input more than 3 tokens");
            return;
         end if;
         
         -- NumTokens = 3
         if NumTokens = 3 then
            Command := Lines.Substring(S, T(1).Start, T(1).Start + T(1).Length - 1);
            Token1  := Lines.Substring(S, T(2).Start, T(2).Start + T(2).Length - 1);
            Token2  := Lines.Substring(S, T(3).Start, T(3).Start + T(3).Length - 1);
         end if;
         
         -- NumTokens = 2
         if NumTokens = 2 then
            Command := Lines.Substring(S, T(1).Start, T(1).Start + T(1).Length - 1);
            Token1  := Lines.Substring(S, T(2).Start, T(2).Start + T(2).Length - 1);
         end if;

         -- NumTokens = 1
         if NumTokens = 1 then
            Command := Lines.Substring(S, T(1).Start, T(1).Start + T(1).Length - 1);
         end if;
         
         -- lock state
         if CalculatorManager."="(CalculatorManager.Get_State(CM), CalculatorManager.Locked) then
            -- unlock
            if Lines.Equal(Command, CMD_UNLOCK) then
               declare
                  temp_pin : PIN.PIN;
               begin
                  if Utils.Is_Valid_PIN(Lines.To_String(Token1)) = True then
                     pragma Assert (Lines.To_String (Token1)'Length = 4);
                     temp_pin := PIN.From_String(Lines.To_String(Token1));
                     if PIN."="(temp_pin, CalculatorManager.Get_Master_PIN(CM)) then
                        -- change CM state to unlocked state
                        CalculatorManager.Set_State(CM, CalculatorManager.Unlocked);
                     else
                        Put_Line("Wrong PIN.");
                     end if;
                  else
                     Put_Line ("Wrong Input: " & Lines.To_String(Token1));
                     return;
                  end if;
               end;
            elsif Lines.Equal(Command, CMD_LOCK) then
               Put_Line("Already locked");
            else
               Put_Line("Unexpect input");
               return;
            end if;
         -- unlock state
         elsif CalculatorManager."="(CalculatorManager.Get_State(CM), CalculatorManager.Unlocked) then
            -- lock the calculator
            if Lines.Equal(Command, CMD_LOCK) then
               -- check token
               if NumTokens /= 2 then
                  Put_Line ("SYSTEM: Token number not correct");
                  return;
               end if;

               declare
                  temp_pin : PIN.PIN;
               begin
                  if Utils.Is_Valid_PIN(Lines.To_String(Token1)) = False then
                     Put_Line ("Wrong Input: " & Lines.To_String(Token1));
                     return;
                  end if;

                  temp_pin := PIN.From_String(Lines.To_String(Token1));
                  CalculatorManager.Set_Locked(CM, temp_pin);
               end;
            -- push1
            elsif Lines.Equal(Command, CMD_PUSH1) then
               -- check token
               if NumTokens /= 2 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Size(CM) = False then
                  Put_Line("Cannot push anymore");
               else
                  CalculatorManager.Push(CM, StringToInteger.From_String(Lines.To_String(Token1)));
               end if;
            -- push2
            elsif Lines.Equal(Command, CMD_PUSH2) then
               -- check token
               if NumTokens /= 3 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Size(CM) = False then
                  Put_Line("Cannot push anymore");
               else
                  CalculatorManager.Push(CM, StringToInteger.From_String(Lines.To_String(Token1)));
                  if CalculatorManager.Check_Stack_Size(CM) = False then
                     Put_Line("Cannot push anymore");
                  else
                     CalculatorManager.Push(CM, StringToInteger.From_String(Lines.To_String(Token2)));
                  end if;
               end if;
            -- pop
            elsif Lines.Equal(Command, CMD_POP) then
               if NumTokens /= 1 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Pop(CM) = False then
                  Put_Line("Cannot pop anymore");
               else
                  CalculatorManager.Pop(CM);
               end if;
            -- operators +
            elsif Lines.Equal(Command, CMD_ADD) then
               if NumTokens /= 1 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Operation(CM) = False then
                  Put_Line("no enough for operation");
               else
                  CalculatorManager.Add(CM);
               end if;
            -- operators -
            elsif Lines.Equal(Command, CMD_SUBTRACT) then
               if NumTokens /= 1 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Operation(CM) = False then
                  Put_Line("no enough for operation");
               else
                  CalculatorManager.Subtract(CM);
               end if;
            -- operators *
            elsif Lines.Equal(Command, CMD_MULTIPLY) then
               if NumTokens /= 1 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Operation(CM) = False then
                  Put_Line("no enough for operation");
               else
                  CalculatorManager.Multiply(CM);
               end if;
            -- operators /
            elsif Lines.Equal(Command, CMD_DIVIDE) then
               if NumTokens /= 1 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               elsif CalculatorManager.Check_Stack_Operation(CM) = False then
                  Put_Line("no enough for operation");
               else
                  CalculatorManager.Divide(CM);
               end if;
            -- storeTo
            elsif Lines.Equal(Command, CMD_STORETO) then
               declare
                  Address : MemoryStore.Location_Index;
               begin
                  if NumTokens /= 2 then
                     Put_Line("SYSTEM: Token number not correct");
                     return;
                  elsif CalculatorManager.Check_Stack_Pop(CM) = False then
                     Put_Line("No more number in stack");
                  elsif Utils.Is_Legal_Number (StringToInteger.From_String(Lines.To_String(Token1))) = False then
                     Put_Line ("Please type a legal address: 1...256");
                  else
                     Address := StringToInteger.From_String(Lines.To_String(Token1));
                     CalculatorManager.Store(CM, Address);
                  end if;
               end;
            -- list
            elsif Lines.Equal(Command, CMD_LIST) then
               if NumTokens /= 1 then
                  Put_Line("SYSTEM: Token number not correct");
                  return;
               else
                  CalculatorManager.List(CM);
               end if;
            -- remove
            elsif Lines.Equal(Command, CMD_REMOVE) then
               declare
                     Address : MemoryStore.Location_Index;
               begin
                  if NumTokens /= 2 then
                     Put_Line("SYSTEM: Token number not correct");
                        return;
                  elsif Utils.Is_Legal_Number (StringToInteger.From_String(Lines.To_String(Token1))) = False then
                     Put_Line ("Please type a legal address: 1...256");
                  else
                     Address := StringToInteger.From_String(Lines.To_String(Token1));
                     CalculatorManager.Remove(CM, Address);
                  end if;
               end;
            elsif Lines.Equal(Command, CMD_LOADFROM) then
               declare
                     Address : MemoryStore.Location_Index;
               begin
                  if NumTokens /= 2 then
                     Put_Line("SYSTEM: Token number not correct");
                        return;
                  elsif Utils.Is_Legal_Number (StringToInteger.From_String(Lines.To_String(Token1))) = False then
                     Put_Line ("Please type a legal address: 1...256");
                  else
                     Address := StringToInteger.From_String(Lines.To_String(Token1));
                     CalculatorManager.Load(CM, Address);
                  end if;
               end;
            else
               Put_Line("SYSTEM: uncorrect input");
               return;
            end if;
         end if;
         

         -- ENDING LOOP TESTING
         --  CalculatorManager.Print_Stack_Size (CM);
         --  New_Line;
      end;
      
   end loop; 
end Main;
