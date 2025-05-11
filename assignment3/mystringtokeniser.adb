
package body MyStringTokeniser with SPARK_Mode is



   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      Processed : Natural := 0;
      OutIndex : Integer := Tokens'First;
   begin
      Count := 0;
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Processed < Tokens'Length loop
         -- It is check for all the elements that have been store into the Tokens, i.e., Tokens'First..OutIndex-1
         -- 1. For each of start index of token must greater or equal to the first index of S, avoid access outside of boundary
         -- 2. Each Token length must greater than 0, otherwise will access illegal index range
         -- 3. If 1 and 2 both true, then check,
         --    the posistion of the last character of the token must smaller or equal to the last index of the string S
         --    ensure that token not access the right boundary of the string to access illegal memory
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

         -- Each time, OutIndex equal to the index of first token + number f processed
         -- Ensuring that OutIndex not duplicates, no able to write to the same position twice
         -- ensuring that OutIndex not missing any position in the array, and
         -- ensuring that OutIndex not out of boundary and always within legal index range
         pragma Loop_Invariant (OutIndex = Tokens'First + Processed);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index
              and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last)
              and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Processed := Processed + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               Count := Processed;
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- advance to first character after the token
            Index := Index + Extent.Length;
         end if;
      end loop;
      Count := Processed;
   end Tokenise;

end MyStringTokeniser;
