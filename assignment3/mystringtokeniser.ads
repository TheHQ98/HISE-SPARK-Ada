with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,

     -- Count must smaller than or equal to the length of the Tokens array
     -- if count > Tokens'Length, it will leading to Index access to position beyond the end of Tokens array,
     -- also cause SPARK to fail to prove the loop is safe and program may crash at runtime
     Post => Count <= Tokens'Length and

     -- For each of Index in the first position of Token array to first Count elements,
     -- each Index must satisfy the following three conditions
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>

          -- The starting posistion of the Index in Tokens is geater than or equal to the begining of the index of S
          -- Otherwise, it will accesses to the outside of the left boundary of S, which is illegal memory
          -- and the token not be inside the bounds of the string, also leading SPARK cannot prove
          (Tokens(Index).Start >= S'First and

           -- The length of the Index token must be greater than 0
           -- if length of the Index token is 0, access the token will be outside of array and SPARK cannot prove it
           Tokens(Index).Length > 0) and then

           -- The posistion of the last character of the token must not exceed the last index of the S string
           -- otherwise will be read outside of the string S, leading overflow and SPARK cannot prove it
           Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
