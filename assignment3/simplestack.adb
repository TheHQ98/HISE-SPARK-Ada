-- Note: this file is from SWEN90010 Lecture 14 and 15 - Live Programming and Proving in SPARK (Simple Stack)

package body SimpleStack  with SPARK_Mode is

   procedure Init(S : out SimpleStack) is
   begin
      S.size := 0;
      S.storage := (others => Default_Item);
   end Init;

   procedure Push(S : in out SimpleStack; I : in Item) is
   begin
      S.size := S.size + 1;
      S.storage(S.size) := I;
   end Push;

   procedure Pop(S : in out SimpleStack; I : out Item) is
   begin
      I := S.storage(S.size);
      S.size := S.size - 1;
   end Pop;

end SimpleStack;
