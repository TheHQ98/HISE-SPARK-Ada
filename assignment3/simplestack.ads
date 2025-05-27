-- Note: this file is from SWEN90010 Lecture 14 and 15 - Live Programming and Proving in SPARK (Simple Stack)

generic
   Max_Size : Positive;
   type Item is private;
   Default_Item : Item;

package SimpleStack with SPARK_Mode is
   type SimpleStack is private;

   procedure Init(S : out SimpleStack)
     with Post => Size(S) = 0;

   procedure Push(S : in out SimpleStack; I : in Item)
     with Pre => (Size(S) /= Max_Size),
     Post => Size(S) = Size(S'Old) + 1 and
     Storage(S, Size(S)) = I and
     (for all J in 1..Size(S'Old) => Storage(S,J) = Storage(S'Old, J));

   procedure Pop(S : in out SimpleStack; I : out Item)
     with Pre => (Size(S) /= 0),
     Post => Size(S) = Size(S'Old) - 1 and
     I = Storage(S'Old, Size(S'Old)) and
     (for all J in 1..Size(S) => Storage(S,J) = Storage(S'Old,J));

   function Size (S : in SimpleStack) return Integer;

   function Storage (S : in SimpleStack; Pos : in Integer) return Item
     with Pre => (Pos in 1..Max_Size),
     Ghost;

private
   type StorageArray is array(1..Max_Size) of Item;

   type SimpleStack is record
      size : Integer range 0..Max_Size;
      storage : StorageArray;
   end record;

   function Size (S : in SimpleStack) return Integer is
     (S.size);

   function Storage (S : in SimpleStack; Pos : in Integer) return Item is
     (S.storage(Pos));

end SimpleStack;
