package body Spawn.Types is

   -------------------------------------------------------------------------

   function Deserialize
     (Buffer : Ada.Streams.Stream_Element_Array)
      return Data_Type
   is
      use type Ada.Streams.Stream_Element;

      Result : Data_Type;
   begin
      Result.Success := Buffer (1) = 1;
      return Result;
   end Deserialize;

   -------------------------------------------------------------------------

   function Serialize
     (Data : Data_Type)
      return Ada.Streams.Stream_Element_Array
   is
      Result : Ada.Streams.Stream_Element_Array (1 .. 1);
   begin
      Result (1) := Boolean'Pos (Data.Success);
      return Result;
   end Serialize;

end Spawn.Types;
