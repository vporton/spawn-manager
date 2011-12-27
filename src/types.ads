with Ada.Streams;

package Types is

   type Data_Type is record
      Success : Boolean := False;
   end record;

   function Serialize
     (Data : Data_Type)
      return Ada.Streams.Stream_Element_Array;
   --  Serialize given data type.

   function Deserialize
     (Buffer : Ada.Streams.Stream_Element_Array)
      return Data_Type;
   --  Deserialize given data type.

end Types;
