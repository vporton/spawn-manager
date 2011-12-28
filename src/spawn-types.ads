with Ada.Streams;
with Ada.Strings.Unbounded;

package Spawn.Types is

   type Data_Type is record
      Success : Boolean := False;
      Do_Quit : Boolean := False;
      Command : Ada.Strings.Unbounded.Unbounded_String;
      Dir     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Shutdown_Token : constant Data_Type;

   function Serialize
     (Data : Data_Type)
      return Ada.Streams.Stream_Element_Array;
   --  Serialize given data type.

   function Deserialize
     (Buffer : Ada.Streams.Stream_Element_Array)
      return Data_Type;
   --  Deserialize given data type.

private

   Shutdown_Token : constant Data_Type := (Do_Quit => True,
                                           others  => <>);

end Spawn.Types;
