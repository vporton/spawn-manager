with Ada.Streams;
with Ada.Strings.Unbounded;

with Spawn.Types;

package body Spawn_Types_Tests is

   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Ahven;
   use Spawn.Types;

   -------------------------------------------------------------------------

   procedure Deserialize_Data
   is
      Stream : constant Stream_Element_Array := (1, 97, 98, 99);
      Data   : constant Data_Type            := Deserialize (Buffer => Stream);
   begin
      Assert (Condition => Data.Success = True,
              Message   => "Status field incorrect");
      Assert (Condition => Data.Command = "abc",
              Message   => "Command field incorrect");
   end Deserialize_Data;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Spawn types tests");
      T.Add_Test_Routine
        (Routine => Serialize_Data'Access,
         Name    => "Serialize data");
      T.Add_Test_Routine
        (Routine => Deserialize_Data'Access,
         Name    => "Deserialize data");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Serialize_Data
   is
      Request : constant Data_Type :=
        (Success => True,
         Command => To_Unbounded_String ("abc"));
      Ref_Cmd : constant Stream_Element_Array := (97, 98, 99);
      Stream  : constant Stream_Element_Array := Serialize (Data => Request);
   begin
      Assert (Condition => Stream'Length = 4,
              Message   => "Length incorrect");
      Assert (Condition => Stream (1) = 1,
              Message   => "Status data incorrect");
      Assert (Condition => Stream (2 .. 4) = Ref_Cmd,
              Message   => "Command data incorrect");
   end Serialize_Data;

end Spawn_Types_Tests;
