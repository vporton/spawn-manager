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
      Stream0 : constant Stream_Element_Array := (1, 0);
      Stream1 : constant Stream_Element_Array
        := (1, 1, 1, 115, 116, 117, 2, 118, 119, 120);
      Stream2 : constant Stream_Element_Array
        := (0, 1, 2, 118, 119, 120);
      Stream3 : constant Stream_Element_Array
        := (0, 0, 1, 118, 119, 120, 97);
      Data0   : constant Data_Type := Deserialize (Buffer => Stream0);
      Data1   : constant Data_Type := Deserialize (Buffer => Stream1);
      Data2   : constant Data_Type := Deserialize (Buffer => Stream2);
      Data3   : constant Data_Type := Deserialize (Buffer => Stream3);
   begin
      Assert (Condition => Data0.Success = True,
              Message   => "Status field incorrect");
      Assert (Condition => Data0.Do_Quit = False,
              Message   => "Quit field incorrect");
      Assert (Condition => Data0.Command = Null_Unbounded_String,
              Message   => "Command not null");
      Assert (Condition => Data0.Dir = Null_Unbounded_String,
              Message   => "Directory not null");

      Assert (Condition => Data1.Success = True,
              Message   => "Status field incorrect");
      Assert (Condition => Data1.Do_Quit = True,
              Message   => "Quit field incorrect");
      Assert (Condition => Data1.Command = "stu",
              Message   => "Command field incorrect: "
              & To_String (Data1.Command));
      Assert (Condition => Data1.Dir = "vwx",
              Message   => "Directory field incorrect: "
              & To_String (Data1.Dir));

      Assert (Condition => Data2.Success = False,
              Message   => "Status field incorrect");
      Assert (Condition => Data2.Do_Quit = True,
              Message   => "Quit field incorrect");
      Assert (Condition => Data2.Command = Null_Unbounded_String,
              Message   => "Command not null");
      Assert (Condition => Data2.Dir = "vwx",
              Message   => "Directory field incorrect: "
              & To_String (Data2.Dir));

      Assert (Condition => Data3.Success = False,
              Message   => "Status field incorrect");
      Assert (Condition => Data3.Do_Quit = False,
              Message   => "Quit field incorrect");
      Assert (Condition => Data3.Command = "vwxa",
              Message   => "Command field incorrect: "
              & To_String (Data3.Command));
      Assert (Condition => Data3.Dir = Null_Unbounded_String,
              Message   => "Directory field not null");
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
      Request0 : constant Data_Type :=
        (Success => False,
         Do_Quit => True,
         others  => <>);
      Request1 : constant Data_Type :=
        (Success => True,
         Do_Quit => True,
         Command => To_Unbounded_String ("abc"),
         Dir     => To_Unbounded_String ("cba"));
      Request2 : constant Data_Type :=
        (Success => True,
         Do_Quit => False,
         Dir     => To_Unbounded_String ("cba"),
         others  => <>);
      Request3 : constant Data_Type :=
        (Success => False,
         Do_Quit => False,
         Command => To_Unbounded_String ("abc"),
         others  => <>);
      Ref_Cmd : constant Stream_Element_Array := (1, 97, 98, 99);
      Ref_Dir : constant Stream_Element_Array := (2, 99, 98, 97);

      S0 : constant Stream_Element_Array := Serialize (Data => Request0);
      S1 : constant Stream_Element_Array := Serialize (Data => Request1);
      S2 : constant Stream_Element_Array := Serialize (Data => Request2);
      S3 : constant Stream_Element_Array := Serialize (Data => Request3);
   begin
      Assert (Condition => S0'Length = 2,
              Message   => "Length incorrect" & Natural'Image (S0'Length));
      Assert (Condition => S0 (1) = 0,
              Message   => "Status data incorrect");
      Assert (Condition => S0 (2) = 1,
              Message   => "Quit flag incorrect");

      Assert (Condition => S1'Length = 10,
              Message   => "Length incorrect" & Natural'Image (S1'Length));
      Assert (Condition => S1 (1) = 1,
              Message   => "Status data incorrect");
      Assert (Condition => S1 (2) = 1,
              Message   => "Quit flag incorrect");

      Assert (Condition => S1 (3 .. 6) = Ref_Cmd,
              Message   => "Command data incorrect");
      Assert (Condition => S1 (7 .. 10) = Ref_Dir,
              Message   => "Dir data incorrect");

      Assert (Condition => S2'Length = 6,
              Message   => "Length incorrect" & Natural'Image (S2'Length));
      Assert (Condition => S2 (1) = 1,
              Message   => "Status data incorrect");
      Assert (Condition => S2 (2) = 0,
              Message   => "Quit flag incorrect");
      Assert (Condition => S2 (3 .. 6) = Ref_Dir,
              Message   => "Dir data incorrect");

      Assert (Condition => S3'Length = 6,
              Message   => "Length incorrect" & Natural'Image (S3'Length));
      Assert (Condition => S3 (1) = 0,
              Message   => "Status data incorrect");
      Assert (Condition => S3 (2) = 0,
              Message   => "Quit flag incorrect");
      Assert (Condition => S3 (3 .. 6) = Ref_Cmd,
              Message   => "Command data incorrect");
   end Serialize_Data;

end Spawn_Types_Tests;
