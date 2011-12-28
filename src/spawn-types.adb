package body Spawn.Types is

   use Ada.Streams;
   use Ada.Strings.Unbounded;

   function To_Stream (Str : String) return Stream_Element_Array;
   --  Convert given string to stream array.

   function To_String (Stream : Stream_Element_Array) return String;
   --  Convert given stream array to string.

   -------------------------------------------------------------------------

   function Deserialize
     (Buffer : Ada.Streams.Stream_Element_Array)
      return Data_Type
   is
      use type Ada.Streams.Stream_Element;

      Result : Data_Type;
   begin
      Result.Success := Buffer (Buffer'First)     = 1;
      Result.Do_Quit := Buffer (Buffer'First + 1) = 1;
      Result.Command := To_Unbounded_String
        (To_String (Buffer (Buffer'First + 2 .. Buffer'Last)));
      return Result;
   end Deserialize;

   -------------------------------------------------------------------------

   function Serialize
     (Data : Data_Type)
      return Ada.Streams.Stream_Element_Array
   is
      Cmd_Len : constant Stream_Element_Offset := Stream_Element_Offset
        (Length (Data.Command));
      Result  : Stream_Element_Array (1 .. 2 + Cmd_Len);
   begin
      Result (1) := Boolean'Pos (Data.Success);
      Result (2) := Boolean'Pos (Data.Do_Quit);

      if Cmd_Len /= 0 then
         Result (3 .. Result'Last) := To_Stream (To_String (Data.Command));
      end if;

      return Result;
   end Serialize;

   -------------------------------------------------------------------------

   function To_Stream (Str : String) return Ada.Streams.Stream_Element_Array
   is
      Result : Stream_Element_Array (1 .. Str'Length);
   begin
      for R in Result'Range loop
         Result (R) := Character'Pos (Str (Integer (R)));
      end loop;

      return Result;
   end To_Stream;

   -------------------------------------------------------------------------

   function To_String (Stream : Stream_Element_Array) return String
   is
      Result : String (1 .. Stream'Length);
   begin
      for R in Result'Range loop
         Result (R) := Character'Val
           (Stream (Stream'First - 1 + Stream_Element_Offset (R)));
      end loop;

      return Result;
   end To_String;

end Spawn.Types;
