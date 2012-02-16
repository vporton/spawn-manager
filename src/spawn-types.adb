--
--  Process Spawn Manager
--
--  Copyright (C) 2012 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2012 secunet Security Networks AG
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
--  USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit,  or  you  link  this  unit  with  other  files  to  produce  an
--  executable   this  unit  does  not  by  itself  cause  the  resulting
--  executable to  be  covered by the  GNU General  Public License.  This
--  exception does  not  however  invalidate  any  other reasons why  the
--  executable file might be covered by the GNU Public License.
--

package body Spawn.Types is

   use Ada.Streams;
   use Ada.Strings.Unbounded;

   function To_Stream (Str : String) return Stream_Element_Array;
   --  Convert given string to stream array.

   -------------------------------------------------------------------------

   function Deserialize
     (Buffer : Ada.Streams.Stream_Element_Array)
      return Data_Type
   is
      Result : Data_Type;
   begin
      Result.Success := Buffer (Buffer'First)     = 1;
      Result.Do_Quit := Buffer (Buffer'First + 1) = 1;

      if Buffer'Length = 2 then
         return Result;
      end if;

      declare
         Temp : Unbounded_String;
         Char : Character;
      begin
         for C in reverse Buffer'First + 2 .. Buffer'Last loop
            Char := Character'Val (Buffer (C));

            if Char = ASCII.SOH then
               Result.Command := Temp;
               Temp := Null_Unbounded_String;
            elsif Char = ASCII.STX then
               Result.Dir := Temp;
               Temp := Null_Unbounded_String;
            else
               Temp := Char & Temp;
            end if;
         end loop;
      end;
      return Result;

   exception
      when Constraint_Error =>
         raise Deserialize_Error with "Unable to deserialize data";
   end Deserialize;

   -------------------------------------------------------------------------

   function Serialize
     (Data : Data_Type)
      return Ada.Streams.Stream_Element_Array
   is
      Cmd_Len   : constant Stream_Element_Offset := Stream_Element_Offset
        (Length (Data.Command));
      Dir_Len   : constant Stream_Element_Offset := Stream_Element_Offset
        (Length (Data.Dir));
      Total_Len : Stream_Element_Offset          := 2;
   begin
      if Cmd_Len /= 0 then
         Total_Len := Total_Len + Cmd_Len + 1;
      end if;

      if Dir_Len /= 0 then
         Total_Len := Total_Len + Dir_Len + 1;
      end if;

      declare
         Result : Stream_Element_Array (1 .. Total_Len);
         Idx    : Stream_Element_Offset;
      begin
         Result (1) := Boolean'Pos (Data.Success);
         Result (2) := Boolean'Pos (Data.Do_Quit);

         if Total_Len = 2 then
            return Result;
         end if;

         --  SOH/1 : Command
         --  STX/2 : Directory

         Idx := 3;

         if Cmd_Len /= 0 then
            Result (Idx .. Idx + Cmd_Len) := To_Stream
              (ASCII.SOH & To_String (Data.Command));
            Idx := Idx + Cmd_Len + 1;
         end if;

         if Dir_Len /= 0 then
            Result (Idx .. Idx + Dir_Len) := To_Stream
              (ASCII.STX & To_String (Data.Dir));
         end if;

         return Result;
      end;
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

end Spawn.Types;
