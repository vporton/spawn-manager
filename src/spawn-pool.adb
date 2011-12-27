with Ada.Text_IO;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;

with GNAT.OS_Lib;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Types;

package body Spawn.Pool is

   use ZMQ;

   Ctx : Contexts.Context;
   S   : Sockets.Socket;

   Mngr_Bin  : constant String := "obj/spawn_manager";
   Addr_Base : constant String := "/tmp/spawn_manager-";

   subtype Chars is Character range 'a' .. 'z';
   package Random_Chars is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Chars);
   Generator : Random_Chars.Generator;

   function Random_String (Len : Positive) return String;
   --  Return a random string of given length.

   -------------------------------------------------------------------------

   procedure Execute (Command : String)
   is
      Query : Messages.Message;
   begin
      Query.Initialize (Data => Command);
      S.Send (Msg => Query);
      Query.Finalize;

      declare
         Resultset : Messages.Message;
         Data      : Types.Data_Type;
      begin
         Resultset.Initialize;
         S.recv (Msg => Resultset);

         Data := Types.Deserialize (Buffer => Resultset.getData);
         if Data.Success /= True then
            raise Command_Failed with "Could not execute command";
         end if;

         Resultset.Finalize;
      end;
   end Execute;

   -------------------------------------------------------------------------

   procedure Init
   is
      use type GNAT.OS_Lib.Process_Id;

      Args : GNAT.OS_Lib.Argument_List (1 .. 1);
      Pid  : GNAT.OS_Lib.Process_Id;
      Addr : constant String := "ipc://" & Addr_Base
        & Random_String (Len => 8);
   begin
      Ctx.Initialize (App_Threads => 1);
      S.Initialize (With_Context => Ctx,
                    Kind         => Sockets.REQ);

      Args (1) := new String'(Addr);
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn
        (Program_Name => Mngr_Bin,
         Args         => Args);
      GNAT.OS_Lib.Free (X => Args (1));

      if Pid = GNAT.OS_Lib.Invalid_Pid then
         raise Command_Failed with "Could not initialize spawn pool";
      end if;

      --  TODO: Handle case where no socket exists -> no exception raised

      S.Connect (Address => Addr);
   end Init;

   -------------------------------------------------------------------------

   function Random_String (Len : Positive) return String
   is
      Result : String (1 .. Len);
   begin
      for I in Result'Range loop
         Result (I) := Random_Chars.Random (Gen => Generator);
      end loop;

      return Result;
   end Random_String;

begin
   Random_Chars.Reset (Gen => Generator);
end Spawn.Pool;
