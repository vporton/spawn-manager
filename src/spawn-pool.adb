with Ada.Text_IO;
with Ada.Directories;

with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

with Types;

package body Spawn.Pool is

   use ZMQ;

   Ctx  : Contexts.Context;
   S    : Sockets.Socket;
   Addr : constant String := "ipc:///tmp/spawn_manager";

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
   begin
      Ctx.Initialize (App_Threads => 1);
      S.Initialize (With_Context => Ctx,
                    Kind         => Sockets.REQ);

      --  TODO: Handle case where no socket exists -> no exception raised

      S.Connect (Address => "ipc:///tmp/spawn_manager");
   end Init;

end Spawn.Pool;
