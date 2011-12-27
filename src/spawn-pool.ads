package Spawn.Pool is

   procedure Init;
   --  Init client.

   procedure Execute (Command : String);
   --  Execute given command.

   Command_Failed : exception;

end Spawn.Pool;
