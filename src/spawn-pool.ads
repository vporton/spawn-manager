package Spawn.Pool is

   procedure Init;
   --  Init spawn pool.

   procedure Execute (Command : String);
   --  Execute given command.

   Command_Failed : exception;

end Spawn.Pool;
