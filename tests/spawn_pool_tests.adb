with Spawn.Pool;

package body Spawn_Pool_Tests is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Execute_Bin_False
   is
   begin
      Spawn.Pool.Execute (Command => "/bin/false");
      Fail (Message => "Exception expected");

   exception
      when Spawn.Pool.Command_Failed => null;
   end Execute_Bin_False;

   -------------------------------------------------------------------------

   procedure Execute_Bin_True
   is
   begin
      Spawn.Pool.Execute (Command => "/bin/true");
   end Execute_Bin_True;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Spawn pool tests");
      T.Add_Test_Routine
        (Routine => Execute_Bin_True'Access,
         Name    => "Execute /bin/true");
      T.Add_Test_Routine
        (Routine => Execute_Bin_False'Access,
         Name    => "Execute /bin/false");
      T.Add_Test_Routine
        (Routine => Parallel_Execution'Access,
         Name    => "Parallel execution");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Parallel_Execution
   is
      task type Executor is
         entry Call;
         entry Done;
      end Executor;

      task body Executor is
      begin
         accept Call;
         Spawn.Pool.Execute (Command => "/bin/true");
         accept Done;
      end Executor;

      Task_Array : array (1 .. 4) of Executor;
   begin
      for T in Task_Array'Range loop
         Task_Array (T).Call;
      end loop;

      for T in Task_Array'Range loop
         Task_Array (T).Done;
      end loop;
   end Parallel_Execution;

end Spawn_Pool_Tests;
