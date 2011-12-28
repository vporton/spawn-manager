with Ada.Text_IO;
with Ada.Exceptions;

with Spawn.Pool;

package body Spawn_Pool_Tests is

   use Ahven;

   task type Executor is
      entry Call;
      entry Done (Success : out Boolean);
   end Executor;
   --  Task used in parallel tests.

   task body Executor is
      Got_Exception : Boolean := False;
   begin
      accept Call;

      begin
         Spawn.Pool.Execute (Command => "/bin/true");

      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Exceptions.Exception_Information (X => E));
            Got_Exception := True;
      end;

      accept Done (Success : out Boolean) do
         Success := not Got_Exception;
      end Done;
   end Executor;

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
      T.Add_Test_Routine
        (Routine => Pool_Depleted'Access,
         Name    => "Pool depleted");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Parallel_Execution
   is
      Task_Array : array (1 .. 4) of Executor;
   begin
      for T in Task_Array'Range loop
         Task_Array (T).Call;
      end loop;

      for T in Task_Array'Range loop
         declare
            Result : Boolean;
         begin
            Task_Array (T).Done (Success => Result);
            Assert (Condition => Result,
                    Message   => "Parallel execution failed");
         end;
      end loop;
   end Parallel_Execution;

   -------------------------------------------------------------------------

   procedure Pool_Depleted
   is
      Task_Array : array (1 .. 8) of Executor;
      Result     : Boolean := True;
   begin
      for T in Task_Array'Range loop
         Task_Array (T).Call;
      end loop;

      for T in Task_Array'Range loop
         declare
            Temp : Boolean;
         begin
            Task_Array (T).Done (Success => Temp);

            --  One should fail (pool depleted).

            Result := Result and Temp;
         end;
      end loop;

      Assert (Condition => not Result,
              Message   => "No call failed");
   end Pool_Depleted;

end Spawn_Pool_Tests;
