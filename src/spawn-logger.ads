with Ada.Text_IO;

package Spawn.Logger is

   procedure Log (Message : String) renames Ada.Text_IO.Put_Line;
   --  Log procedure.

end Spawn.Logger;
