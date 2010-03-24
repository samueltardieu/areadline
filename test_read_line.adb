with Ada.IO_Exceptions;
with Ada.Text_IO;
with Readline;

procedure Test_Read_Line is
begin
   Readline.Add_Word ("One");
   Readline.Add_Word ("Two");
   Readline.Add_Word ("Three");
   Readline.Add_Word ("Four");
   loop
      declare
         Line : constant String := Readline.Read_Line ("Type something> ");
      begin
         Ada.Text_IO.Put_Line ("You typed: " & Line);
      end;
   end loop;
exception
   when Ada.IO_Exceptions.End_Error =>
      null;
end Test_Read_Line;
