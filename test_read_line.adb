with Ada.IO_Exceptions;
with Ada.Text_IO;
with Read_Line;

procedure Test_Read_Line is
begin
   loop
      declare
         Line : constant String := Read_Line ("Type something> ");
      begin
         Ada.Text_IO.Put_Line ("You typed: " & Line);
      end;
   end loop;
exception
   when Ada.IO_Exceptions.End_Error =>
      null;
end Test_Read_Line;
