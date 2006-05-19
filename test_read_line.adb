with Ada.Text_IO; use Ada.Text_IO;
with Read_Line;

procedure Test_Read_Line is
begin
   loop
      declare
         Line : constant String := Read_Line ("Type something> ");
      begin
         Put_Line ("You typed: " & Line);
      end;
   end loop;
end Test_Read_Line;
