with Ada.Text_IO; use Ada.Text_IO;
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
         Put_Line ("You typed: " & Line);
      end;
   end loop;
end Test_Read_Line;
