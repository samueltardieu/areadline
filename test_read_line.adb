with Ada.Text_IO;         use Ada.Text_IO;
with Readline;            use Readline;
with Readline.Completion; use Readline.Completion;

procedure Test_Read_Line is
begin
   Add_Word ("One");
   Add_Word ("Two");
   Add_Word ("Three");
   Add_Word ("Four");
   loop
      declare
         Line : constant String := Read_Line ("Type something> ");
      begin
         Put_Line ("You typed: " & Line);
      end;
   end loop;
exception
   when End_Error =>
      null;
end Test_Read_Line;
