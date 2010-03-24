with Readline;

--------------
-- Read_Line --
---------------

function Read_Line (Prompt : String := "") return String is
begin
   return Readline.Read_Line (Prompt);
end Read_Line;
