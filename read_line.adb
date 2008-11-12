with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Interfaces.C.Strings; use Interfaces.C.Strings;

function Read_Line (Prompt : String := "") return String is

   pragma Linker_Options ("-lreadline");
   pragma Linker_Options ("-lncurses");

   function Readline (Prompt : chars_ptr) return chars_ptr;
   pragma Import (C, Readline, "readline");

   procedure Add_History (Line : chars_ptr);
   pragma Import (C, Add_History, "add_history");

   C_Prompt : chars_ptr;
   C_Line   : chars_ptr;

begin
   if Prompt /= "" then
      C_Prompt := New_String (Prompt);
   else
      C_Prompt := Null_Ptr;
   end if;
   C_Line := Readline (C_Prompt);
   if C_Prompt /= Null_Ptr then
      Free (C_Prompt);
   end if;

   if C_Line = Null_Ptr then
      raise End_Error;
   end if;

   declare
      Result : constant String := Value (C_Line);
   begin
      Add_History (C_Line);
      Free (C_Line);
      return Result;
   end;
end Read_Line;
