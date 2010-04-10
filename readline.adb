with Ada.IO_Exceptions;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

package body Readline is

   pragma Linker_Options ("-lreadline");
   pragma Linker_Options ("-lncurses");

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (Prompt : String := "") return String is

      function Readline (Prompt : char_array) return chars_ptr;
      pragma Import (C, Readline, "readline");

      procedure Add_History (Line : chars_ptr);
      pragma Import (C, Add_History, "add_history");

      C_Line   : chars_ptr;

   begin
      C_Line := Readline (To_C (Prompt));

      if C_Line = Null_Ptr then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      declare
         Result : constant String := Value (C_Line);
      begin
         Add_History (C_Line);
         Free (C_Line);
         return Result;
      end;
   end Read_Line;

end Readline;
