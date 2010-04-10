package Readline is

   pragma Preelaborate;

   function Read_Line (Prompt : String := "") return String;
   --  May raise Ada.IO_Exceptions.End_Error if end-of-file is encountered

end Readline;
