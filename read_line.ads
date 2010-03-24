package Read_Line is

   procedure Add_Word (Word : String);

   procedure Clear_All_Words;

   procedure Variable_Bind (Name : String; Value : String);

   function Variable_Value (Name : String) return String;

   function Read_Line (Prompt : String := "") return String;
   --  May raise Ada.IO_Exceptions.End_Error if end-of-file is encountered

end Read_Line;
