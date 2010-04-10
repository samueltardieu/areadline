package Readline.Variables is

   pragma Preelaborate;

   procedure Variable_Bind (Name : String; Value : String);
   function Variable_Value (Name : String) return String;

end Readline.Variables;
