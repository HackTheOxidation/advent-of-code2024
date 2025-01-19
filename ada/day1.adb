with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day1 is
    use Ada.Text_IO;

    F_Type    : File_Type;
    File_Name : String;

    package Integer_Vectors is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => Integer);

    use Integer_Vectors;

    Left_Vector  : Vector;
    Right_Vector : Vector;

    Result : Natural := 0;
begin
    Put ("Enter the name of the file: ");
    Get (File_Name);

    Open (F_Type, In_File, File_Name);

    declare
        Number  : Integer;
        Counter : Natural := 0;
    begin
        while not End_Of_File (F_Type) loop
            Read (F_Type, Number);
            if Counter mod 2 = 0 then
                Left_Vector.Append (Number);
            else
                Right_Vector.Append (Number);
            end if;

            Counter := Counter + 1;
        end loop;
    end;

    Close (F_Type);

    declare
        Difference : Integer;
    begin
        for I in Left_Vector.First_Index .. Left_Vector.Last_Index loop
            Difference := abs (Left_Vector (I) - Right_Vector (I));
            Result     := Result + Difference;
        end loop;
    end;

    Put_Line (Result'Image);

exception
    when Name_Error =>
        Put_Line ("File: '" & File_Name & "' does not exist");
    when others     =>
        Put_Line
           ("Encountered unknown error while processing file: " & File_Name);
end Day1;
