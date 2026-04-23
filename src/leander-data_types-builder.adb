with Leander.Core.Tycons;
with Leander.Names;

package body Leander.Data_Types.Builder is

   -------------
   -- Add_Con --
   -------------

   procedure Add_Con
     (This   : in out Data_Type_Builder'Class;
      Name   : Leander.Core.Conid;
      Scheme : Leander.Core.Schemes.Reference)
   is
   begin
      This.Cons.Append
        (Con_Scheme'
           (Id => Name, Scheme => Nullable_Scheme_Reference (Scheme)));
   end Add_Con;

   -----------
   -- Build --
   -----------

   procedure Build
     (This   : in out Data_Type_Builder'Class)
   is

      function Pat_Id
        (Index : Positive)
         return String
      is ("$" & Character'Val (Character'Pos ('a')
          + Index + This.Cons.Last_Index - 1));

      function Var_Id
        (Index : Positive)
         return String
      is ("$" & Character'Val (Character'Pos ('a') + Index - 1));

      Var_Ids : constant Leander.Names.Name_Array :=
                  [for I in 1 .. This.Cons.Last_Index =>
                               Leander.Names.To_Leander_Name
                                 (Var_Id (I))];

      function Con_Arg_Count
        (Scheme : Leander.Core.Schemes.Reference)
         return Natural;

      function Create_Con_Record
        (Index : Positive)
         return Con_Record;

      -------------------
      -- Con_Arg_Count --
      -------------------

      function Con_Arg_Count
        (Scheme : Leander.Core.Schemes.Reference)
         return Natural
      is
         use type Core.Types.Reference;
         T : Core.Types.Reference := Scheme.Inner_Type;
         Count : Natural := 0;
      begin
         while T.Is_Application
           and then T.Left.Is_Application
           and then T.Left.Left = Core.Types.T_Arrow
         loop
            Count := Count + 1;
            T := T.Right;
         end loop;
         return Count;
      end Con_Arg_Count;

      -----------------------
      -- Create_Con_Record --
      -----------------------

      function Create_Con_Record
        (Index : Positive)
         return Con_Record
      is
         CR        : Con_Scheme renames This.Cons (Index);
         Id        : constant Core.Conid := CR.Id;
         Scheme    : constant Leander.Core.Schemes.Reference :=
                       Leander.Core.Schemes.Reference (CR.Scheme);
         Arg_Count : constant Natural := Con_Arg_Count (Scheme);
         Pat_Ids   : constant Leander.Names.Name_Array :=
                       [for I in 1 .. Arg_Count =>
                                      Leander.Names.To_Leander_Name
                                        (Pat_Id (I))];
         E         : Calculus.Tree :=
                       Calculus.Symbol (Var_Ids (Index));
      begin
         for Id of Pat_Ids loop
            E := Calculus.Apply (E, Calculus.Symbol (Id));
         end loop;
         for Id of reverse Var_Ids loop
            E := Calculus.Lambda (Id, E);
         end loop;
         for Id of reverse Pat_Ids loop
            E := Calculus.Lambda (Id, E);
         end loop;
         return Con_Record'
           (Con_Name => Id,
            Con_Type => Scheme,
            Con_Defn => E);
      end Create_Con_Record;

   begin
      This.DT := new Instance'
        (Con_Count => This.Cons.Last_Index,
         Id        => This.Tycon.Constructor.Id,
         Tycon     => Core.Types.Reference (This.Tycon),
         Applied   => Core.Types.Reference (This.Applied),
         Kind      => This.Kind,
         Cons      => [for I in 1 .. This.Cons.Last_Index =>
                           Create_Con_Record (I)]);
   end Build;

   ---------------
   -- Data_Type --
   ---------------

   function Data_Type
     (This : Data_Type_Builder'Class)
      return Reference
   is
   begin
      return Reference (This.DT);
   end Data_Type;

   -----------
   -- Start --
   -----------

   procedure Start
     (This  : in out Data_Type_Builder'Class;
      Ty    : Leander.Core.Types.Reference)
   is
      T : Leander.Core.Types.Reference := Ty;
      K : Leander.Core.Kinds.Kind := Leander.Core.Kinds.Star;

      function Rebuild
        (T         : Leander.Core.Types.Reference;
         Head_Kind : Leander.Core.Kinds.Kind)
         return Leander.Core.Types.Reference;

      -------------
      -- Rebuild --
      -------------

      function Rebuild
        (T         : Leander.Core.Types.Reference;
         Head_Kind : Leander.Core.Kinds.Kind)
         return Leander.Core.Types.Reference
      is
      begin
         if T.Is_Application then
            return Leander.Core.Types.Application
              (Rebuild (T.Left, Head_Kind), T.Right);
         else
            return Leander.Core.Types.TCon
              (Leander.Core.Tycons.Tycon (T.Constructor.Id, Head_Kind));
         end if;
      end Rebuild;

   begin
      while T.Is_Application loop
         T := T.Left;
         K := Leander.Core.Kinds.Kind_Function (K, Leander.Core.Kinds.Star);
      end loop;

      --  The parser gives every type constructor in a data declaration the
      --  default kind *. Rebuild the head tycon (and the applied spine)
      --  with the kind derived from the number of type arguments, so that
      --  Applied_Type.Get_Kind succeeds on higher-kinded types like
      --  Maybe a.
      declare
         Head : constant Leander.Core.Types.Reference :=
                  Leander.Core.Types.TCon
                    (Leander.Core.Tycons.Tycon (T.Constructor.Id, K));
      begin
         This.Id := T.Constructor.Id;
         This.Kind := K;
         This.Tycon := Nullable_Type_Reference (Head);
         This.Applied := Nullable_Type_Reference (Rebuild (Ty, K));
      end;
      This.Cons.Clear;
      This.DT := null;
   end Start;

end Leander.Data_Types.Builder;
