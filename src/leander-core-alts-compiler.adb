with Leander.Core.Types;
with Leander.Data_Types;

package body Leander.Core.Alts.Compiler is

   procedure Add_Alts
     (This     : in out Builder'Class;
      Pat_Type : Leander.Core.Types.Reference;
      Alts     : Reference_Array)
     with Pre => Alts'Length > 0;

   ---------
   -- Add --
   ---------

   procedure Add
     (This : in out Builder'Class;
      Alts : Reference_Array)
   is
      Alt      : constant Leander.Core.Alts.Reference := Alts (Alts'First);
   begin
      if not Alt.Has_Pattern then
         This.Con_Dfl :=
           (Con_Pat_Expr'
              (Pat  => null,
               Expr => Nullable_Expression (Alt.Expr)));
      else
         This.Add_Alts
           (This.Context.Get_Type (Alts (Alts'First).Pat)
            .Apply (This.Context.Current_Substitution),
            Alts => Alts);
      end if;
   end Add;

   --------------
   -- Add_Alts --
   --------------

   procedure Add_Alts
     (This     : in out Builder'Class;
      Pat_Type : Leander.Core.Types.Reference;
      Alts     : Reference_Array)
   is
      First_Alt : constant Reference := Alts (Alts'First);
   begin
      if Pat_Type.Is_Variable then
         This.Con_Dfl := (Nullable_Pattern (First_Alt.Pat),
                          Nullable_Expression (First_Alt.Expr));
      elsif Pat_Type.Is_Constructor
        and then Pat_Type.Head = To_Conid ("Int")
      then
         This.Compare_Mode := True;
         for Alt of Alts loop
            declare
               Pat  : constant Nullable_Pattern_Reference := Alt.Pat;
            begin
               if Pat.Is_Variable then
                  This.Con_Dfl := (Nullable_Pattern (Pat),
                                   Nullable_Expression (Alt.Expr));
                  exit;
               end if;

               This.Con_Pats.Append
                 (Con_Pat_Expr'
                    (Nullable_Pattern (Pat),
                     Nullable_Expression (Alt.Expr)));
            end;
         end loop;

      else
         declare
            Con : constant Leander.Core.Conid := Pat_Type.Head;
            pragma Assert
              (This.Env.Exists
                 (Leander.Names.Leander_Name (Con),
                  Leander.Environment.Type_Constructor),
               "undefined constructor: "
               & Leander.Core.To_String (Con)
               & " in pat type "
               & Pat_Type.Show);
            DT  : constant Leander.Data_Types.Reference :=
                    This.Env.Data_Type (Con);
         begin
            for I in 1 .. DT.Constructor_Count loop
               This.Con_Pats.Append
                 (Con_Pat_Expr'(null, null));
            end loop;

            for Alt of Alts loop
               declare
                  Pat  : constant Nullable_Pattern_Reference := Alt.Pat;
               begin
                  if Pat.Is_Variable then
                     This.Con_Dfl := (Nullable_Pattern (Pat),
                                      Nullable_Expression (Alt.Expr));
                     exit;
                  end if;

                  declare
                     Pat_Con  : constant Leander.Core.Conid :=
                                  Pat.Constructor;
                     Index    : constant Natural :=
                                  DT.Constructor_Index (Pat_Con);
                  begin
                     if Index = 0 then
                        raise Constraint_Error with
                          "no such constructor " & Core.To_String (Pat_Con)
                          & " for type " & Core.To_String (DT.Id);
                     end if;
                     pragma Assert (Index <= This.Con_Pats.Last_Index);
                     if This.Con_Pats (Index).Expr = null then
                        This.Con_Pats (Index) :=
                          (Nullable_Pattern (Pat),
                           Nullable_Expression (Alt.Expr));
                     end if;
                  end;
               end;
            end loop;
         end;
      end if;
   end Add_Alts;
   --------------
   -- Add_Name --
   --------------

   procedure Add_Name
     (This : in out Builder'Class;
      Name : Varid)
   is
   begin
      This.Names.Append (Name);
   end Add_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This      : in out Builder'Class;
      Context   : Leander.Core.Inference.Inference_Context'Class;
      Env       : Leander.Environment.Reference)
   is
   begin
      This.Context := Leander.Core.Inference.Inference_Context (Context);
      This.Env := Env;
   end Initialize;

   -----------------
   -- To_Calculus --
   -----------------

   function To_Calculus
     (This : Builder'Class)
      return Leander.Calculus.Tree
   is
   begin
      if This.Con_Pats.Is_Empty
        and then This.Con_Dfl.Pat = null
      then
         return This.Con_Dfl.Expr.To_Calculus (This.Context, This.Env);
      end if;

      declare
         use Leander.Calculus;
         V          : constant Leander.Names.Leander_Name :=
                        Leander.Names.New_Name;
         R          : Tree := Symbol (V);
         Err        : Tree;
         Have_Error : Boolean := False;
      begin
         if This.Compare_Mode then
            if This.Con_Dfl.Expr = null then
               R := Symbol ("#error");
            else
               R := This.Con_Dfl.Expr.To_Calculus
                 (This.Context, This.Env);
               if not This.Con_Dfl.Pat.Is_Wildcard then
                  R :=
                    Apply
                      (Lambda
                         (Leander.Names.Leander_Name (This.Con_Dfl.Pat.Variable),
                          R),
                       Symbol (V));
               end if;
            end if;
            for Cp of reverse This.Con_Pats loop
               R :=
                 Apply
                   (Apply
                      (Apply
                         (Apply
                              (Symbol ("#eq"),
                               Symbol (V)),
                          Cp.Pat.Literal.To_Calculus),
                       Cp.Expr.To_Calculus (This.Context, This.Env)),
                    R);
            end loop;
         else
            R := Symbol (V);
            for I in 1 .. This.Con_Pats.Last_Index loop
               declare
                  Cp : Con_Pat_Expr renames This.Con_Pats (I);
                  E  : Tree;
               begin
                  if Cp.Expr = null then
                     if This.Con_Dfl.Expr = null then
                        if not Have_Error then
                           Err := Symbol ("#error");
                           Have_Error := True;
                        end if;
                        E := Err;
                     else
                        E := This.Con_Dfl.Expr.To_Calculus
                          (This.Context, This.Env);
                        E := Lambda
                          (Leander.Names.Leander_Name (This.Con_Dfl.Pat.Variable),
                           E);
                     end if;
                  else
                     E := Cp.Expr.To_Calculus (This.Context, This.Env);
                     for Id of reverse Cp.Pat.Con_Arguments loop
                        E := Lambda (Leander.Names.Leander_Name (Id), E);
                     end loop;
                  end if;
                  R := Apply (R, E);
               end;
            end loop;

         end if;

         R := Lambda (V, R);

         for Name of This.Names loop
            R :=
              Apply
                (Symbol ("Y"),
                 Lambda
                   (Leander.Names.Leander_Name (Name),
                    R));
         end loop;

         return R;
      end;

   end To_Calculus;

end Leander.Core.Alts.Compiler;
