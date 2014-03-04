program Polly_06r;

{C:\lazarus\fpc\2.6.2\bin\i386-win32\fpc -Sogh -O2 -viwn -g -Cr-t- Polly_06r.lpr
 C:\lazarus\fpc\2.6.2\bin\i386-win32\fpc -Sogh -O2 -viwn -g Polly_06r.lpr
 - OpbouwRuitAfstBlok adapted for unreachable fields (hidden by diamond connections of opponnent) => 88 }

Type V_110 = array[0..110] of Integer;
Type Z_110 = array[0..110, 1..2] of Integer;
Type BD255 = array[0..255] of Integer;
//8 and 9: summed distances without using Abs()
Type AFST255 = array[1..2, 1..9, 0..255] of Integer;
Type GHD255 = array[1..5, 0..255] of Integer;
Type RS5_7 = array[0..5, 0..7] of Integer;
Type RR5_6 = array[1..5, 0..6] of Integer;

Const giRuiten : array[1..12, 0..7] of Integer = (
(-13, 16, 29, 13,-16,-29,-13, 16),(-14, 17, 31, 14,-17,-31,-14, 17), //0:T1, 1:T2
(-14, 17, 31, 29, 13,-16,-14, 17),(-14, 16, 29, 13,-17,-31,-14, 16), //2:T3, 3:T4
(-14, 16, 29, 13,-16,-14, 16, 29),(  2, 16, 29, 13,-16,-29,  2, 16), //4:T5, 5:T6
(-13, 16, 29, 13,-16,-30,-13, 16),(-14, 17, 31, 14, -2,-31,-14, 17), //6:T7, 7:T8
(-14, 17, 30, 14,-17,-31,-14, 17),(-14, 17, 30, 29, 13,-16,-14, 17), //8:T9, 9:T10 (for field 31)
(-14, 16, 29, 13, -2,-31,-14, 16),    //%:T11 (for field 44)
(-14, 17, 30, 14, -2,-31,-14, 17) );  //#:T12 (for field 32)

Const DATA_INVL_C : array[1..17] of String = ('...............',
  '...............',
  '........6......',
  '.......656.....',
  '......65456....',
  '.....6543456...',
  '....654323456..',
  '...65432123456.',
  '..6543210123456',
  '..654321123456.',
  '..65432223456..',
  '..6543333456...',
  '..654444456....',
  '..65555556.....',
  '..6666666......',
  '...............',
  '...............');
Const DATA_SWAP : array[1..17] of String = ('...............',
  '...............',
  '........6......',
  '.......656.....',
  '......65156....',
  '.....6511156...',
  '....651111156..',
  '...65111111156.',
  '..6511111111156',
  '..651111111156.',
  '..65111111156..',
  '..6511111156...',
  '..651111156....',
  '..65555556.....',
  '..6666666......',
  '...............',
  '...............');


var gi, gj, gk : Integer;

Const CRLF = #13#10;
Const WIT = 1; ZWART = 2;
Const INITTRANS = 'EDCBA.0123456789%#abcdefghvwxyz';  {Helparray used with init.boardvariables}

var giVeldnr : V_110; // [0]=total nr of fields(max 106); relation fieldnr x board-index.
    giZetten : Z_110; //(..,1)=move; (..,2)=index in giBord().
    giMx, giMbu : array[1..2, 0..106, 1..3] of Integer; {color(1/2), best move(0=number, max 106), 1/2/3=som4a,som4,move}
    giBord, giRand, giBType, giRType : BD255;
    giInvloedC, giSwap : BD255;  //Preference for central fields.
    //8 and 9: summed distances without using Abs()
    giAfstand,giRuitAfst : AFST255; //[1]player,edge 1-5, sum3edges, sum4edges, field.
    giRandRuit : RR5_6;   {extra fields for init. diamond-distances.}
                          {giRandRuit[i,0]=number; [i,1..6] fields from where to start testing for diamond.}
    giRandRuitRT : array[1..5,1..2] of Integer;     {(i,1)=>giRuiten;(i,2)=>RType for rand i}
    giGhd : GHD255; //edge 1-5,field.
    giRandstenen : RS5_7;              //(5, 7) As Integer
    spMe, spOpp, giZetnr : Integer;

// 5 types of neighbour fields --
var giBuren : array[1..5, 0..7] of Integer;  //7th item for possible check on diamond.
    //giRuiten : array[1..12, 0..7] of Integer; 12 types of diamond-neighbour fields, now declared as Const.
{**Not yet used:
    giKetens : array[1..2, 0..55, 0..55] of Integer;  //color, chain, stones
    giKetenPtr : array[1..2, 0..55] of Integer;
  }
  
Procedure DebugPrint(s : string);
  begin
  writeln(StdErr, s);
  //flush(output);
end;

Function IntToStr (pI : Integer) : String;
  Var S : String;
  begin
     Str (pI,S);
     IntToStr := S;
end;

Function StrToInt(pS : String) : Integer;
{ If invalid number then return 0. }
  Var I, Code : Integer;
begin
  Val (pS,I,Code);
  If Code<>0 then begin
    DebugPrint('Not an integer, Error at position '+IntToStr(code)+': '+pS);
    I := 0;
  end; //if
  StrToInt := I;
end;

Procedure zShowArr(pArr : BD255; pb, pe : Integer);
{Show contents of array pArr, row pb t/m row pe}
//pol_Init
//Use with: giBord, giRand, giBType.
//zShowArr giRand, 1, 17
//For gi = 0 To 100 Step 10: For gk = 1 To 10: Debug.Print giVeldnr(gi + gk);: Next gk: Debug.Print: Next gi
//for gi=1 to 5:for gk=0 to 7:debug.Print giburen(gi,gk);:next gk:debug.Print:next gi
//for gi=1 to 5:for gk=0 to 7:debug.Print girandstenen(gi,gk);:next gk:debug.Print:next gi
//
  var i,k : Integer;
      txt : string;
begin
  i := 15 * (pb - 1);
  While i <= 15 * (pe - 1) do
   begin
    txt := '';
    For k := 1 To 15 do
      txt := txt + IntToStr(pArr[i + k]) + ' '; 
    //Next k
    DebugPrint(txt + '| from '+IntToStr(i));  
    i := i + 15;
   end; //Next i

end; //Sub

Procedure zShowAfst(pAfst : AFST255; pKlr, pRand, pb, pe : Integer);
{Show contents of array pAfst(pKlr, pRand, rij pb t/m rij pe}
//pol_Init
//Use with: giAfstand(2, 9, 255).
//zShowAfst giAfstand, WIT, 1, 2, 16
//
  var i, k, a : Integer;
      txt, s : String;
begin
  i := 15 * (pb - 1);
  while i <= 15 * (pe - 1) do
   begin
    txt := '';
    For k := 1 To 15 do
     begin
      a := pAfst[pKlr, pRand, i + k];
      If a > 99 Then a := 99;
      s := IntToStr(a);
      If Length(s) = 1 then s := ' ' + s;
      If Length(s) = 2 then s := ' ' + s;
      //If a <= 9 Then s := ' ' + s;
      txt := txt + s;  //Debug.Print s & " ";
     end;
    //Next k
    DebugPrint(txt); //Debug.Print
    i := i + 15;
   end; //Next i

end;  //Sub

Function fVeldnr(pX : Integer) : Integer;
  Var i : Integer;
begin
  fVeldnr := 0;
  for i := 1 to 106 do begin
    if giVeldnr[i] = pX then begin
      fVeldnr := i;
      Break;
    end;
  end;
end;

Procedure zShowBuren;
  var i, t, j, b : Integer;
      s : String;
begin
  for i := 1 to 106 do begin
    s := 'Direct neighbours field '+IntToStr(i)+': ';
    t := giBType[giVeldnr[i]];
    for j := 1 to 6 do begin
      b := fVeldnr(giVeldnr[i] + giBuren[t,j]);
      if b > 0 then s := s + IntToStr(b) + '.';
    end;
    DebugPrint(s);
    s := 'Diamond neighbours field '+IntToStr(i)+': ';
    t := giRType[giVeldnr[i]];
    for j := 1 to 6 do begin
      b := fVeldnr(giVeldnr[i] + giRuiten[t,j]);
      if b > 0 then s := s + IntToStr(b) + '.';
    end;
    DebugPrint(s);
  end;
end;



Function pol_fKortsteAfstanden(var pAfst:AFST255; pKlr, pi :Integer;
                                          var ps3a, ps3, ps4:Integer):Integer;
  { Determine smallest sum of distances to 3 and 4 edges from 5 }
  //Call: som4 := pol_fKortsteAfstanden(giAfstand, pKlr, i, som3, som3x, som4x);
    var i, ih, n, iMaxN, r3, r4 : Integer;
    var bVeranderd : Boolean;
    var ix : array[1..5] of Integer;
    label Lb_n;
  begin
    ix[1] := 1; ix[2] := 2; ix[3] := 3; ix[4] := 4; ix[5] := 5;
    iMaxN := 4;
    For n := 1 To iMaxN Do
    begin
      bVeranderd := False;
      For i := 5 downto (1 + n) do  //Step -1
      begin
        If Abs(pAfst[pKlr, ix[i], pi]) < Abs(pAfst[pKlr, ix[i - 1], pi]) Then
        //exchange indexes--
        begin
          ih := ix[i - 1]; ix[i - 1] := ix[i]; ix[i] := ih;
          bVeranderd := True;
        end;  // end if;
      end;  //Next i;
      //If no changes then ready--
      If Not bVeranderd Then Break;  //Exit For;
    end; //Next n;
    //Now sum distances
    Lb_n:r3 := 0; r4 := 0; ps3 := 0; ps4 := 0;
    For i := 1 To 3 do
      r3 := r3 + Abs(pAfst[pKlr, ix[i], pi]);
      ps3 := ps3 + pAfst[pKlr, ix[i], pi];
    //Next i;
    r4 := r3 + Abs(pAfst[pKlr, ix[4], pi]);
    ps4 := ps3 + pAfst[pKlr, ix[4], pi];
    //Return results
    ps3a := r3;
    pol_fKortsteAfstanden := r4;
  End; // Function;

Procedure pol_OpbouwRuitAfstBlokSub(pKlr, pi, px, pa, pt, pOpp : Integer);
//- Fill diamondframe/chain of own stones with distance pa, start with own stone on px.
//- And fill other (diamond)neighbourfields with: (abs(pa)+1 (if empty) resp. 77 (if Opponent-stone).
//- After handling direct neighbourfields, fill fields on diamond-distance with -(abs(pa)+1).
var b, y, tb, tr, pah, cs, a : Integer;
begin
  pah := Abs(pa);
  giRuitAfst[pKlr, pi, px] := pah;
  giGhd[pi, px] := -2;
{First handle direct neighbours}
  tb := giBType[px];  //Neigbours-type
  For b := 1 To 6 Do begin  //Max. 6 neighbours
    y := px + giBuren[tb, b];
    If giGhd[pi, y] = -1 Then begin
      cs:=4;
      if giBord[y]=pOpp then begin cs:=1 end
      else
        if giBord[y]=0 then begin cs:=2 end
        else
          if giBord[y]=pKlr then begin cs:=3 end;
      Case cs of
       1 : begin //-Stone opponent-
        giRuitAfst[pKlr, pi, y] := 77;
        giGhd[pi, y] := -2;
        end;
       2 : //-Empty field-
        //Check here for opponent-diamond, if giBord[p] is 0 also!
        //If it is a diamond-block, then skip!
        If (giBord[px] <> 0) Or ( (giBord[px + giBuren[tb, b + 1]] <> pOpp) And (giBord[px + giBuren[tb, b + 1]] < 9) )
                     Or ( (giBord[px + giBuren[tb, b - 1]] <> pOpp) And (giBord[px + giBuren[tb, b - 1]] < 9) ) Then
          begin
          If Abs(giRuitAfst[pKlr, pi, y]) > pah + 1 Then begin
            giRuitAfst[pKlr, pi, y] := pah + 1;
            giGhd[pi, y] := pt + 1;
          end; // If
        end; //if
       3 : //-Own stone-
        //-recursive call-
        pol_OpbouwRuitAfstBlokSub(pKlr, pi, y, pa, pt, pOpp)
      otherwise // outside the board, do nothing--
        giGhd[pi, y] := -2;
      End; //End Select
    End; // If
  End; //Next b
{px is an own stone, so check on diamond-distance also}
  tr := giRType[px];  //Diamond-neigbours type
  For b := 1 To 6 Do begin //Max. 6 neighbours
    {--Check if the 2 fields between diamond [px, y] are both empty!!--
     => that is, the direct neighbours px+giBuren[tb,b] and px+giBuren[tb,b-1].}
    If (giBord[px + giBuren[tb, b]] = 0) And (giBord[px + giBuren[tb, b - 1]] = 0) Then
      begin
      y := px + giRuiten[tr, b];
      If giGhd[pi, y] = -1 Then begin
        //Select Case giBord[y]
        if giBord[y]=pOpp then begin //Case pOpp //-Stone opponent-
          giRuitAfst[pKlr, pi, y] := 77;
          giGhd[pi, y] := -2;
        end;
        if giBord[y]=0 then begin //Case 0  //-Empty field-
          If Abs(giRuitAfst[pKlr, pi, y]) > pah + 1 Then begin
            giRuitAfst[pKlr, pi, y] := -(pah + 1);
            giGhd[pi, y] := pt + 1;
          End; // If
        end;
        if giBord[y]=pKlr then begin //Case pKlr //-Own stone-
          //-recursief aanroepen-
          pol_OpbouwRuitAfstBlokSub(pKlr, pi, y, pa, pt, pOpp);
        end;
        if giBord[y]>8 then begin //Case Else // point outside the board, do nothing--
          giGhd[pi, y] := -2;
        End; //Select
      End; // If
    End; // If
  end; //Next b
End; //Sub

Procedure pol_OpbouwRuitAfstandenBlok(pKlr : Integer);
{Determine field diamond distances to every edge.
 --Steps:
 1. First do for the edge-fields only:
 1a. Fill diamond-distance to this edge for colour pKlr:
   - field empty => distance = 1
   - field pKlr => distance = 0
   - field Opp  => distance = "infinite", e.g. 77
 1b. distance to other edges = "undetermined", e.g. 88

 1c. for each neighbour field, fill distance to this edge
   neighbour field:
   - distance to this edge = minimum of [already determined distance or 88] and
        [ my distance + 1 (if empty) / 0 (own stone) / 77 (opponent stone) ]
 }
var i, j, k, p, opp : Integer;
var v1, ri, rt : Integer;
var a, x, t, nk, tf, rb : Integer;
var nogeens, ruit : Boolean;
begin
//-- giGhd[i,p]= -2/-1/t <=> field ready / field to do / neighbours must be handled (t>0)
//-- NB: t = t + 1 with every iteration.
//-- 1. Init. variables --
if pKlr=1 then opp := 2 else opp := 1; //opp = IIf(pKlr = 1, 2, 1)
For i := pKlr To pKlr do begin
 For p := 0 To 9 do
  For j := 0 To 255 do
   giRuitAfst[i, p, j] := 88;
  //Next j
 //Next p
end; //Next i
For p := 1 To 5 do
  For j := 0 To 255 do
    If giBord[j] > 8 Then
      giGhd[p, j] := -2
    Else
      giGhd[p, j] := -1;
    //End If
  //Next j
//Next p
//-- 2. Init. for edge-stones --
For i := 1 To 5 do begin
  nk := 0;  //Number of edge-fields filled with Opponents stones
  For k := 1 To giRandstenen[i, 0] do begin
    p := giRandstenen[i, k];
    //fill distance field p to edge: empty = 1, own stone = 0, opp.stone = 77
    //Select Case giBord[p]
    if giBord[p] = 0 then begin //Case 0
      giRuitAfst[pKlr, i, p] := 1;
      giGhd[i, p] := 0;
    end;
    if giBord[p] = opp then begin //Case opp
      nk := nk + 1;
      giRuitAfst[pKlr, i, p] := 77;
      giGhd[i, p] := -2;
    end;
    if giBord[p] = pKlr then //Case pKlr
      pol_OpbouwRuitAfstBlokSub(pKlr, i, p, 0, 0, opp);
    //End Select
  End; //Next k
  //Init for diamond-distance.
 //diamond-direction and -type for check on p diamond-distance for this edge.
  ri := giRandRuitRT[i, 1]; rt := giRandRuitRT[i, 2];
  For k := 1 To giRandRuit[i, 0] do begin
    v1 := giRandRuit[i, k];
    p := v1 + giRuiten[rt, ri];
    If (giBord[v1 + giBuren[rt, ri]] = 0) And (giBord[v1 + giBuren[rt, ri - 1]] = 0) Then
      begin
      //Fields inbetween are empty, so a real diamond.
      //Select Case giBord[p]
      if giBord[p] = 0 then begin //Case 0
        giRuitAfst[pKlr, i, p] := -1;
        giGhd[i, p] := 0;
      end;
      if giBord[p] = opp then begin //Case opp
        giRuitAfst[pKlr, i, p] := 77;
        giGhd[i, p] := -2;
      end;
      if giBord[p] = pKlr then //Case pKlr
        pol_OpbouwRuitAfstBlokSub(pKlr, i, p, 0, 0, opp);
      //End Select
    End; //If
  End; //Next k
  If nk = giRandstenen[i, 0] Then  //Whole edge filled with Opp-stones? --
    For j := 0 To 255 do begin
      giRuitAfst[pKlr, i, j] := 77;
      giGhd[i, j] := -2;
    end; //Next j
  //End If
End; //Next i
//--Now iterations to handle all direct neighbour and diamond neighbour field distances.
t := 0;
Repeat
nogeens := False; ruit := False;
//First loop for direct neighbours:
For p := 36 To 219 do
  For i := 1 To 5 do
    If giGhd[i, p] = t Then begin //handle field for edge i.
      //Check directe neighbours
      rb := giBType[p];       //Type neighbours
      For j := 1 To 6 do begin
        x := p + giBuren[rb, j];
        If giGhd[i, x] = -1 Then begin
          //Select Case giBord[x]
          if giBord[x] = opp then begin //Case opp
            giRuitAfst[pKlr, i, x] := 77;
            giGhd[i, x] := -2;
          end;
          if giBord[x] = pKlr then begin //Case pKlr
            pol_OpbouwRuitAfstBlokSub(pKlr, i, x, giRuitAfst[pKlr, i, p], t + 1, opp);
            nogeens := True;
          end;
          if giBord[x] = 0 then begin //Case 0
            //Check here for opponent diamond, if giBord[p] is 0 also!
            //Indien ruit-block, dan overslaan ipv flink ophogen!!
            If (giBord[p] <> 0) Or ( (giBord[p + giBuren[rb, j + 1]] <> opp) And (giBord[p + giBuren[rb, j + 1]] < 9) )
                          Or ( (giBord[p + giBuren[rb, j - 1]] <> opp) And (giBord[p + giBuren[rb, j - 1]] < 9) ) Then
              begin
              a := Abs(giRuitAfst[pKlr, i, p]) + 1;
              If Abs(giRuitAfst[pKlr, i, x]) > a Then giRuitAfst[pKlr, i, x] := a;
              giGhd[i, x] := t + 1;
              nogeens := True;
            end; //if
          end; //if
          //End Select
        End; //If
      End; //Next j
      //If Afst(p)>0 then later on no check for diamond!
      If giRuitAfst[pKlr, i, p] > 0 Then giGhd[i, p] := -2;
    End; //If giGhd[i, p] = t
  //Next i
//Next p
//Now loop for check on diamonds.
For p := 36 To 219 do
  For i := 1 To 5 do
    If giGhd[i, p] = t Then begin //handle field for edge i.
      If giRuitAfst[pKlr, i, p] <= 0 Then begin //Yes!
        rb := giBType[p];       //Type neighbours
        tf := giRType[p];       //Type diamond neighbours
        For j := 1 To 6 do begin
          x := p + giRuiten[tf, j];
          If (giGhd[i, x] = -1) And (giBord[p + giBuren[rb, j]] = 0)
              And (giBord[p + giBuren[rb, j - 1]] = 0) Then begin
            //If inbetween fields are empty, then:
            //Select Case giBord[x]
            if giBord[x] = opp then begin //Case opp
              giRuitAfst[pKlr, i, x] := 77;
              giGhd[i, x] := -2;
              end;
            if giBord[x] = pKlr then begin //Case pKlr
              pol_OpbouwRuitAfstBlokSub(pKlr, i, x, giRuitAfst[pKlr, i, p], t + 1, opp);
              nogeens := True;
              end;
            if giBord[x] = 0 then begin //Case 0
              a := Abs(giRuitAfst[pKlr, i, p]) + 1;
              If Abs(giRuitAfst[pKlr, i, x]) > a Then begin
                giRuitAfst[pKlr, i, x] := -a;
                giGhd[i, x] := t + 1;
                nogeens := True;
              End; //If
            End; //If
            //End Select
          End; //If giBord[p]=0 And giRuitAfst[pKlr,i,p]<0
        End; //Next j
      End; //If
      giGhd[i, p] := -2;
    End; //If giGhd[i, p] = t
  //Next i
//Next p
t := t + 1;
Until Not nogeens;

if pKlr = spMe then begin
  zShowAfst(giRuitAfst, pKlr, 1, 3, 16);
  zShowAfst(giRuitAfst, pKlr, 2, 3, 16);
  zShowAfst(giRuitAfst, pKlr, 3, 3, 16);
  zShowAfst(giRuitAfst, pKlr, 4, 3, 16);
  zShowAfst(giRuitAfst, pKlr, 5, 3, 16);
end;

//Fields "hidden" behind opponents diamonds: giRuitAfst(pKlr, i, p) >= 77 --
For p := 36 To 219 Do begin
  k := 0;
  For i := 1 To 5 Do begin
    If (giBord[p] <= 8) And (giRuitAfst[pKlr, i, p] >= 77) Then k := k + 1
  end; //Next i
  If k > 2 Then //set distance = 88 for all edges
    For j := 1 To 5 Do begin
      giRuitAfst[pKlr, j, p] := 88
    end; //Next j
  //End If
end; //Next p

End; // Sub

Function pol_OpbouwSomAfst(var pAfst : AFST255; pKlr:Integer; var ps4a:Integer) : Integer;
{ Determine smallest sum of 3/4 edge-distances per field.
  Fill pAfst(pKlr,6 en 8,i)=3edges and pAfst(pKlr,7 en 9,i)=4edges,
  and remember smallest values.
 }
  var i, s3, s4, am, s3a, s4a, pInv : Integer;
      z : array[1..3] of Integer;
      txt : String;
begin
  pinv := 1;
  //z[1]=Abs(afst);z[2]=non-abs(afst);z[3]=index giBord.
  For i := 0 To 106 do begin
    giMx[pKlr, i, 1] := 999; giMx[pKlr, i, 2] := 0; giMx[pKlr, i, 3] := 0;
  end; //Next i
  am := 1;
  For i := 33 To 219 do begin
    If giBord[i] < 9 Then begin
      s4a := pol_fKortsteAfstanden(pAfst, pKlr, i, s3a, s3, s4);
      pAfst[pKlr, 6, i] := s3a;
      pAfst[pKlr, 7, i] := s4a;
      pAfst[pKlr, 8, i] := s3;
      pAfst[pKlr, 9, i] := s4;
      If (giBord[i] = 0) And (s4a < giMx[pKlr, 1, 1]) Then begin //new smallest value
        am := 1;
        giMx[pKlr, 1, 1] := s4a;
        giMx[pKlr, 1, 2] := s4;
        giMx[pKlr, 1, 3] := i;
        z[1] := s4a; z[2] := s4; z[3] := i;
        end;
      If (giBord[i] = 0) And (s4a = giMx[pKlr, 1, 1]) Then begin //add same smallest value
        am := am + 1;
        giMx[pKlr, am, 1] := s4a;
        giMx[pKlr, am, 2] := s4;
        giMx[pKlr, am, 3] := i;
        If s4 < z[2] Then begin //New 'best'
          z[1] := s4a; z[2] := s4; z[3] := i;
        End; //If
      End; //If
    End; //If
  End; //Next i;
  txt := 'OpbouwSomAfst('+IntToStr(pKlr)+'): I found '+IntToStr(am)+' smallest s4-values.';
  DebugPrint(txt);
  giMx[pKlr, 0, 1] := am;
  if pInv = 1 then begin
    for i := 1 to am do begin
      giMbu[pKlr, i, 1] := giMx[pKlr, (am-i)+1, 1];
      giMbu[pKlr, i, 2] := giMx[pKlr, (am-i)+1, 2];
      giMbu[pKlr, i, 3] := giMx[pKlr, (am-i)+1, 3];
    end;
    for i := 1 to am do begin
      giMx[pKlr, i, 1] := giMbu[pKlr, i, 1];
      giMx[pKlr, i, 2] := giMbu[pKlr, i, 2];
      giMx[pKlr, i, 3] := giMbu[pKlr, i, 3];
    end;
    z[1] := giMx[pKlr,1,1];
    z[2] := giMx[pKlr,1,2];
    z[3] := giMx[pKlr,1,3];
  end;
  ps4a := z[1];
  txt := 'Value = '+IntToStr(z[1])+'/'+IntToStr(z[2])+'; best field = '+IntToStr(z[3]);
  DebugPrint(txt);
  pol_OpbouwSomAfst := z[3];

End; //Function

Function pol_Evaluatie(var pAfst : AFST255; pKlr:Integer) : Integer;
{ Determine evaluation-value per field
 }
  var i, h, hs, ho, s3a, opp : Integer;
      s4aMe, zMe, s4aOpp, zOpp : Integer;
      z : array[1..5] of integer;
      txt : String;
begin
  //giMx[,,1]=Abs(afst);giMx[,,2]=non-abs(afst);giMx[,,3]=index giBord.
  //z[1]=Abs(afst);z[2]=non-abs(afst);z[3]=index giBord.
  z[1] := 999; z[2] := 0; z[3] := 0; z[4] := 999; z[5] := 999;
  If pKlr = 1 then opp := 2 else opp := 1;
  z[3]:= pol_OpbouwSomAfst(pAfst, pKlr, s4aMe);
  If giMx[pKlr, 0, 1] > 1 Then begin
    //There are 2 or more "equal" moves. Look which move bothers opponent the most.
    zOpp := pol_OpbouwSomAfst(pAfst, opp, s4aOpp);
    //Search which move in giMx has the smallest sumvalue in giRuitAfst[opp, 7, 1..255]
    For i := 1 To giMx[pKlr, 0, 1] do begin
      h := giInvloedC[giMx[pKlr, i, 3]];
      hs := h + giMx[pKlr, i, 1];
      ho := h + pAfst[opp, 7, giMx[pKlr, i, 3]];
      {Test for opponents sumvalue with preference for more central fields}
      If (pAfst[opp, 7, giMx[pKlr, i, 3]] < z[1]) Or
          ( (pAfst[opp, 7, giMx[pKlr, i, 3]] = z[1]) And (z[4] > hs) ) Then begin
        //( (pAfst[opp, 7, giMx[pKlr, i, 3]] = z[1]) And (z[2] > ho) ) Then begin
        z[1] := pAfst[opp, 7, giMx[pKlr, i, 3]];
        z[2] := ho;
        z[3] := giMx[pKlr, i, 3];
        z[4] := hs;
        z[5] := giMx[pKlr, i, 2] + h;
        Continue
      End; //If
      {Test for central + pace}
//    If ( (pAfst[opp, 7, giMx[pKlr, i, 3]] = z[1]) And (z[2] = ho) )
//       And ( (hs < z[4]) Or ( (hs = z[4]) And ((giMx[pKlr, i, 2] + h) < z[5])) ) Then begin
//      z[4] := hs;
      If ( (pAfst[opp, 7, giMx[pKlr, i, 3]] = z[1]) And (z[4] = hs) )
         And ( (ho < z[2]) Or ( (ho = z[2]) And ((giMx[pKlr, i, 2] + h) < z[5])) ) Then begin
        z[2] := ho;
        z[5] := giMx[pKlr, i, 2] + h;
        z[3] := giMx[pKlr, i, 3];
        Continue //GoTo Nx
      End; //If
    End; //Next i
    DebugPrint('Move that bothers Opp + central: '+IntToStr(z[3]));
  End; //If
  pol_Evaluatie := z[3];

End;  //Function


Function pol_Eval_random : Integer;
//Do a move at random. (not used)
  var i : Integer;
begin
  repeat
    i :=random(106)+1; // Int((106 * Rnd) + 1)
  until giBord[giVeldnr[i]] = 0;
  //wln = wln & ' My move is: ' & i
  pol_Eval_random := giVeldnr[i];

end;  //Function

Procedure pol_OpbouwenAfstandSub(pKlr, pi, px, pa, pt, pOpp : Integer);
{- Fill chain of own stones with distance pa, start with own stone at px.
 - Also fill neighbourfields with: pa+1 (if empty) resp. 77 (if Opponent-stone).
 }
 var b, y, t : Integer;
begin
  giAfstand[pKlr, pi, px] := pa;
  giGhd[pi, px] := -2;
  t := giBType[px];  //Type neighbours
  For b := 1 To 6 Do  //Max. 6 neighbours
   begin
    y := px + giBuren[t, b];
    If giGhd[pi, y] = -1 Then begin
      //Case giBord[y] of
      if giBord[y] = pOpp then begin
        //-Stone opponent-
        giAfstand[pKlr, pi, y] := 77;
        giGhd[pi, y] := -2;
      end; //if
      if giBord[y] = 0 then
        //-Empty field-
        If giAfstand[pKlr, pi, y] > pa + 1 Then begin
          giAfstand[pKlr, pi, y] := pa + 1;
          giGhd[pi, y] := pt + 1;
        End; //if
      //End if
      if giBord[y] = pKlr then //-Own stone-
        //-recursive call-
        pol_OpbouwenAfstandSub(pKlr, pi, y, pa, pt, pOpp);
      //Else
      if giBord[y] > 8 then // point outside the board, do nothing--
        giGhd[pi, y] := -2;
      //End Select
    End; //if
   end; //Next b
End; //Sub

Procedure pol_OpbouwenAfstanden(pKlr : Integer);
{Determine direct field distances to every edge.
 --Steps:
 1. First do for the edge-fields only:
 1a. Fill distance to this edge for colour pKlr:
   - field empty => distance = 1
   - field pKlr => distance = 0
   - field Opp  => distance = "infinite", e.g. 77
 1b. distance to other edges = "undetermined", e.g. 88

 1c. for each neighbour field, fill distance to this edge
   neighbour field:
   - distance to this edge = minimum of [already determined distance or 88] and
        [ my distance + 1 (if empty) / 0 (own stone) / 77 (opponent stone) ]
}

  var i, j, k, p, Opp, st : Integer;
      a, x, t, nk, tf : Integer;
      nogeens, ruit : Boolean;
      txt : string;
begin
//-- giGhd[i,p]= -2/-1/t <=> field ready / field to do / neighbours must be handled (t>0)
//-- NB: t = t + 1 with every iteration.
//-- 1. Init. variables --
If pKlr = 1 then Opp := 2 else Opp := 1;
If pKlr = 1 then st := 1 else st := 2;
For i := st To st do  //Useless, cannot remember why I did this extra loop... but no harm is done.
 For p := 1 To 7 do
  For j := 0 To 255 do
   giAfstand[i, p, j] := 88;    { NULL }
  //Next j
 //Next p
//Next i
For p := 1 To 5 do
  For j := 0 To 255 do
    giGhd[p, j] := -1;
  //Next j
//Next p

//-- 2. Init. for edge-stones --
For i := 1 To 5 do begin
  nk := 0;  //Number of edge-fields filled with Opponents stones
  For k := 1 To giRandstenen[i, 0] do
   begin
    p := giRandstenen[i, k];
    //fill distance field p to edge: empty = 1, own stone = 0, opp.stone = 77
    {Select Case giBord[p]}
    If giBord[p] = 0 then begin
      giAfstand[pKlr, i, p] := 1;
      giGhd[i, p] := 0;
      end;
    If giBord[p] = Opp then begin
      nk := nk + 1;
      giAfstand[pKlr, i, p] := 77;
      giGhd[i, p] := -2;
      end;
    If giBord[p] = pKlr then
      pol_OpbouwenAfstandSub(pKlr, i, p, 0, 0, Opp);
    //End Select
   end; //Next k
  If nk = giRandstenen[i, 0] Then  //Whole edge filled with Opp-stones? --
    For j := 0 To 255 do begin
      giAfstand[pKlr, i, j] := 77;
      giGhd[i, j] := -2;
    end; //Next j
  //End If
end; //Next i
//--Now iterate to handle all direct neighbour field distances.
t := 0;
Repeat //Do
nogeens := False; ruit := False;
For p := 36 To 219 Do
{ For i := 2 To 2  '//tbv Test}
  For i := 1 To 5 Do
    If giGhd[i, p] = t Then  //handle field for edge i
     begin
      tf := giBType[p];      //Type neighbours
      For j := 1 To 6 Do begin
        x := p + giBuren[tf, j];
        If giGhd[i, x] = -1 Then
         begin //Select Case giBord[x]
          if giBord[x] = Opp then begin
            giAfstand[pKlr, i, x] := 77;
            giGhd[i, x] := -2;
            end;
          if giBord[x] = pKlr then begin
            pol_OpbouwenAfstandSub(pKlr, i, x, giAfstand[pKlr, i, p], t + 1, Opp);
            nogeens := True;
            end;
          if giBord[x] = 0 then
            //--Check here for opponent diamond block.
            If (giBord[p + giBuren[tf, j - 1]] = Opp) And (giBord[p + giBuren[tf, j + 1]] = Opp) Then begin
              ruit := True;
              giAfstand[pKlr, i, x] := 66;
              giGhd[i, x] := -1;  //--Diamond block, handle later on?--
              end
            Else  begin
              a := giAfstand[pKlr, i, p] + 1;
              If giAfstand[pKlr, i, x] > a Then giAfstand[pKlr, i, x] := a;
              giGhd[i, x] := t + 1;
              nogeens := True;
              End; //Else
            //End if
          end; //Select
        //End If
      end; //Next j;
      giGhd[i, p] := -2;
     end; //If
  //Next i;
//Next p;
t := t + 1;
Until Not nogeens;

End; //Sub

Procedure pol_InitInvloedC;
//-Put data from DATA_INVL_C into giInvloedC
  var i, w, h, j : Integer;
      s : String;
  Const TRANS : String = '.0123456';
begin
  For i := 1 To 17 do begin
    h := (i - 1) * 15;
    For j := 1 To 15 do begin
      s := Copy(DATA_INVL_C[i], j, 1); //Mid$(DATA_INVL_C[i], j, 1);
      w := Pos(s, TRANS) - 2;          //InStr(TRANS, s) - 2;
      If w < 0 Then
        giInvloedC[h + j] := 9
      Else
        giInvloedC[h + j] := w;
      //End If
    End; //Next j
  End; //Next i

End; //Sub

Procedure pol_InitSwap;
//-Put data from DATA_SWAP into giSwap
  var i, w, h, j : Integer;
      s : String;
  Const TRANS : String = '.0123456';
begin
  For i := 1 To 17 do begin
    h := (i - 1) * 15;
    For j := 1 To 15 do begin
      s := Copy(DATA_SWAP[i], j, 1); //Mid$(DATA_INVL_C[i], j, 1);
      w := Pos(s, TRANS) - 2;          //InStr(TRANS, s) - 2;
      If w < 0 Then
        giSwap[h + j] := 9
      Else
        giSwap[h + j] := w;
      //End If
    End; //Next j
  End; //Next i

End; //Sub

//-Const INITTRANS = 'EDCBA.0123456789%#abcdefghvwxyz';  {Helparray for init.boardvariables}
//                    5432101234567890123456789012345
Procedure pol_InitRij(var piBord:BD255; iStart:Integer; sWaarde:string; var piRand:BD255;
                       var piRandstenen:RS5_7; var piVeldnr:V_110; var piBType:BD255;
                       var piRType:BD255; var piRandRuit:RR5_6);
//-
  var i, w, h, f, hs : Integer;
      s : String;
begin
  For i := 1 To 15 do
   begin
    s := Copy(sWaarde, i, 1);
    w := Pos(s, INITTRANS) - 6;  {Cornerfields are -1 t/m -5}
    //--Init giBord--
    If (w=0) or (w > 20) then piBord[iStart + i] := 9 else piBord[iStart + i] := 0;
    //--Init giVeldnr--
    If (w <> 0) And (w <= 20) Then //Fill fieldnumbers.
     begin
      piVeldnr[0] := piVeldnr[0] + 1;
      piVeldnr[piVeldnr[0]] := iStart + i;
     End; //If

    //--Neighbourtype en Diamond neighbourtype--
    If (w >= 1) And (w <= 12) Then begin
      piRType[iStart + i] := w; {-normal field; Also for %=T11 en #=T12.}
      If w <= 5 Then piBType[iStart + i] := w;
      If (w = 6) Or (w = 7) Then piBType[iStart + i] := 1; {5:T6, 6:T7}
      If (w = 8) or (w = 9) or (w = 12) Then piBType[iStart + i] := 2; {7:T8, 8:T9, #:T12}
      If (w = 10) Then piBType[iStart + i] := 3; {9:T10}
      If (w = 11) Then piBType[iStart + i] := 4; {%:T11}
      end; //if
    If s = 'f' Then  begin {-edgefield, fieldnr 2}
      piRType[iStart + i] := 6;
      piBType[iStart + i] := 1;
      end;
    If s = 'g' Then begin {-edgefield, fieldnr 4}
      piRType[iStart + i] := 8;
      piBType[iStart + i] := 2;
      end;
    If s = 'h' Then begin {-edgefield}
      piRType[iStart + i] := 7;
      piBType[iStart + i] := 1;
      end;

    //--Edgefields--
    If (w >= 13) And (w <= 17) Then {-edgefield.}
     begin
      If (w - 12) = 1 Then begin//Edge a
       piBType[iStart + i] := 2;
       piRType[iStart + i] := 2;
       end
      Else begin
        piBType[iStart + i] := 1;
        piRType[iStart + i] := 1;
        end;
      //End If
      {-Fill piRand/piRandstenen}
      piRand[iStart + i] := w - 12;
      piRandstenen[w - 12, 0] := piRandstenen[w - 12, 0] + 1;
      piRandstenen[w - 12, piRandstenen[w - 12, 0]] := iStart + i;
     end; //If
    If s = 'f' Then begin
      piRand[iStart + i] := 5;
      piRandstenen[5, 0] := piRandstenen[5, 0] + 1;
      piRandstenen[5, piRandstenen[5, 0]] := iStart + i;
      end;
    If s = 'g' Then begin
      piRand[iStart + i] := 1;
      piRandstenen[1, 0] := piRandstenen[1, 0] + 1;
      piRandstenen[1, piRandstenen[1, 0]] := iStart + i;
      end;
    If s = 'h' Then begin
      piRand[iStart + i] := 2;
      piRandstenen[2, 0] := piRandstenen[2, 0] + 1;
      piRandstenen[2, piRandstenen[2, 0]] := iStart + i;
      end;
   // End If
    //--Outer edgefields for diamond frame search initialisation--
    If w > 20 Then begin
      piRandRuit[w - 20, 0] := piRandRuit[w - 20, 0] + 1;
      piRandRuit[w - 20, piRandRuit[w - 20, 0]] := iStart + i;
      end;
    //End If

    //--Cornerfields--
    If w < 0 Then hs:=Pos(s, 'ABCDE'); {-cornerfield.}
    If w < 0 Then {-cornerfield.}
      Case hs of
       1 : begin  //'A'
        piBType[iStart + i] := 3;
        piRType[iStart + i] := 3;
        piRand[iStart + i] := -1; {PROBLEM: BELONGS TO EDGES 1 AND 5...}
        piRandstenen[1, 0] := piRandstenen[1, 0] + 1;
        piRandstenen[1, piRandstenen[1, 0]] := iStart + i;
        piRandstenen[5, 0] := piRandstenen[5, 0] + 1;
        piRandstenen[5, piRandstenen[5, 0]] := iStart + i;
        end;
       2 : begin  //'B'
        piBType[iStart + i] := 4;
        piRType[iStart + i] := 4;
        piRand[iStart + i] := -2; {PROBLEM: BELONGS TO EDGES 1 AND 2...}
        piRandstenen[1, 0] := piRandstenen[1, 0] + 1;
        piRandstenen[1, piRandstenen[1, 0]] := iStart + i;
        piRandstenen[2, 0] := piRandstenen[2, 0] + 1;
        piRandstenen[2, piRandstenen[2, 0]] := iStart + i;
        end;
       3 : begin  //'C'
        piBType[iStart + i] := 1;
        piRType[iStart + i] := 1;
        piRand[iStart + i] := -3; {PROBLEM: BELONGS TO EDGES 2 AND 3...}
        piRandstenen[2, 0] := piRandstenen[2, 0] + 1;
        piRandstenen[2, piRandstenen[2, 0]] := iStart + i;
        piRandstenen[3, 0] := piRandstenen[3, 0] + 1;
        piRandstenen[3, piRandstenen[3, 0]] := iStart + i;
        end;
       4 : begin  //'D'
        piBType[iStart + i] := 1;
        piRType[iStart + i] := 1;
        piRand[iStart + i] := -4; {PROBLEM: BELONGS TO EDGES 3 AND 4...}
        piRandstenen[3, 0] := piRandstenen[3, 0] + 1;
        piRandstenen[3, piRandstenen[3, 0]] := iStart + i;
        piRandstenen[4, 0] := piRandstenen[4, 0] + 1;
        piRandstenen[4, piRandstenen[4, 0]] := iStart + i;
        end;
       5 : begin  //'E'
        piBType[iStart + i] := 1;
        piRType[iStart + i] := 1;
        piRand[iStart + i] := -5; {PROBLEM: BELONGS TO EDGES 4 AND 5...}
        piRandstenen[4, 0] := piRandstenen[4, 0] + 1;
        piRandstenen[4, piRandstenen[4, 0]] := iStart + i;
        piRandstenen[5, 0] := piRandstenen[5, 0] + 1;
        piRandstenen[5, piRandstenen[5, 0]] := iStart + i;
        end;
      End; // Case
    //End If
  end; //Next i;
End; // Sub

//----------------------------------------------------------------------------
Procedure pol_TestInit;
//Initialize for a new game.
  var i, j : Integer;
      txt : string;
      pb : array[0..6] of Integer;
      pR : array[0..6] of Integer;
begin
  //CRLF = Chr$(13) & Chr$(10)
  //Neighbours: (0th/7th item to ease check for diamond: pb[i)+pb[i+1)=pR(i) !!)
  //16,15,14,-1,-16,-15,-14, 1
  For i:=1 to 5 do
    For j:=0 to 7 do
      giBuren[i,j]:=0;
  pb[1] := 15; pb[2] := 14; pb[3] := -1; pb[4] := -15; pb[5] := -14; pb[6] := 1;
  For i := 1 To 6 do giBuren[1, i] := pb[i];
  giBuren[1, 7] := pb[1]; giBuren[1, 0] := pb[6];
  pb[1] := 16; pb[2] := 15; pb[3] := -1; pb[4] := -16; pb[5] := -15; pb[6] := 1;
  For i := 1 To 6 do giBuren[2, i] := pb[i];
  giBuren[2, 7] := pb[1]; giBuren[2, 0] := pb[6];
  pb[1] := 16; pb[2] := 15; pb[3] := 14; pb[4] := -1; pb[5] := -15; pb[6] := 1;
  For i := 1 To 6 do giBuren[3, i] := pb[i];
  giBuren[3, 7] := pb[1]; giBuren[3, 0] := pb[6];
  pb[1] := 15; pb[2] := 14; pb[3] := -1; pb[4] := -16; pb[5] := -15; pb[6] := 1;
  For i := 1 To 6 do giBuren[4, i] := pb[i];
  giBuren[4, 7] := pb[1]; giBuren[4, 0] := pb[6];
  {-- Center (field 43) special case, only 5 neighbours; --}
  pb[1] := 15; pb[2] := 14; pb[3] := -1; pb[4] := -15; pb[5] := 1; pb[6] := 15;
  For i := 1 To 6 do giBuren[5, i] := pb[i];
  giBuren[5, 7] := pb[2]; giBuren[5, 0] := pb[5];

  //Testdisplay:
  txt := '';
  DebugPrint('giBuren[1..5, 0..7]');
  for i:=1 to 5 do begin
    txt := 'R'+IntToStr(i)+': ';
    for j:=0 to 7 do
      txt := txt + IntToStr(giburen[i,j]) + ' ';
    DebugPrint(txt);
  end;
  //------------

  //--giRuiten: Now declared as Constant--

  //giRandRuitRT[1..5,1..2]:  [i,1]=>row [RType];[i,2]=>column in giRuiten for edge i
  pb[1] := 3; pb[2] := 4; pb[3] := 5; pb[4] := 6; pb[5] := 1;
  For i := 1 To 5 do giRandRuitRT[i, 1] := pb[i];
  pb[1] := 2; pb[2] := 1; pb[3] := 1; pb[4] := 1; pb[5] := 1;
  For i := 1 To 5 do giRandRuitRT[i, 2] := pb[i];


  For i := 0 To 255 do
   begin
    giBord[i] := 0;
    giRand[i] := 0;
    giBType[i] := 0;
    If i <= 110 Then
     begin
      giVeldnr[i] := 0;
      giZetten[i, 1] := 0; giZetten[i, 2] := 0;
     End; //If
    // giNullen[i] := 0
   end; //Next i

  For i := 0 To 5 do
    For j := 0 To 7 do begin
      giRandstenen[i, j] := 0; // Next j
      If (i > 0) and (j < 7) Then giRandRuit[i, j] := 0;
    end; //Next j
  //Next i

  pol_InitInvloedC;
  zShowArr(giInvloedC, 1, 17);

  pol_InitSwap;
  zShowArr(giSwap, 1, 17);

  pol_InitRij(giBord, 0, '...............', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 15, '...............', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 30, '.......zAv.....', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 45, '......zf2gv....', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 60, '.....ze527av...', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 75, '....ze05271av..', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 90, '...ze0052711av.', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 105, '..ze00059#888av', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 120, '..E000004%3333B', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 135, '.yd0000066666hw', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 150, '.yd000000000bw.', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 165, '.yd00000000bw..', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 180, '.yd0000000bw...', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 195, '.yd000000bw....', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 210, '.yDcccccCw.....', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 225, '..xxxxxx.......', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);
  pol_InitRij(giBord, 240, '...............', giRand, giRandstenen, giVeldnr, giBType, giRType, giRandRuit);

  zShowBuren;
{  flush(output);
  readln(txt);  //write_readln wln, s
}
  //Gebruiken bij: giBord, giRand, giBType.
  DebugPrint('giBord:');
  zShowArr(giBord, 1, 17);
  DebugPrint('giRand:');
  zShowArr(giRand, 1, 17);
  DebugPrint('giBType:');
  zShowArr(giBType, 1, 17);

  DebugPrint('giRandstenen:');
  for i:=1 to 5 do begin
    txt := 'i='+IntToStr(i)+': ';
    for j:=0 to 7 do
      txt := txt + IntToStr(giRandstenen[i,j]) + ' ';
    DebugPrint(txt);
  end;

  DebugPrint('giVeldnr[0] = ' + IntToStr(giVeldnr[0]));
  i := 0;
  while  i <= 99 do begin
    txt := '';
    for j:=1 to 11 do
      txt := txt + IntToStr(giVeldnr[i+j]) + ' ';
    DebugPrint(txt + '| vanaf ' + IntToStr(i+1));
    i := i + 11;
  end;

  pol_OpbouwenAfstanden(1);  //Voor white
  pol_OpbouwRuitAfstandenBlok(1);
  DebugPrint('giAfstanden white edge 1:');
  zShowAfst(giAfstand, WIT, 1, 1, 17);
  DebugPrint('giAfstanden white edge 5:');
  zShowAfst(giAfstand, WIT, 5, 1, 17);
  i := pol_Evaluatie(giAfstand, 1);
  DebugPrint('giAfstanden white sum 3-edges:');
  zShowAfst(giAfstand, WIT, 6, 1, 17);
  DebugPrint('giAfstanden white sum 4-edges:');
  zShowAfst(giAfstand, WIT, 7, 1, 17);

  DebugPrint('giRuitAfst white edge 1:');
  zShowAfst(giRuitAfst, WIT, 1, 1, 17);
  DebugPrint('giRuitAfst white sum 4-edges:');
  zShowAfst(giRuitAfst, WIT, 7, 1, 17);

End; //Sub

Function pol_IsRuitaanval(pz, pKlr : Integer; var pv : Integer) : Boolean;
{Check if opponents move pz is an attack of my (pKlr) diamond connection.
 If so, return True and return the alternate empty field in pv.}
  var i, n, p, t, opp : Integer;
      br : Boolean;
      v : array[1..3] of Integer;
begin
  pol_IsRuitaanval := False;
  br := False;
  If pKlr = 1 then opp := 2 else opp := 1; //opp = IIf(pKlr = 1, 2, 1)
  For i := 1 To 3 do v[i] := -1; //Next i
  t := giBType[pz]; n := 1;
//If "edge-attack" (attack of diamond connection with edge) then keep apart.
//If there is also a "normal" diamond attack, then that is the preferred defense.
  For i := 0 To 7 do begin
    p := pz + giBuren[t, i];
    //Select Case giBord[p]
    If giBord[p] = 0 then //Case 0
      If n = 2 Then begin
        v[2] := p;
        n := 3
        end
      Else
        If n = 3 Then n := 1; //2 empty fields after own stone.
      //End If
    If (giBord[p] = pKlr) or (giBord[p] = 9) then //Case pKlr, 9 //9 => pz must be an edge-field!
      begin
      If n <= 2 Then begin
        v[1] := p; n := 2;
        if giBord[p] = 9 then br := True else br := False;  //Check for edge-attack.
      End; //If
      //End If
      If n = 3 Then  //We have a diamond attack!
        if br or (giBord[p] = 9) then begin //edge-attack, keep apart and continue checking.
          pv := v[2];
          pol_IsRuitaanval := True;
          v[1] := p; n := 2;
          if giBord[p] = 9 then br := True else br := False;  //Check for edge-attack.
          end
        Else begin      //"normal" attack.
          pv := v[2];
          pol_IsRuitaanval := True;
          Break;
        End; //If
    End; //if
    If giBord[p] = opp then n := 1; //Case opp
    //End Select
  End; //Next i
  //return result
End; //Function

//----------------------------------------------------------------------------

Procedure pol_Main;
{Main program}
  var i, j, z, antw, code, zi, zr, hs, zx : Integer;
      ok : Boolean;
      s, txt, wln : String;
begin
  DebugPrint('Polly is ready for the battle. Please input ("start", "quit", -1=swap, or a fieldnumber)');
  pol_TestInit;  // Initialize
  spOpp := WIT; spMe := ZWART; giZetnr := 0;
  z := 0;
  Repeat //Do   '{In Pascal: repeat ... until z < -1}
    //DebugPrint(wln);
    flush(output);
    readln(s);  //write_readln wln, s
    s := lowerCase(s);

    hs := 0;
    if s = 'start' then hs := 1;
    if s = 'quit' then hs := 2;
    if s = '-1' then hs := 3;

    Case hs of
     1 : begin //'start'
         DebugPrint('ingelezen: start'); // I play White.
         If giZetnr <> 0 Then
           DebugPrint('**ERROR: "start" not allowed at move number ' + IntToStr(giZetnr) + '**');
         //End If
         spMe := WIT; spOpp := ZWART;
         z := 0;
         end;
     2 : begin //'quit'
         DebugPrint('input: quit');
         z := -2;
         end;
     3 : begin //'-1'
         DebugPrint('input: -1 (=swap)');
         z := -1;
         If giZetnr <> 1 Then
           DebugPrint('**ERROR: swap not allowed at move number ' + IntToStr(giZetnr) + '**');
         //End If
         end;
      Else //No start, quit or -1, so it must be a move, a positive integer
        begin
          DebugPrint('input: ' + s);
          z := StrToInt(s);  //Eval(s)
          DebugPrint('; So your move is ' + IntToStr(z));
          If z > 0 Then  //fill zi with the proper index in giBord, check if that field is still empty.
           begin
            zi := giVeldnr[z];
            If giBord[zi] <> 0 Then
              DebugPrint('**ERROR: field ' + IntToStr(z) + '(=> ' + IntToStr(zi) + ') is not empty!**');
            end; //If
          //End If;
        end;
    End; //Case Select;

    If z < -1 Then Break; // Exit Do  '//quit

    If z = -1 Then        //swap
      //change colour of move giZetnr in spOpp, and add -1 as next move.
     begin
      zi := giBord[giZetten[giZetnr, 2]];
      giBord[giZetten[giZetnr, 2]] := spOpp;
      giZetnr := giZetnr + 1;
      giZetten[giZetnr, 1] := -1;
      giZetten[giZetnr, 2] := zi;
     end
    Else
      If z > 0 Then  //handle move zi for opponent
       begin
        giBord[zi] := spOpp;
        giZetnr := giZetnr + 1;
        giZetten[giZetnr, 1] := z;
        giZetten[giZetnr, 2] := zi;
       end;
    //End; //If

    //now do own move (giZetnr+1) [not too good as first move!]
    pol_OpbouwenAfstanden(spMe);  //Voor me
    pol_OpbouwRuitAfstandenBlok(spMe);
   {
    zShowAfst(giRuitAfst, spMe, 1, 3, 16);
    zShowAfst(giRuitAfst, spMe, 2, 3, 16);
    zShowAfst(giRuitAfst, spMe, 3, 3, 16);
    zShowAfst(giRuitAfst, spMe, 4, 3, 16);
    zShowAfst(giRuitAfst, spMe, 5, 3, 16);
   }
    pol_OpbouwenAfstanden(spOpp);  //For opponent
    pol_OpbouwRuitAfstandenBlok(spOpp);

    //1. At random
    //antw = pol_Eval_random
    //2. Mbv evaluatie
    If (giZetnr > 1) And pol_IsRuitaanval(zi, spMe, zr) Then
      antw := zr
    Else begin
      //Pre-chosen first move if we move first.
      //  antw :=zx //=42; 98=20; 174=V79; 171=V76; 126=V40; 132=V46; 84=V13; 99=Veld 21; 129 = Veld 43.  (219=veld 106)
      zx :=158 ; //158=67; 97=19; 176=81; 114=31; 128=42;
      if (giZetnr = 0) and (giBord[zx] = 0) then
        antw :=zx
      else
        if (giZetnr = 1) and (giSwap[zi] = 1) then begin
          antw := -1;
          i := -1;
          giBord[zi] := spMe;
          giZetnr := giZetnr + 1;
          giZetten[giZetnr, 1] := i;
          end
        else
          antw := pol_Evaluatie(giRuitAfst, spMe);
        End; //Else
    //end if;

    zShowAfst(giRuitAfst, spMe, 7, 2, 16);
    if antw <> -1 then begin
      ok := False;
      For i := 1 To 106 do
        If giVeldnr[i] = antw Then begin
          ok := True;
          Break;
        end; //If
      //Next i;
      If Not ok Then begin
        DebugPrint('**Something is wrong!! Move ' + IntToStr(antw) + ' not found in giVeldnr()?!?**');
        Exit;
      end; //If
    end; //if
    DebugPrint(' My move is: ' + IntToStr(i));
    if i <> -1 then begin
      giBord[giVeldnr[i]] := spMe;
      giZetnr := giZetnr + 1;
      giZetten[giZetnr, 1] := i;
      giZetten[giZetnr, 2] := giVeldnr[i];
    end; //if
    writeln(i);
    //End of move-handling

  Until z < -1;  //Quit
  DebugPrint(' z = ' + IntToStr(z) + ', so we quit!');

End; //Sub

begin
  pol_Main();
  DebugPrint('Click Enter to stop');
  flush(output);
  //readln();
end.

