(* ______________________________________________________________________
   html4.g
   ______________________________________________________________________ *)

%name HTML4;

%defs(

open HTML4Utils

fun optToList NONE = []
  | optToList (SOME thing) = [thing]

fun optListToList NONE = []
  | optListToList (SOME thing) = thing

);

%tokens        : OPENTAG of Atom.atom * HTML4Utils.tag_payload
               | CLOSETAG of Atom.atom
               | COMMENT of string
               | PCDATA of string
               | DOCTYPE of string
               | CHAR_REF of Atom.atom
               | ENTITY_REF of Atom.atom
               | XML_PROCESSING of string
               (* HTML 4 element tokens. *)
               | STARTA of HTML4Utils.tag_payload
               | ENDA
               | STARTABBR of HTML4Utils.tag_payload
               | ENDABBR
               | STARTACRONYM of HTML4Utils.tag_payload
               | ENDACRONYM
               | STARTADDRESS of HTML4Utils.tag_payload
               | ENDADDRESS
               | STARTAPPLET of HTML4Utils.tag_payload
               | ENDAPPLET
               | STARTAREA of HTML4Utils.tag_payload
               (* No END tag for AREA element. *)
               | STARTB of HTML4Utils.tag_payload
               | ENDB
               | STARTBASE of HTML4Utils.tag_payload
               (* No END tag for BASE element. *)
               | STARTBASEFONT of HTML4Utils.tag_payload
               (* No END tag for BASEFONT element. *)
               | STARTBDO of HTML4Utils.tag_payload
               | ENDBDO
               | STARTBIG of HTML4Utils.tag_payload
               | ENDBIG
               | STARTBLOCKQUOTE of HTML4Utils.tag_payload
               | ENDBLOCKQUOTE
               | STARTBODY of HTML4Utils.tag_payload
               | ENDBODY
               | STARTBR of HTML4Utils.tag_payload
               (* No END tag for BR element. *)
               | STARTBUTTON of HTML4Utils.tag_payload
               | ENDBUTTON
               | STARTCAPTION of HTML4Utils.tag_payload
               | ENDCAPTION
               | STARTCENTER of HTML4Utils.tag_payload
               | ENDCENTER
               | STARTCITE of HTML4Utils.tag_payload
               | ENDCITE
               | STARTCODE of HTML4Utils.tag_payload
               | ENDCODE
               | STARTCOL of HTML4Utils.tag_payload
               (* No END tag for COL element. *)
               | STARTCOLGROUP of HTML4Utils.tag_payload
               | ENDCOLGROUP
               | STARTDD of HTML4Utils.tag_payload
               | ENDDD
               | STARTDEL of HTML4Utils.tag_payload
               | ENDDEL
               | STARTDFN of HTML4Utils.tag_payload
               | ENDDFN
               | STARTDIR of HTML4Utils.tag_payload
               | ENDDIR
               | STARTDIV of HTML4Utils.tag_payload
               | ENDDIV
               | STARTDL of HTML4Utils.tag_payload
               | ENDDL
               | STARTDT of HTML4Utils.tag_payload
               | ENDDT
               | STARTEM of HTML4Utils.tag_payload
               | ENDEM
               | STARTFIELDSET of HTML4Utils.tag_payload
               | ENDFIELDSET
               | STARTFONT of HTML4Utils.tag_payload
               | ENDFONT
               | STARTFORM of HTML4Utils.tag_payload
               | ENDFORM
               | STARTFRAME of HTML4Utils.tag_payload
               (* No END tag for FRAME element. *)
               | STARTFRAMESET of HTML4Utils.tag_payload
               | ENDFRAMESET
               | STARTH1 of HTML4Utils.tag_payload
               | ENDH1
               | STARTH2 of HTML4Utils.tag_payload
               | ENDH2
               | STARTH3 of HTML4Utils.tag_payload
               | ENDH3
               | STARTH4 of HTML4Utils.tag_payload
               | ENDH4
               | STARTH5 of HTML4Utils.tag_payload
               | ENDH5
               | STARTH6 of HTML4Utils.tag_payload
               | ENDH6
               | STARTHEAD of HTML4Utils.tag_payload
               | ENDHEAD
               | STARTHR of HTML4Utils.tag_payload
               (* No END tag for HR element. *)
               | STARTHTML of HTML4Utils.tag_payload
               | ENDHTML
               | STARTI of HTML4Utils.tag_payload
               | ENDI
               | STARTIFRAME of HTML4Utils.tag_payload
               | ENDIFRAME
               | STARTIMG of HTML4Utils.tag_payload
               (* No END tag for IMG element. *)
               | STARTINPUT of HTML4Utils.tag_payload
               (* No END tag for INPUT element. *)
               | STARTINS of HTML4Utils.tag_payload
               | ENDINS
               | STARTISINDEX of HTML4Utils.tag_payload
               (* No END tag for ISINDEX element. *)
               | STARTKBD of HTML4Utils.tag_payload
               | ENDKBD
               | STARTLABEL of HTML4Utils.tag_payload
               | ENDLABEL
               | STARTLEGEND of HTML4Utils.tag_payload
               | ENDLEGEND
               | STARTLI of HTML4Utils.tag_payload
               | ENDLI
               | STARTLINK of HTML4Utils.tag_payload
               (* No END tag for LINK element. *)
               | STARTMAP of HTML4Utils.tag_payload
               | ENDMAP
               | STARTMENU of HTML4Utils.tag_payload
               | ENDMENU
               | STARTMETA of HTML4Utils.tag_payload
               (* No END tag for META element. *)
               | STARTNOFRAMES of HTML4Utils.tag_payload
               | ENDNOFRAMES
               | STARTNOSCRIPT of HTML4Utils.tag_payload
               | ENDNOSCRIPT
               | STARTOBJECT of HTML4Utils.tag_payload
               | ENDOBJECT
               | STARTOL of HTML4Utils.tag_payload
               | ENDOL
               | STARTOPTGROUP of HTML4Utils.tag_payload
               | ENDOPTGROUP
               | STARTOPTION of HTML4Utils.tag_payload
               | ENDOPTION
               | STARTP of HTML4Utils.tag_payload
               | ENDP
               | STARTPARAM of HTML4Utils.tag_payload
               (* No END tag for PARAM element. *)
               | STARTPRE of HTML4Utils.tag_payload
               | ENDPRE
               | STARTQ of HTML4Utils.tag_payload
               | ENDQ
               | STARTS of HTML4Utils.tag_payload
               | ENDS
               | STARTSAMP of HTML4Utils.tag_payload
               | ENDSAMP
               | STARTSCRIPT of HTML4Utils.tag_payload
               | ENDSCRIPT
               | STARTSELECT of HTML4Utils.tag_payload
               | ENDSELECT
               | STARTSMALL of HTML4Utils.tag_payload
               | ENDSMALL
               | STARTSPAN of HTML4Utils.tag_payload
               | ENDSPAN
               | STARTSTRIKE of HTML4Utils.tag_payload
               | ENDSTRIKE
               | STARTSTRONG of HTML4Utils.tag_payload
               | ENDSTRONG
               | STARTSTYLE of HTML4Utils.tag_payload
               | ENDSTYLE
               | STARTSUB of HTML4Utils.tag_payload
               | ENDSUB
               | STARTSUP of HTML4Utils.tag_payload
               | ENDSUP
               | STARTTABLE of HTML4Utils.tag_payload
               | ENDTABLE
               | STARTTBODY of HTML4Utils.tag_payload
               | ENDTBODY
               | STARTTD of HTML4Utils.tag_payload
               | ENDTD
               | STARTTEXTAREA of HTML4Utils.tag_payload
               | ENDTEXTAREA
               | STARTTFOOT of HTML4Utils.tag_payload
               | ENDTFOOT
               | STARTTH of HTML4Utils.tag_payload
               | ENDTH
               | STARTTHEAD of HTML4Utils.tag_payload
               | ENDTHEAD
               | STARTTITLE of HTML4Utils.tag_payload
               | ENDTITLE
               | STARTTR of HTML4Utils.tag_payload
               | ENDTR
               | STARTTT of HTML4Utils.tag_payload
               | ENDTT
               | STARTU of HTML4Utils.tag_payload
               | ENDU
               | STARTUL of HTML4Utils.tag_payload
               | ENDUL
               | STARTVAR of HTML4Utils.tag_payload
               | ENDVAR
;

%start document;

(*
%entry document, body, flow, block, inline;
*)

document : cdata_opt
           (DOCTYPE cdata_opt => ((Lf (Tok.DOCTYPE DOCTYPE)) :: cdata_opt))?
           (STARTHTML cdata_opt
            => ((Lf (Tok.STARTHTML STARTHTML)) :: cdata_opt))?
           head
           (body | frameset)
           (ENDHTML cdata_opt => ((Lf (Tok.ENDHTML)) :: cdata_opt))?
           => (Nd (cdata_opt @ (optListToList SR1) @ (optListToList SR2) @
                   (head :: SR3 :: (optListToList SR4))))
;

(* ______________________________________________________________________
   HEAD and related elements
   ______________________________________________________________________ *)

head : (STARTHEAD cdata_opt => ((Lf (Tok.STARTHEAD STARTHEAD)) :: cdata_opt))?
       (head_content cdata_opt => (head_content :: cdata_opt))*
       (ENDHEAD cdata_opt => ((Lf (Tok.ENDHEAD)) :: cdata_opt))?
       => (Nd ((optListToList SR1) @ (foldr op@ [] SR2) @ (optListToList SR3)))
;

head_content : title | base | script | style | meta | link | object
;

title : STARTTITLE cdata_opt ENDTITLE
        => (Nd ((Lf (Tok.STARTTITLE STARTTITLE)) ::
                (cdata_opt @ [Lf (Tok.ENDTITLE)])))
;

base : STARTBASE
       => (Lf (Tok.STARTBASE STARTBASE))
;

script : STARTSCRIPT cdata_opt ENDSCRIPT
         => (Nd ((Lf (Tok.STARTSCRIPT STARTSCRIPT)) ::
                 (cdata_opt @ [Lf (Tok.ENDSCRIPT)])))
;

style : STARTSTYLE cdata_opt ENDSTYLE
         => (Nd ((Lf (Tok.STARTSTYLE STARTSTYLE)) ::
                 (cdata_opt @ [Lf (Tok.ENDSTYLE)])))
;

meta : STARTMETA
       => (Lf (Tok.STARTMETA STARTMETA))
;

link : STARTLINK
       => (Lf (Tok.STARTLINK STARTLINK))
;

object : STARTOBJECT (param | flow)* ENDOBJECT
         => (Nd ((Lf (Tok.STARTOBJECT STARTOBJECT)) ::
                 (SR @ [Lf (Tok.ENDOBJECT)])))
;

param : STARTPARAM
       => (Lf (Tok.STARTPARAM STARTPARAM))
;

(* ______________________________________________________________________
   BODY and related elements
   ______________________________________________________________________ *)

body : STARTBODY body_rest
       => (Nd ((Lf (Tok.STARTBODY STARTBODY)) :: body_rest))
     | (block | ins | del) body_rest
       => (Nd (SR :: body_rest))
;

body_rest : (block | script | ins | del | cdata)*
            (ENDBODY cdata_opt => ((Lf (Tok.ENDBODY)) :: cdata_opt))?
            => (SR1 @ (optListToList SR2))
;

flow : block
     | inline
;

block : p 
      | heading
      | list
      | preformatted
      | dl
      | div
      | noscript
      | blockquote
      | form
      | hr
      | table
      | fieldset
      | address
      | block_loose
;

block_loose : center
            | isindex
;

heading : h1
        | h2
        | h3
        | h4
        | h5
        | h6
;

list : ul
     | ol
     | list_loose
;

list_loose : dir
           | menu
;

preformatted : pre
;

inline : fontstyle
       | phrase
       | special
       | formctrl
       | cdata
;

fontstyle : tt
          | i
          | b
          | big
          | small
          | fontstyle_loose
;

fontstyle_loose : u
                | s
                | strike
;

phrase : em
       | strong
       | dfn
       | code
       | samp
       | kbd
       | var
       | cite
       | abbr
       | acronym
;

special : a 
        | img
        | object
        | br
        | script
        | map
        | q
        | sub
        | sup
        | span
        | bdo
        | special_loose
;

special_loose : applet 
              | basefont
              | font
              | iframe
;

formctrl : input
         | select
         | textarea
         | label
         | button
;

(* Actual elements *)

a : STARTA inline* ENDA
         => (Nd ((Lf (Tok.STARTA STARTA)) :: (inline @ [Lf (Tok.ENDA)])))
;

abbr : STARTABBR inline* ENDABBR
         => (Nd ((Lf (Tok.STARTABBR STARTABBR)) ::
                 (inline @ [Lf (Tok.ENDABBR)])))
;

acronym : STARTACRONYM inline* ENDACRONYM
         => (Nd ((Lf (Tok.STARTACRONYM STARTACRONYM)) ::
                 (inline @ [Lf (Tok.ENDACRONYM)])))
;

address : STARTADDRESS inline* ENDADDRESS
         => (Nd ((Lf (Tok.STARTADDRESS STARTADDRESS)) ::
                 (inline @ [Lf (Tok.ENDADDRESS)])))
;

applet : STARTAPPLET (param | flow)* ENDAPPLET
         => (Nd ((Lf (Tok.STARTAPPLET STARTAPPLET)) ::
                 (SR @ [Lf (Tok.ENDAPPLET)])))
;

area : STARTAREA
       => (Lf (Tok.STARTAREA STARTAREA))
;

b : STARTB inline* ENDB
         => (Nd ((Lf (Tok.STARTB STARTB)) :: (inline @ [Lf (Tok.ENDB)])))
;

basefont : STARTBASEFONT
       => (Lf (Tok.STARTBASEFONT STARTBASEFONT))
;

bdo : STARTBDO inline* ENDBDO
         => (Nd ((Lf (Tok.STARTBDO STARTBDO)) ::
                 (inline @ [Lf (Tok.ENDBDO)])))
;

big : STARTBIG inline* ENDBIG
         => (Nd ((Lf (Tok.STARTBIG STARTBIG)) ::
                 (inline @ [Lf (Tok.ENDBIG)])))
;

blockquote : STARTBLOCKQUOTE (block | script | cdata)+ ENDBLOCKQUOTE
         => (Nd ((Lf (Tok.STARTBLOCKQUOTE STARTBLOCKQUOTE)) ::
                 (SR @ [Lf (Tok.ENDBLOCKQUOTE)])))
;

br : STARTBR
       => (Lf (Tok.STARTBR STARTBR))
;

button : STARTBUTTON flow* ENDBUTTON
         => (Nd ((Lf (Tok.STARTBUTTON STARTBUTTON)) ::
                 (flow @ [Lf (Tok.ENDBUTTON)])))
;

caption : STARTCAPTION inline* ENDCAPTION
          => (Nd ((Lf (Tok.STARTCAPTION STARTCAPTION)) ::
                  (inline @ [Lf (Tok.ENDCAPTION)])))
;

center : STARTCENTER flow* ENDCENTER
         => (Nd ((Lf (Tok.STARTCENTER STARTCENTER)) ::
                 (flow @ [Lf (Tok.ENDCENTER)])))
;

cite : STARTCITE inline* ENDCITE
       => (Nd ((Lf (Tok.STARTCITE STARTCITE)) ::
               (inline @ [Lf (Tok.ENDCITE)])))
;

code : STARTCODE inline* ENDCODE
       => (Nd ((Lf (Tok.STARTCODE STARTCODE)) ::
               (inline @ [Lf (Tok.ENDCODE)])))
;

col : STARTCOL
      => (Lf (Tok.STARTCOL STARTCOL))
;

colgroup : STARTCOLGROUP cdata_opt
           (col cdata_opt => (col :: cdata_opt))*
           (ENDCOLGROUP => (Lf (Tok.ENDCOLGROUP)))?
           => (Nd ((Lf (Tok.STARTCOLGROUP STARTCOLGROUP)) ::
                   (cdata_opt @ (foldr op@ [] SR1) @ (optToList SR2))))
;

dd : STARTDD flow* (ENDDD => (Lf (Tok.ENDDD)))?
     => (Nd ((Lf (Tok.STARTDD STARTDD)) :: (flow @ (optToList SR))))
;

del : STARTDEL flow* ENDDEL
      => (Nd ((Lf (Tok.STARTDEL STARTDEL)) ::
              (flow @ [Lf (Tok.ENDDEL)])))
;

dfn : STARTDFN inline* ENDDFN
      => (Nd ((Lf (Tok.STARTDFN STARTDFN)) ::
              (inline @ [Lf (Tok.ENDDFN)])))
;

dir : STARTDIR cdata_opt li+ ENDDIR
      => (Nd ((Lf (Tok.STARTDIR STARTDIR)) ::
              (cdata_opt @ li @ [Lf (Tok.ENDDIR)])))
;

div : STARTDIV flow* ENDDIV
      => (Nd ((Lf (Tok.STARTDIV STARTDIV)) ::
              (flow @ [Lf (Tok.ENDDIV)])))
;

dl : STARTDL cdata_opt (dt | dd)+ ENDDL
     => (Nd ((Lf (Tok.STARTDL STARTDL)) ::
             (cdata_opt @ SR @ [Lf (Tok.ENDDL)])))
;

dt : STARTDT inline* (ENDDT => (Lf (Tok.ENDDT)))?
     => (Nd ((Lf (Tok.STARTDT STARTDT)) :: (inline @ (optToList SR))))
;

em : STARTEM inline* ENDEM
     => (Nd ((Lf (Tok.STARTEM STARTEM)) :: (inline @ [Lf (Tok.ENDEM)])))
;

fieldset : STARTFIELDSET cdata_opt legend flow* ENDFIELDSET
           => (Nd ((Lf (Tok.STARTFIELDSET STARTFIELDSET)) ::
                   (cdata_opt @ [legend] @ flow @
                    [Lf (Tok.ENDFIELDSET)])))
;

font : STARTFONT inline* ENDFONT
       => (Nd ((Lf (Tok.STARTFONT STARTFONT)) ::
               (inline @ [Lf (Tok.ENDFONT)])))
;

form : STARTFORM (cdata | block | script)+ ENDFORM
       => (Nd ((Lf (Tok.STARTFORM STARTFORM)) ::
               (SR @ [Lf (Tok.ENDFORM)])))
;

frame : STARTFRAME
        => (Lf (Tok.STARTFRAME STARTFRAME))
;

frameset : STARTFRAMESET (frameset | frame | noframes | cdata)+ ENDFRAMESET
           => (Nd ((Lf (Tok.STARTFRAMESET STARTFRAMESET)) ::
                   (SR @ [Lf (Tok.ENDFRAMESET)])))
;

h1 : STARTH1 inline* ENDH1
     => (Nd ((Lf (Tok.STARTH1 STARTH1)) :: (inline @ [Lf (Tok.ENDH1)])))
;

h2 : STARTH2 inline* ENDH2
     => (Nd ((Lf (Tok.STARTH2 STARTH2)) :: (inline @ [Lf (Tok.ENDH2)])))
;

h3 : STARTH3 inline* ENDH3
     => (Nd ((Lf (Tok.STARTH3 STARTH3)) :: (inline @ [Lf (Tok.ENDH3)])))
;

h4 : STARTH4 inline* ENDH4
     => (Nd ((Lf (Tok.STARTH4 STARTH4)) :: (inline @ [Lf (Tok.ENDH4)])))
;

h5 : STARTH5 inline* ENDH5
     => (Nd ((Lf (Tok.STARTH5 STARTH5)) :: (inline @ [Lf (Tok.ENDH5)])))
;

h6 : STARTH6 inline* ENDH6
     => (Nd ((Lf (Tok.STARTH6 STARTH6)) :: (inline @ [Lf (Tok.ENDH6)])))
;

hr : STARTHR
     => (Lf (Tok.STARTHR STARTHR))
;

i : STARTI inline* ENDI
         => (Nd ((Lf (Tok.STARTI STARTI)) ::
                 (inline @ [Lf (Tok.ENDI)])))
;

iframe : STARTIFRAME flow* ENDIFRAME
         => (Nd ((Lf (Tok.STARTIFRAME STARTIFRAME)) ::
                 (flow @ [Lf (Tok.ENDIFRAME)])))
;

img : STARTIMG
       => (Lf (Tok.STARTIMG STARTIMG))
;

input : STARTINPUT
       => (Lf (Tok.STARTINPUT STARTINPUT))
;

ins : STARTINS flow* ENDINS
         => (Nd ((Lf (Tok.STARTINS STARTINS)) ::
                 (flow @ [Lf (Tok.ENDINS)])))
;

isindex : STARTISINDEX
       => (Lf (Tok.STARTISINDEX STARTISINDEX))
;

kbd : STARTKBD inline* ENDKBD
         => (Nd ((Lf (Tok.STARTKBD STARTKBD)) ::
                 (inline @ [Lf (Tok.ENDKBD)])))
;

label : STARTLABEL inline* ENDLABEL
         => (Nd ((Lf (Tok.STARTLABEL STARTLABEL)) ::
                 (inline @ [Lf (Tok.ENDLABEL)])))
;

legend : STARTLEGEND inline* ENDLEGEND
         => (Nd ((Lf (Tok.STARTLEGEND STARTLEGEND)) ::
                 (inline @ [Lf (Tok.ENDLEGEND)])))
;

li : STARTLI flow* (ENDLI => (Lf (Tok.ENDLI)))?
     => (Nd ((Lf (Tok.STARTLI STARTLI)) :: (flow @ (optToList SR))))
;

map : STARTMAP (cdata | block | area)+ ENDMAP
      => (Nd ((Lf (Tok.STARTMAP STARTMAP)) :: (SR @ [Lf (Tok.ENDMAP)])))
;

menu : STARTMENU cdata_opt li+ ENDMENU
       => (Nd ((Lf (Tok.STARTMENU STARTMENU)) ::
               (cdata_opt @ li @ [Lf (Tok.ENDMENU)])))
;

noframes : STARTNOFRAMES body ENDNOFRAMES
           => (Nd [Lf (Tok.STARTNOFRAMES STARTNOFRAMES), body,
                   Lf (Tok.ENDNOFRAMES)])
;

noscript : STARTNOSCRIPT (cdata | block)+ ENDNOSCRIPT
           => (Nd ((Lf (Tok.STARTNOSCRIPT STARTNOSCRIPT)) ::
                   (SR @ [Lf (Tok.ENDNOSCRIPT)])))
;

ol : STARTOL cdata_opt li+ ENDOL
     => (Nd ((Lf (Tok.STARTOL STARTOL)) ::
             (cdata_opt @ li @ [Lf (Tok.ENDOL)])))
;

optgroup : STARTOPTGROUP cdata_opt option+ ENDOPTGROUP cdata_opt
           => (Nd ((Lf (Tok.STARTOPTGROUP STARTOPTGROUP)) ::
                   (cdata_opt1 @ option @ ((Lf (Tok.ENDOPTGROUP))
                                           :: cdata_opt2))))
;

option : STARTOPTION cdata_opt
         (ENDOPTION cdata_opt => ((Lf (Tok.ENDOPTION)) ::
                                  cdata_opt))?
         => (Nd ((Lf (Tok.STARTOPTION STARTOPTION)) ::
                 (cdata_opt @ (optListToList SR))))
;

(* TODO: Making the ENDP optional, which is valid, causes
left-recursion for the inline* part.  This can be fixed by having a
two state flow nonterminal, which the older HTML library does. *)

p : STARTP inline* ENDP
    => (Nd ((Lf (Tok.STARTP STARTP)) :: (inline @ [Lf (Tok.ENDP)])))
;

pre : STARTPRE inline* ENDPRE
      => (Nd ((Lf (Tok.STARTPRE STARTPRE)) ::
              (inline @ [Lf (Tok.ENDPRE)])))
;

q : STARTQ inline* ENDQ
    => (Nd ((Lf (Tok.STARTQ STARTQ)) :: (inline @ [Lf (Tok.ENDQ)])))
;

s : STARTS inline* ENDS
    => (Nd ((Lf (Tok.STARTS STARTS)) :: (inline @ [Lf (Tok.ENDS)])))
;

samp : STARTSAMP inline* ENDSAMP
       => (Nd ((Lf (Tok.STARTSAMP STARTSAMP)) ::
               (inline @ [Lf (Tok.ENDSAMP)])))
;

select : STARTSELECT cdata_opt (optgroup | option)+ ENDSELECT
         => (Nd ((Lf (Tok.STARTSELECT STARTSELECT)) ::
                 (cdata_opt @ SR @ [Lf (Tok.ENDSELECT)])))
;

small : STARTSMALL inline* ENDSMALL
        => (Nd ((Lf (Tok.STARTSMALL STARTSMALL)) ::
                (inline @ [Lf (Tok.ENDSMALL)])))
;

span : STARTSPAN inline* ENDSPAN
       => (Nd ((Lf (Tok.STARTSPAN STARTSPAN)) ::
               (inline @ [Lf (Tok.ENDSPAN)])))
;

strike : STARTSTRIKE inline* ENDSTRIKE
         => (Nd ((Lf (Tok.STARTSTRIKE STARTSTRIKE)) ::
                 (inline @ [Lf (Tok.ENDSTRIKE)])))
;

strong : STARTSTRONG inline* ENDSTRONG
         => (Nd ((Lf (Tok.STARTSTRONG STARTSTRONG)) ::
                 (inline @ [Lf (Tok.ENDSTRONG)])))
;

sub : STARTSUB inline* ENDSUB
      => (Nd ((Lf (Tok.STARTSUB STARTSUB)) ::
              (inline @ [Lf (Tok.ENDSUB)])))
;

sup : STARTSUP inline* ENDSUP
      => (Nd ((Lf (Tok.STARTSUP STARTSUP)) ::
              (inline @ [Lf (Tok.ENDSUP)])))
;

(* My reading of the HTML DTD indicates the following order of
elements is enforceable: *)

table : STARTTABLE cdata_opt
        (caption cdata_opt => (caption :: cdata_opt))?
        col_or_colgroups table_content ENDTABLE
        => (Nd ((Lf (Tok.STARTTABLE STARTTABLE)) ::
                (cdata_opt @ (optListToList SR) @ col_or_colgroups @
                 table_content @ [Lf (Tok.ENDTABLE)])))
;

(* The whole tr+ thing makes the original table production ambiguous:
   STARTTABLE ... thead? tfoot? tbody+ ENDTABLE *)

table_content
        : thead tfoot? tbodies
          => (thead :: ((optToList tfoot)) @ tbodies)
        | tfoot tbodies
          => (tfoot :: tbodies)
        | tbodies_nostart
;

col_or_colgroups : (* empty *)
                   => ([])
                 | (col cdata_opt => (col :: cdata_opt))+
                   => (foldr op@ [] SR)
                 | colgroup+
;


tbodies_nostart : (STARTTBODY cdata_opt =>
                   ((Lf (Tok.STARTTBODY STARTTBODY)) :: cdata_opt))?
                  tr+ tbodies_rest?
                  => (let val (tbody_rest, tbody_peers) =
                              case tbodies_rest of
                                  NONE => ([], [])
                                | SOME tbodies_tup => tbodies_tup
                      in (Nd ((optListToList SR) @ tr @ tbody_rest)) ::
                         tbody_peers end)
;

tbodies : STARTTBODY cdata_opt tr+ tbodies_rest
          => (let val (tbody_rest, tbody_peers) = tbodies_rest
              in (Nd ((Lf (Tok.STARTTBODY STARTTBODY)) ::
                      (cdata_opt @ tr @ tbody_rest))) ::
                 tbody_peers end)
;

tbodies_rest : ENDTBODY cdata_opt tbodies?
               => ((Lf (Tok.ENDTBODY)) :: cdata_opt,
                   optListToList tbodies)
             | STARTTBODY cdata_opt tr+ tbodies_rest?
               => (let val (tbody_rest, tbody_peers) =
                           case tbodies_rest of NONE => ([], [])
                                              | SOME tbodies_tup => tbodies_tup
                   in ([], (Nd ((Lf (Tok.STARTTBODY STARTTBODY)) ::
                                (cdata_opt @ tr @ tbody_rest))) :: tbody_peers)
                   end)
;

td : STARTTD flow* (ENDTD cdata_opt => ((Lf (Tok.ENDTD)) :: cdata_opt))?
     => (Nd ((Lf (Tok.STARTTD STARTTD)) :: (flow @ (optListToList SR))))
;

textarea : STARTTEXTAREA cdata_opt ENDTEXTAREA
           => (Nd ((Lf (Tok.STARTTEXTAREA STARTTEXTAREA)) ::
                   (cdata_opt @ [Lf (Tok.ENDTEXTAREA)])))
;

tfoot : STARTTFOOT cdata_opt tr+
        (ENDTFOOT cdata_opt => ((Lf (Tok.ENDTFOOT)) :: cdata_opt))?
        => (Nd ((Lf (Tok.STARTTFOOT STARTTFOOT)) :: (cdata_opt @ tr @
                                                     (optListToList SR))))
;

th : STARTTH flow* (ENDTH cdata_opt => ((Lf (Tok.ENDTH)) :: cdata_opt))?
     => (Nd ((Lf (Tok.STARTTH STARTTH)) :: (flow @ (optListToList SR))))
;

thead : STARTTHEAD cdata_opt tr+
        (ENDTHEAD cdata_opt => ((Lf (Tok.ENDTHEAD)) :: cdata_opt))?
        => (Nd ((Lf (Tok.STARTTHEAD STARTTHEAD)) :: (cdata_opt @ tr @
                                                     (optListToList SR))))
;

tr : STARTTR cdata_opt (th | td)+
     (ENDTR cdata_opt => ((Lf (Tok.ENDTR)) :: cdata_opt))?
     => (Nd ((Lf (Tok.STARTTR STARTTR)) :: (cdata_opt @ SR1 @
                                            (optListToList SR2))))
;

tt : STARTTT inline* ENDTT
     => (Nd ((Lf (Tok.STARTTT STARTTT)) :: (inline @ [Lf (Tok.ENDTT)])))
;

u : STARTU inline* ENDU
    => (Nd ((Lf (Tok.STARTU STARTU)) :: (inline @ [Lf (Tok.ENDU)])))
;

ul : STARTUL cdata_opt li+ ENDUL
     => (Nd (((Lf (Tok.STARTUL STARTUL)) :: (cdata_opt @ li @
                                             [Lf (Tok.ENDUL)]))))
;

var : STARTVAR inline* ENDVAR
      => (Nd ((Lf (Tok.STARTVAR STARTVAR)) :: (inline @
                                               [Lf (Tok.ENDVAR)])))
;

(* ______________________________________________________________________
   Miscellaneous data nonterminals
   ______________________________________________________________________ *)

cdata : (PCDATA => (Tok.PCDATA PCDATA) 
        | CHAR_REF => (Tok.CHAR_REF CHAR_REF)
        | ENTITY_REF => (Tok.ENTITY_REF ENTITY_REF)
        | COMMENT => (Tok.COMMENT COMMENT))
        => ((Lf SR) : HTML4Tokens.token tree)
;

cdata_opt : cdata* => (cdata : HTML4Tokens.token tree list)
;

(* ______________________________________________________________________
   End of html4.g
   ______________________________________________________________________ *)
