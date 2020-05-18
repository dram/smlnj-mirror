signature MD_BUILD =
sig
   structure Ast : MD_AST

   exception ParseError

   val load : string -> Ast.decl list

end

functor MDBuild(AstPP : MD_PP) : MD_BUILD =
struct

   structure Ast = AstPP.Ast
   structure Error = MDError
   structure LrVals = MDParser(structure Token = LrParser.Token
                               structure AstPP = AstPP
                              )
   structure Lex = MDLexFun(LrVals.Tokens)
   structure Parser = JoinWithArg(structure ParserData = LrVals.ParserData
                                  structure Lex = Lex
                                  structure LrParser = LrParser
                                 )
   open PrecedenceParser


   val defaultPrec = 
       foldr (fn ((id,fixity),S) => declare(S,id,fixity)) empty
        [("+",INFIX 5),
         ("-",INFIX 5),
         ("*",INFIX 6),
         ("div",INFIX 6),
         ("mod",INFIX 6),
         ("=",INFIX 3),
         ("==",INFIX 3),
         (">",INFIX 3),
         ("<",INFIX 3),
         ("<=",INFIX 3),
         (">=",INFIX 3),
         ("<>",INFIX 3),
         ("<<",INFIX 4),
         (">>",INFIX 4),
         ("~>>",INFIX 4),
         ("&&",INFIX 5),
         ("^^",INFIX 5),
         ("^",INFIX 5),
         ("||",INFIX 4),
         (":=",INFIX 2),
         ("andalso",INFIX 1),
         ("orelse",INFIX 0),
         ("::",INFIXR 5),
         ("@",INFIXR 5)
        ]

   exception ParseError


   fun parse filename stream =
   let val _     = Lex.UserDeclarations.init ()
       val srcMap = SourceMap.newmap{srcFile=filename}
       fun err(a,b,msg) = 
       let val loc = SourceMap.location srcMap (a,b)
       in  Error.setLoc loc; Error.error(msg) end
       fun input n = TextIO.inputN(stream,n)
       val lexArg = {srcMap=srcMap, err=err}
       val lexer = Parser.Stream.streamify(Lex.makeLexer input lexArg)
       fun parseError(msg,a,b) = err(a,b,msg)
       val (result,lexer) = 
             Parser.parse(15,lexer,parseError,
               (srcMap,Error.errorPos,import,ref defaultPrec))
   in  if !Error.errorCount > 0 then raise ParseError else result end

   and load filename =
   let val stream = TextIO.openIn filename
   in  parse filename stream before TextIO.closeIn stream 
          handle e => (TextIO.closeIn stream; raise e)
   end handle IO.Io{function,name,cause,...} => 
       (
        Error.error(function^" failed in \""^name^"\" ("^exnName cause^")");
        raise ParseError)

   and import (loc,filename) = (Error.setLoc loc; load filename)

end
