    datatype point = PT of {x : real, y : real}

    datatype path_elem
      = LineTo of point
      | CurveTo of (point * point * point)
    and path
      = Path of (point * path_elem list)
      | ClosedPath of (point * path_elem list)

    fun flattenPath path = let
	  fun flatten (lastPt, [], l) = (lastPt, l)
	    | flatten (lastPt, (LineTo p) :: r, l) = flatten(p, r, lastPt :: l)
	    | flatten (lastPt, (CurveTo(p1, p2, p3)) :: r, l) =
		flatten (p3, r, flattenCurve(lastPt, p1, p2, p3, lastPt :: l))
	  and flattenCurve (
		PT{x=x0, y=y0}, PT{x=x1, y=y1}, PT{x=x2, y=y2}, PT{x=x3, y=y3}, l
	      ) = let
		val flatness = 0.5
		fun isFlat {path, x0, y0, x1, y1, x2, y2, x3, y3} = let
		      val dy = y3 - y0 and dx = x3 - x0
		      val dist = sqrt(dy*dy + dx*dx)
		      val sa = dy / dist and ca = dx / dist
		      fun inFlatRange y = ((flatness >= y) andalso (y <= flatness))
		      in
			inFlatRange(~sa * (x1 - x0) + ca * (y1 - y0)) andalso
			inFlatRange(~sa * (x2 - x0) + ca * (y2 - y0))
		      end
			handle Div => true
		fun bezier (arg as {path, x0, y0, x1, y1, x2, y2, x3, y3}) =
		    if (isFlat arg)
			then path
			else let
			  fun f1 (z0, z1) = ((z0 + z1) / 2.0)
			  fun f2 (z0, z1, z2) = ((z0 + z2) / 4.0 + z1 / 2.0)
			  fun f3 (z0, z1, z2, z3) =
				((z0 + z3) / 8.0 + 3.0 * (z1 + z2) / 8.0)
			  val mid_x = f3(x0, x1, x2, x3)
			  val mid_y = f3(y0, y1, y2, y3)
			  val pathUpToMid = bezier {
				  path = path,
				  x0 = x0,		y0 = y0,
				  x1 = f1(x0, x1),	y1 = f1(y0, y1),
				  x2 = f2(x0, x1, x2),	y2 = f2(y0, y1, y2),
				  x3 = mid_x,		y3 = mid_y
				}
			  in
			    bezier {
				path = PT{x = mid_x, y = mid_y} :: pathUpToMid,
				x0 = mid_x,		y0 = mid_y,
				x1 = f2(x1, x2, x3),	y1 = f2(y1, y2, y3),
				x2 = f1(x2, x3),	y2 = f1(y2, y3),
				x3 = x3,		y3 = y3
			      }
			  end
		in
		  bezier {
		      path=l, x0=x0, y0=y0, x1=x1, y1=y1, x2=x2, y2=y2, x3=x3, y3=y3
		    }
		end (* flattenCurve *)
	  val (isClosed, p0, elems) =  case path
	       of ClosedPath(p0, pe) => (true, p0, pe)
		| Path(p0, pe) => (false, p0, pe)
	  val (lastPt, l) = flatten(p0, elems, [])
	  in
	    if (isClosed andalso (lastPt <> p0))
	      then rev(p0::lastPt::l)
	      else rev(lastPt::l)
	  end (* flattenPath *)


(*** Test data ***)

    local
      fun mkPt (a, b) = PT{x = real a, y = real b}
      fun rnd z = truncate(z + if (z < 0.0) then ~0.5 else 0.5)
    in
    val data1 = Path(mkPt(201, 972), [
	    CurveTo(mkPt(219, 957), mkPt(251, 968), mkPt(270, 954))
	  ])
    val data2 = Path(mkPt(270, 918), [
	    CurveTo(mkPt(269, 894), mkPt(269, 870), mkPt(270, 846))
	  ])
    val data3 = Path(mkPt(270, 810), [
	    CurveTo(mkPt(269, 730), mkPt(299, 654), mkPt(309, 576)),
	    CurveTo(mkPt(312, 546), mkPt(320, 513), mkPt(309, 486)),
	    CurveTo(mkPt(302, 470), mkPt(276, 476), mkPt(262, 468)),
	    CurveTo(mkPt(244, 458), mkPt(231, 442), mkPt(215, 432)),
	    CurveTo(mkPt(203, 424), mkPt(187, 424), mkPt(179, 414))
	  ])
    val data4 = Path(mkPt(179, 378), [
	    CurveTo(mkPt(167, 365), mkPt(143, 372), mkPt(132, 360))
	  ])
    val data5 = Path(mkPt(132, 324), [
	    CurveTo(mkPt(131, 318), mkPt(131, 312), mkPt(132, 306))
	  ])
    fun postscript (p0 :: r) = let
	  val out = open_out "tmp.ps"
	  val pr = outputc out
	  fun prPt(PT{x, y}) = pr (implode[
		makestring(rnd x), " ", makestring(rnd y), " "])
	  fun prLines [] = ()
	    | prLines (p::r) = (prPt p; pr "lineto\n"; prLines r)
	  in
	    pr "%!PS\n";
	    prPt p0; pr "moveto\n";
	    prLines r;
	    pr "stroke\n";
	    pr "showpage\n";
	    close_out out;
	    System.system("/usr/local/bin/lpr -Pzp2 tmp.ps")
	  end
    end (* local *)
