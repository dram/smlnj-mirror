(* bug1416.1.sml *)

Date.fmt
  ("%j %U %W %Z")
    (Date.date{
	day=3, hour=10, minute=12, month=Date.Mar, offset=NONE, second=42, year=2017
      });
