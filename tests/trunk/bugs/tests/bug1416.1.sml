(* bug1416.1.sml *)

Date.fmt
  ("%j %U %W %z")
    (Date.date{
	day=3, month=Date.Mar, year=2017,
	hour=10, minute=12, second=42,
        offset=SOME(Time.fromSeconds ~18000)
      });
