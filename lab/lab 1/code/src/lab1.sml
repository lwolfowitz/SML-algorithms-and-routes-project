(*  COMP 212 Lab 1:  ML Basics.
*   N. Danner
*)

structure Lab1 =
struct

  (*  isLeapYear (x) = true if x is a leap year, false otherwise.
  *)
  fun isLeapYear(x : int) : bool =
 if ((x mod 4 = 0 andalso x mod 100 <> 0) orelse ( x mod 100 = 0 andalso x mod 400 = 0)) then true else false;  

  (*  validMonth m = true if m is a valid month name, false otherwise.
  *)
  fun validMonth (m : string) : bool = if m = "January" orelse m = "February" orelse m = "March" orelse m = "April" orelse m = "May" orelse m = "June" orelse m = "July" orelse m = "August" orelse m = "September" orelse m = "October" orelse m = "November" orelse m = "December" then true else false;


  (*  daysPerMonth (m, y) = the number of days in the month m, provided
  *   validMonth m = true.  February always has 28 days if y is not a leap
  *   year, 29 days if y is a leap year.
  *)
  fun daysPerMonth (m : string, y : int) : int =
    case m of 
    	   "January" => 31 
	 | "February" => if isLeapYear(y) then (29 : int) else (28 : int)
	 | "March" => 31
	 | "April" => 30
	 | "May" => 31
	 | "June" => 30
	 | "July" => 31
	 | "August" => 31
	 | "September" => 30
	 | "October" => 31
	 | "November" => 30
	 | "December" => 31
	 | _ => 0;

  (*  validDate (d, m, y) = true if d, m, and y form a valid date; that is,
  *   m is a valid month name and 1 <= d <= (the number of days in m)
  *   and 0 <= y.
  *)
  fun validDate (d : int, m : string, y : int) : bool = if ((validMonth(m)) = true andalso (1 <= d andalso d  <= daysPerMonth(m, y)) andalso (0 <= y)) then true else false;
    

end
