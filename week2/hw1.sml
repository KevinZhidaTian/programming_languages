fun is_older (date_1: int*int*int, date_2: int*int*int) =
    if #1 date_1 < #1 date_2 then true
    else if #1 date_1 > #1 date_2 then false
    else if #2 date_1 < #2 date_2 then true
    else if ((#3 date_1 < #3 date_2) andalso (#2 date_1 = #2 date_2)) then true
    else false

fun number_in_month (dates: (int*int*int) list, month: int) = 
    if null dates then 0 
    else
        if #2 (hd dates) = month then (1 + number_in_month(tl dates, month))
        else (0 + number_in_month(tl dates, month))

fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months then 0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates: (int*int*int) list, month: int) = 
    if null dates then []
    else 
        if #2 (hd dates) = month
        then (hd dates) :: dates_in_month (tl dates, month)
        else dates_in_month (tl dates, month)

fun dates_in_months (dates: (int*int*int) list, months: int list) = 
    if null months then []
    else
        dates_in_month(dates,hd months) @ dates_in_months(dates, tl months)

fun get_nth (str: string list, n: int) = 
    if n=1 then hd str else get_nth(tl str, n-1)

fun date_to_string (date: (int*int*int)) =
    let
        val monthsDictionary =  ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(monthsDictionary, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum: int, lst: int list) = 
    let
        fun aux(accumulate: int, subList: int list) = 
            if accumulate + hd subList >= sum then 0
            else 1 + aux(accumulate + hd subList, tl subList)
    in 
        aux(0, lst)
    end

fun what_month (day: int) = 
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum (day, days_in_months)
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2 then []
    else
        let
            fun from_to (from: int, to: int) = 
                if from = to then [to] else from::from_to(from+1, to)
        in
            let
                val days = from_to(day1, day2)
                fun get_months (days: int list) = 
                    if null days then [] else what_month(hd days)::get_months(tl days)
            in get_months(days) end
        end


fun oldest (dates: (int*int*int) list) = 
    let
        fun getOldest (oldest: (int*int*int), dates: (int*int*int) list) = 
            if null dates then oldest
            else if is_older(hd dates, oldest) 
                then getOldest(hd dates, tl dates)
                else getOldest(oldest, tl dates)
    in
        if null dates then NONE else SOME (getOldest(hd dates, tl dates))
    end
