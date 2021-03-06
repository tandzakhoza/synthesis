﻿module Synthesis

let abelar num = (num >12 && num%12 = 0 && num<3097)


   // failwith "Not implemented"

let area b h=
    match b < 0.0 || h < 0.0 with
    |true -> failwith "negative base or height"
    |false -> (b*h*0.5)
//failwith "Not implemented"

let zollo num =
    match num < 0 with
    |true -> (num * (-1))
    |false -> num + num//failwith "Not implemented"

let min a b =
    match a < b with
    |true -> a
    |false -> b
    //failwith "Not implemented"

let max a b =
    match a > b with
    |true -> a
    |false -> b
//failwith "Not implemented"

let ofTime hrs mins secs = ((hrs * 3600) + (mins * 60) + secs)
    //failwith "Not implemented"

let toTime secs = 
    match secs <= 0 with
    |true -> (0,0,0)
    |false -> let hrs = secs/3600
              let mins = (secs - (hrs*3600))/60
              let sec = (secs- (mins*60) - (hrs*3600))
              (hrs,mins,sec)
 //failwith "Not implemented"

let digits num =
    let rec counter n acc=
        match n%10 =n with 
        |true -> acc
        |_-> counter (n/10) (acc+1)
    counter num 1

    //failwith "Not implemented"

let minmax (a,b,c,d) = 
    let maximum = max a b |> max c |> max d
    let minimum = min a b |> min c |>min d
    (minimum,maximum)


  //  failwith "Not implemented"

let isLeap year =
    match year < 1582 with
    |true -> failwith "wrong year"
    |false -> match (year % 4 = 0 && year % 100 <> 0) || year % 400 = 0 with
                |true -> true
                |_ -> false

//failwith "Not implemented"

let month input  =
    match input with
    |1 -> ("January", 31)
    |2 -> ("February", 28)
    |3 -> ("March", 31)
    |4 -> ("April", 30)
    |5 -> ("May", 31)
    |6 -> ("June", 30)
    |7 -> ("July", 31)
    |8 -> ("August", 31)
    |9 -> ("September", 30)
    |10 -> ("October", 31)
    |11 -> ("November", 30)
    |12 -> ("December", 31)
    |_ -> failwith "Not a month"

let toBinary integer =
    match integer < 0 with 
    |true -> failwith "Not implemented"
    |_ -> 
    let rec binary num acc =
        match  num with 
        |0 -> "0"
        |1 -> "1"+ acc // basecase
        |_ -> let tostring = string (num%2)
              binary (num/2) (tostring+acc)
    binary integer ""   

let bizFuzz n=        
    let rec count i (a,b,c) =
        match i<=n with 
        |true ->
            match i%3=0, i%5=0, i%15=0 with
            | _, _, true -> count (i+1) (a+1,b+1,c+1)
            | false, true, _ -> count (i+1) (a,b+1,c)
            | true, false, _ -> count (i+1) (a+1,b,c)
            | _ -> count (i+1) (a,b,c)
        |false -> a,b,c
    count 1 (0,0,0)

//failwith "Not implemented"

let monthDay day year =
    let numofdays= 
        match isLeap year with
        |true -> 366
        |_-> 365
    match day>=1 && day<= numofdays with
    |false -> failwith "error"
    |_ ->   let rec count num acc=
            let _, n = month 1
            match month num, day<=acc, isLeap year with
            |("February", 28),false,true -> count (num+1) (acc+29)
            |(a, b), true, _->  a //basecase
            |(a, b), _, _-> count (num+1) (acc+b)
          count 1 n
    

let sqrt n =
    let rec calc guess i=
        match i with
        |10 -> guess
        |_ -> let g = (guess+n/guess)/2.0
              calc g (i+1)
    match n<= 0.0 with
    |true -> failwith "uh-oh"
    |_-> calc (n/2.0) 0

let coord (a,b)=
  (*  let dist (d,c) = let x, y = a-d, c-d
                     sqrt (x*x + y*y)
    let rect (x, y) width height=
        (a >= x) && (a <= x+width) && (b<=y) && (b>=y-height)
    dist, rect
    *)
    failwith "Not implemented"
