// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello! I'm Bishal Amgai"

// List of wage
let wage = [75000M; 48000M; 120000M; 190000M; 300113M; 92000M; 36000M]

// (i) Filter highest income wage by using 'list.filter' function
let highIncomewage = wage |> List.filter (fun wage -> wage > 100000M)

// (ii) Calculate tax for all wage
let calculateTax wage =
    match wage with
    | x when x <= 49020M -> 0.15M * x
    | x when x <= 98040M -> 0.205M * x
    | x when x <= 151978M -> 0.26M * x
    | x when x <= 216511M -> 0.29M * x
    | _ -> 0.33M * wage

let taxes = wage |> List.map calculateTax

// (iii) Add $20,000 to wage less than $49,020
let adjustwage = wage |> List.map (fun wage -> if wage < 49020M then wage + 20000M else wage)

// (iv) Sum wage between $50,000 and $100,000 
let sumwage = 
    wage 
    |> List.filter (fun wage -> wage >= 50000M && wage <= 100000M) 
    |> List.reduce (+)

// Output results
printfn "Highest Income wage: %A" highIncomewage
printfn "Taxes for the income: %A" taxes
printfn "Adjusted wage: %A" adjustwage
printfn "Sum of wage between $50,000 and $100,000: %M" sumwage

//Performing Tail Recursion
let rec sumMultiplesOfThreeAccumulator giveNumber accumulator =
    if giveNumber = 0 then
        accumulator
    else
        sumMultiplesOfThreeAccumulator (giveNumber - 3) (accumulator + giveNumber)

let sumMultiplesOfThree giveNumber =
    sumMultiplesOfThreeAccumulator giveNumber 0

// Example usage:
let result = sumMultiplesOfThree 27
printfn "Sum of all multiples of 3 up to a given number is: %d" result
