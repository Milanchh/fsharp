type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks


type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float


let calculateBudget activity =
    match activity with
    | BoardGame -> "Playing a board game", 0.0
    | Chill -> "Chilling out", 0.0
    | Movie movieType ->
        let description, cost =
            match movieType with
            | Regular -> "Watching a Regular movie", 12.0
            | IMAX -> "Watching an IMAX movie", 17.0
            | DBOX -> "Watching a DBOX movie", 20.0
            | RegularWithSnacks -> "Watching a Regular movie with snacks", 12.0 + 5.0
            | IMAXWithSnacks -> "Watching an IMAX movie with snacks", 17.0 + 5.0
            | DBOXWithSnacks -> "Watching a DBOX movie with snacks", 20.0 + 5.0
        description, cost
    | Restaurant cuisine ->
        let description, cost =
            match cuisine with
            | Korean -> "Going to a Korean restaurant", 70.0
            | Turkish -> "Going to a Turkish restaurant", 65.0
        description, cost
    | LongDrive (kilometres, fuelPerKm) ->
        let cost = float kilometres * fuelPerKm
        sprintf "Taking a long drive of %d km" kilometres, cost


let activities = [
    BoardGame
    Chill
    Movie Regular
    Movie IMAXWithSnacks
    Restaurant Korean
    LongDrive (100, 1.2)
]

let budgets = activities |> List.map calculateBudget


budgets |> List.iter (fun (description, cost) -> printfn "%s costs: %A CAD" description cost)


let totalBudget = budgets |> List.sumBy snd

printfn "The total budget for the Valentine's Day evening is: %A CAD" totalBudget




type Coach = {
    Name: string
    FormerPlayer: bool
}


type Stats = {
    Wins: int
    Losses: int
}


type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}


let coach1 = { Name = "Phil Jackson"; FormerPlayer = true }
let coach2 = { Name = "Pat Riley"; FormerPlayer = true }
let coach3 = { Name = "Doc Rivers"; FormerPlayer = true }
let coach4 = { Name = "George Karl"; FormerPlayer = false }
let coach5 = { Name = "Brad Stevens"; FormerPlayer = false }


let stats1 = { Wins = 62; Losses = 20 }
let stats2 = { Wins = 58; Losses = 24 }
let stats3 = { Wins = 53; Losses = 29 }
let stats4 = { Wins = 48; Losses = 34 }
let stats5 = { Wins = 45; Losses = 37 }


let team1 = { Name = "Chicago Bulls"; Coach = coach1; Stats = stats1 }
let team2 = { Name = "Miami Heat"; Coach = coach2; Stats = stats2 }
let team3 = { Name = "Los Angeles Clippers"; Coach = coach3; Stats = stats3 }
let team4 = { Name = "Milwaukee Bucks"; Coach = coach4; Stats = stats4 }
let team5 = { Name = "Boston Celtics"; Coach = coach5; Stats = stats5 }


let teams = [team1; team2; team3; team4; team5]


let successfulTeams = 
    teams 
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)


let successPercentages = 
    teams 
    |> List.map (fun team -> 
        let wins = float team.Stats.Wins
        let losses = float team.Stats.Losses
        let percentage = (wins / (wins + losses)) * 100.0
        (team.Name, team.Coach.Name, percentage))


printfn "Successful Teams:"
successfulTeams |> List.iter (fun team -> printfn "%s (Coach: %s)" team.Name team.Coach.Name)



printfn "\nSuccess Percentages:"
successPercentages |> List.iter (fun (name, coachName, percentage) -> printfn "%s (Coach: %s): %.2f%%" name coachName percentage)
