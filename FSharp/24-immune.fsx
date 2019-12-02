open System.IO
open System.Text.RegularExpressions
open System

type Team = ImmuneSystem | Infection

type Group = {
    Team : Team
    UnitCount : int
    HitPoints : int
    AttackType : string
    Damage : int
    Initiative : int
    Immunities : string array
    Weaknesses : string array
}

let mutable immuneBoost = 0

let effectivePower group = group.UnitCount * (group.Damage + (if group.Team = ImmuneSystem then immuneBoost else 0))

let lines = File.ReadAllLines("FSharp/24-immune-input-small.txt")

// 1928 units each with 10700 hit points (weak to cold; immune to fire, radiation, slashing) with an attack that does 50 slashing damage at initiative 3
let groupRegex = Regex("(\d*) units each with (\d*) hit points(?: \((.*)\))? with an attack that does (\d*) (.*) damage at initiative (\d*)", RegexOptions.Compiled)

let parseGroup team line  =
    let rm = groupRegex.Match line
    let group (i : int) = (rm.Groups.[i].Value) |> Int32.Parse

    let immunities = rm.Groups.[3].Value.Split(';')
                     |> Array.filter (fun s -> s.Contains("immune to "))
                     |> Array.map (fun s -> s.Replace("immune to ", ""))
                     |> Array.collect (fun s -> s.Split(','))
                     |> Array.map (fun s -> s.Trim())

    let weaknesses = rm.Groups.[3].Value.Split(';')
                     |> Array.filter (fun s -> s.Contains("weak to "))
                     |> Array.map (fun s -> s.Replace("weak to ", ""))
                     |> Array.collect (fun s -> s.Split(','))
                     |> Array.map (fun s -> s.Trim())

    {
        Team = team
        UnitCount = group 1
        HitPoints = group 2
        Damage = group 4
        AttackType = rm.Groups.[5].Value
        Initiative = group 6
        Immunities = immunities
        Weaknesses = weaknesses
    }

let readImmuneGroups () = lines
                          |> Array.skip 1
                          |> Array.takeWhile (String.IsNullOrWhiteSpace >> not)
                          |> Array.map (parseGroup ImmuneSystem)

let readInfectionGroups () = lines
                             |> Array.skipWhile (fun l -> not (l.StartsWith("Infection:")))
                             |> Array.skip 1
                             |> Array.map (parseGroup Infection)

type AttackDirection = ImmuneToInfection | InfectionToImmune

type Attack = {
    Direction : AttackDirection
    AttackerIndex : int
    DefenderIndex : int
}

let damage attacker defender =
    let multiplier = if defender.Weaknesses |> Array.contains attacker.AttackType
                     then 2
                     elif defender.Immunities |> Array.contains attacker.AttackType
                     then 0
                     else 1
    (effectivePower attacker) * multiplier

let selectTargets immuneGroups infectionGroups =
    let allGroups = Array.append immuneGroups infectionGroups
                    |> Array.sortByDescending (fun g -> (effectivePower g), g.Initiative)
                    |> List.ofArray

    let rec selectTarget groupsToSelect immuneGroups infectionGroups attacks =
        match groupsToSelect with
        | [] -> attacks
        | attacker :: t -> let enemyGroup = match attacker.Team with
                                            | ImmuneSystem -> infectionGroups
                                            | Infection -> immuneGroups
                           let attackDirection = if enemyGroup = immuneGroups then InfectionToImmune else ImmuneToInfection
                           // Find the first enemy group which has not been picked yet.
                           let pick = enemyGroup
                                      |> Array.mapi (fun i d -> (i, d))
                                      |> Array.filter (fun (i, d) -> d.UnitCount > 0 &&
                                                                     (d |> damage attacker) > 0 &&
                                                                     (attacks |> List.filter (fun a -> a.Direction = attackDirection) |> List.map (fun a -> a.DefenderIndex) |> List.contains i |> not))
                                      |> Array.sortByDescending (fun (_, d) -> (d |> damage attacker, effectivePower d, d.Initiative))
                                    //   |> Array.filter (fun (i, d) -> d.UnitCount > 0 && (attacks |> List.filter (fun a -> a.Direction = attackDirection) |> List.map (fun a -> a.DefenderIndex) |> List.contains i |> not))
                                      |> Array.tryHead
                           match pick with
                           | Some (_, p) -> let attack = {
                                                Direction = attackDirection
                                                AttackerIndex = match attackDirection with
                                                                | ImmuneToInfection -> immuneGroups |> Array.findIndex ((=) attacker)
                                                                | InfectionToImmune -> infectionGroups |> Array.findIndex ((=) attacker)
                                                DefenderIndex = match attackDirection with
                                                                | ImmuneToInfection -> infectionGroups |> Array.findIndex ((=) p)
                                                                | InfectionToImmune -> immuneGroups |> Array.findIndex ((=) p)
                                            }
                                            selectTarget t immuneGroups infectionGroups ( attack :: attacks )
                           | None -> selectTarget t immuneGroups infectionGroups attacks

    selectTarget allGroups immuneGroups infectionGroups []
    |> List.sortByDescending (fun a -> let attacker = match a.Direction with
                                                      | ImmuneToInfection -> immuneGroups.[a.AttackerIndex]
                                                      | InfectionToImmune -> infectionGroups.[a.AttackerIndex]
                                       attacker.Initiative)

let rec processAttacks attacks (immuneGroups : Group array) (infectionGroups : Group array) =
    match attacks with
    | [] -> ()
    | attack :: t -> let attackerGroups = match attack.Direction with
                                          | ImmuneToInfection -> immuneGroups
                                          | InfectionToImmune -> infectionGroups
                     let defenderGroups = match attack.Direction with
                                          | ImmuneToInfection -> infectionGroups
                                          | InfectionToImmune -> immuneGroups
                     let attacker = attackerGroups.[attack.AttackerIndex]
                     let defender = defenderGroups.[attack.DefenderIndex]
                     let damageDealt = damage attacker defender

                     defenderGroups.[attack.DefenderIndex] <- { defender with UnitCount = max 0 (defender.UnitCount - (damageDealt / defender.HitPoints))} 

                     processAttacks t immuneGroups infectionGroups

let rec fightUntilOver immuneGroups infectionGroups =
    if immuneGroups |> Array.forall (fun g -> g.UnitCount = 0)
    then infectionGroups |> Array.sumBy (fun g -> g.UnitCount), Some Infection
    elif infectionGroups |> Array.forall (fun g -> g.UnitCount = 0)
    then immuneGroups |> Array.sumBy (fun g -> g.UnitCount), Some ImmuneSystem
    else let attacks = selectTargets immuneGroups infectionGroups
         let totalUnits = (Array.sumBy (fun g -> g.UnitCount) immuneGroups) + (Array.sumBy (fun g -> g.UnitCount) infectionGroups)
         processAttacks attacks immuneGroups infectionGroups
         if totalUnits = (Array.sumBy (fun g -> g.UnitCount) immuneGroups) + (Array.sumBy (fun g -> g.UnitCount) infectionGroups)
         then 0, None
         else fightUntilOver immuneGroups infectionGroups

// Part 1
immuneBoost <- 0
let result = fightUntilOver (readImmuneGroups ()) (readInfectionGroups ())

immuneBoost <- 0

let rec findSmallestBoost () =
    immuneBoost <- immuneBoost + 1
    let result, winner = fightUntilOver (readImmuneGroups ()) (readInfectionGroups ())
    printfn "Boost: %d, winner: %A, remaining units: %d" immuneBoost winner result
    if winner = Some ImmuneSystem
    then result
    else findSmallestBoost ()

let result2 = findSmallestBoost ()

