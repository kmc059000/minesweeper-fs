//generic console printing utilities

module ConsolePrinting

open System

type ConsoleCoords = {X: int; Y: int}

type ConsoleString = {Text:string; Foreground: ConsoleColor; Background: ConsoleColor; }

type ConsoleOutput = { 
    String: ConsoleString;
    Coords: ConsoleCoords
}

module ConsoleString =
    let create text foreground background = { Text = text; Foreground = foreground; Background = background }

module ConsoleCoords =
    let create x y = { X = x; Y = y; }
    let origin = create 0 0

module ConsoleOutput =
    let emptyUI = List.empty<ConsoleOutput>

    let private isNewLine s = s = '\n'

    let private  newLineCount s = s |> String.filter isNewLine |> String.length

    let private lengthAfterLastNewLine hasNewLines s =
        match hasNewLines with
        | false -> String.length s
        | true -> (String.length s) - s.LastIndexOf('\n')

    let private getNewCoords coords s = 
        let yDiff = s |> newLineCount
        let xDiff = s |> lengthAfterLastNewLine (yDiff > 0)
        let newX = if yDiff > 0 then 0 else coords.X + xDiff
        let newY = coords.Y + yDiff
        { X = newX; Y = newY }

    let rec withCoords startCoords lst =
        match lst with
        | [] -> []
        | x::xs -> 
            let item = { String = x; Coords = startCoords; }
            let newCoords = getNewCoords startCoords x.Text
            item :: (withCoords newCoords xs)


let private pringConsoleText next =
    System.Console.SetCursorPosition(next.Coords.X, next.Coords.Y)
    System.Console.ForegroundColor <- next.String.Foreground
    System.Console.BackgroundColor <- next.String.Background
    printf "%s" next.String.Text

let rec printConsoleText lastPos prevRows newRows =
    match prevRows,newRows with
    | [],[] -> 
        System.Console.SetCursorPosition(lastPos.X, lastPos.Y)
    | [], next::nexts -> 
        pringConsoleText next
        printConsoleText next.Coords [] nexts 
    | prev::prevs, next::nexts ->
        match prev = next with
        | true -> ()
        | false -> pringConsoleText next
        printConsoleText next.Coords prevs nexts
    | _,[] -> failwith "Previous is longer than next."