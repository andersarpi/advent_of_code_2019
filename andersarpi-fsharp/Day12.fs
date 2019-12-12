module Day12

let (++) (x1,y1, z1) (x2,y2, z2) = (x1+x2, y1+y2, z1+z2)

type Body(pos) =
    let mutable pos = pos
    let mutable vel = (0, 0, 0)

    member _.Pos () = pos

    member _.ApplyVel () =
        pos <- pos ++ vel

    member _.ApplyGravity (b: Body) =
        let (x, y, z) = b.Pos()
        let (x', y', z') = pos
        vel <- vel ++ (
            (if x' > x then -1 elif x' < x then 1 else 0),
            (if y' > y then -1 elif y' < y then 1 else 0),
            (if z' > z then -1 elif z' < z then 1 else 0)
        )

    member _.Energy() =
        let (xp,yp,zp) = pos
        let (xv,yv,zv) = vel

        (abs xp + abs yp + abs zp) * (abs xv + abs yv + abs zv)


    override _.ToString() = sprintf "pos=<%O>, vel=<%O>" pos vel

let part1 () =
    let moons = [
        Body(13, 9, 5);
        Body(8, 14, -2);
        Body(-5, 4, 11);
        Body(2, -6, 1)
    ]

    for _ in [1..1000] do
        for m1 in moons do
            for m2 in moons do
                if m1 <> m2 then
                    m1.ApplyGravity m2
                else
                    ()
        for m1 in moons do
            m1.ApplyVel()

    moons |> List.sumBy (fun m -> m.Energy()) |> printfn "%O"
