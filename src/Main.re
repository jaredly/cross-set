
let board = Board.PosMap.empty;
let single = board
|> b => Board.setTile(b, (0, 0), {Board.color: Board.C1, shape: Board.S1});
let board = single
|> b => Board.setTile(b, (1, 0), {Board.color: Board.C2, shape: Board.S1})
|> b => Board.setTile(b, (0, 1), {Board.color: Board.C1, shape: Board.S2})
;

let sameColor = Board.([
  {color: C1, shape: S2},
  {color: C1, shape: S4},
  {color: C1, shape: S5},
]);

let sameShape = Board.([
  {color: C2, shape: S1},
  {color: C3, shape: S1},
  {color: C4, shape: S1},
]);

let colorColor = c => Board.(switch c {
  | Board.C1 => Reprocessing.Constants.red
  | C2 => Reprocessing.Constants.green
  | C3 => Reprocessing.Constants.blue
  | C4 => Reprocessing.Constants.black
  | C5 => Reprocessing.Constants.black
  | C6 => Reprocessing.Constants.black
});
let colorName = c => Board.(switch c {
  | Board.C1 => "r"
  | C2 => "g"
  | C3 => "b"
  | C4 => "w"
  | C5 => "y"
  | C6 => "k"
});
let shapeName = s => Board.(switch s {
  | Board.S1 => "*"
  | S2 => "/"
  | S3 => "\\"
  | S4 => "%"
  | S5 => "^"
  | S6 => "&"
});

let showBoard = (board, placements) => {
  let (x0, y0, x1, y1) = Board.currentBounds(board);
  let b = Buffer.create(5);
  for (x in x0 - 4 to x1 + 4) {
    Buffer.add_string(b, " | ");
    for (y in y0 - 4 to y1 + 4) {
      if (List.mem((x, y), placements)) {
        Buffer.add_string(b, "()")
      } else {
        switch (Board.getTile(board, (x, y))) {
          | None => Buffer.add_string(b, "  ")
          | Some({Board.color, shape}) => Buffer.add_string(b, colorName(color) ++ shapeName(shape))
        }
      };
      Buffer.add_string(b, " | ");
    };
    Buffer.add_string(b, "\n");
  };
  Buffer.contents(b)
};

Reprocessing.run(
  ~setup=env => {
    Reprocessing.Env.size(~width=800, ~height=800, env);
    ()
  },
  ~draw=(state, env) => {
    let tile = Board.{color: C3, shape: S1};
    let placements = Board.legalTilePlacements(single, tile);
    let placements = Board.legalPlacements(board, Board.Left, sameShape);
    Reprocessing.Draw.background(Reprocessing.Constants.white, env);

    let (x0, y0, x1, y1) = Board.currentBounds(board);
    let num = List.length(sameShape);
    for (x in x0 - num to x1 + num) {
      for (y in y0 - num to y1 + num) {
          switch (Board.getTile(board, (x, y))) {
            | None => ()
            | Some({Board.color, shape}) => {
              let size = 20;
              Reprocessing.Draw.fill(colorColor(color), env);
              Reprocessing.Draw.rect(
                ~pos=(x * size + 400, y * size + 400),
                ~width=size,
                ~height=size,
                env
              );
              /* Buffer.add_string(b, colorName(color) ++ shapeName(shape)) */
            }
          }
      };
    };
  },
  ()
);

/* print_endline(showBoard(board, placements)); */

