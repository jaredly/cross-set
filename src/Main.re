
let colorColor = c => Board.(switch c {
  | Board.C1 => Reprocessing.Constants.red
  | C2 => Reprocessing.Constants.green
  | C3 => Reprocessing.Constants.blue
  | C4 => Reprocessing_Utils.color(~r=200, ~g=50, ~b=250, ~a=255)
  | C5 => Reprocessing_Utils.color(~r=200, ~g=200, ~b=50, ~a=255)
  | C6 => Reprocessing.Constants.black
});
let showShape = (s, x, y, size, env) => {
  let w = size / 3;
  let (a, b) = switch s {
  | Board.S1 => (0, 0)
  | S2 => (w, 0)
  | S3 => (w * 2, 0)
  | S4 => (0, w)
  | S5 => (w, w)
  | S6 => (w * 2, w)
  };
  Reprocessing.Draw.rect(~pos=(x + a, y + b), ~width=w, ~height=w, env)
};
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

let withAlpha = ({Reprocessing_Common.r, g, b, a}, alpha) => {Reprocessing_Common.r, g, b, a: a *. alpha};

open Reprocessing;
let drawTile = ({Board.shape, color}, (x, y), size, margin, env) => {
  Reprocessing.Draw.noStroke(env);
  Reprocessing.Draw.fill(colorColor(color), env);
  Reprocessing.Draw.rect(~pos=(x + margin, y + margin), ~width=size - margin * 2, ~height=size - margin * 2, env);
  Reprocessing.Draw.fill(Reprocessing.Constants.white, env);
  showShape(shape, x + margin, y + margin, size - margin * 2, env);
};

type state = {
  board: Board.board,
  tiles: list(Board.tile),
  selection: option((int, int)),
  direction: Board.direction,
  dragPos: option(((int, int), (int, int))),
  offset: (int, int),
};

/* let board = Board.PosMap.empty;
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

    let tile = Board.{color: C3, shape: S1};
let tiles = [tile]; */

let rec subList = (items, start, num) => start <= 0
? (num == 0 ? [] : switch items {
  | [] => []
  | [first, ...rest] => [first, ...subList(rest, start, num - 1)]
})
: switch items {
  | [] => []
  | [_, ...rest] => subList(rest, start - 1, num)
};

let rec refreshTiles = (items, start, num) => {
start <= 0
? (num == 0 ? items : switch items {
  | [] => []
  | [first, ...rest] => [Board.random(), ...refreshTiles(rest, start, num - 1)]
})
: switch items {
  | [] => []
  | [first, ...rest] => [first, ...refreshTiles(rest, start - 1, num)]
};
};

let size = 40;

let mouseInTile = ((ox, oy), (mx, my), (x, y), env) => {
  let x = x * size + ox;
  let y = y * size + oy;
  mx >= x && mx <= x + size &&
  my >= y && my <= y + size
};

Reprocessing.run(
  ~setup=env => {
    Reprocessing.Env.size(~width=400, ~height=400, env);
    {
      board: Board.PosMap.empty |> b => Board.setTile(b, (0, 0), Board.random()),
      tiles: [Board.random(), Board.random(), Board.random(), Board.random(), Board.random(), Board.random()],
      selection: Some((0, 2)),
      direction: Board.Down,
      offset: (200, 200),
      dragPos: None,
    }
  },
  ~mouseUp=(state, env) => {...state, dragPos: None},
  ~mouseDragged=(state, env) => {
    let (mx, my) = Env.mouse(env);
    switch state.dragPos {
      | None => state
      | Some(((sx, sy), (ox, oy))) => {
        {
          ...state,
          offset: (ox + (mx - sx), oy + (my - sy))
        }
      }
    }
  },
  ~mouseDown=(state, env) => {
    let (mx, my) = Env.mouse(env);
    let numTiles = List.length(state.tiles);
    if (mx > Env.width(env) - size && my < numTiles * size) {
      if (Env.key(Reprocessing.Events.LeftShift, env) || Env.key(Reprocessing.Events.RightShift, env)) {
        let final = my / size;
        switch (state.selection) {
          | None => {...state, selection: Some((final, 1))}
          | Some((start, len)) => {...state, selection: start < final ? Some((start, final - start + 1)) : Some((final, start - final + 1))}
        }
      } else {
        {
          ...state,
          selection: Some((my / size, 1))
        }
      }
    } else {
      switch (state.selection) {
        | None => {
          {...state, dragPos: Some(((mx, my), state.offset))}
        }
        | Some((start, len)) => {
          let selection = subList(state.tiles, start, len);
          let placements = Board.legalPlacements(state.board, state.direction, selection);
          let rec loop = placements => {
            switch placements {
              | [] => {...state, selection: None, dragPos: Some(((mx,my), state.offset))}
              | [(pos, tiles), ...rest] => {
                if (mouseInTile(state.offset, (mx, my), pos, env)) {
                  {
                    ...state,
                    board: List.fold_left((board, (pos, tile)) => Board.setTile(board, pos, tile), state.board, tiles),
                    tiles: refreshTiles(state.tiles, start, len),
                    selection: None
                  }
                } else {
                  loop(rest)
                }
              }
            }
          };
          loop(placements)
        }
      }
    };
  },
  ~draw=(state, env) => {
    Reprocessing.Draw.background(Reprocessing.Constants.white, env);

    let (ox, oy) = state.offset;
    let wx = x => x * size + ox;
    let wy = y => y * size + oy;

    let (x0, y0, x1, y1) = Board.currentBounds(state.board);
    for (x in x0 to x1) {
      for (y in y0 to y1) {
          switch (Board.getTile(state.board, (x, y))) {
            | None => ()
            | Some(tile) => {
              drawTile(tile, (wx(x), wy(y)), size, 0, env)
            }
          }
      };
    };

    let (mx,my) = Env.mouse(env);

    switch (state.selection) {
      | None => ()
      | Some((start, len)) => {
        let selection = subList(state.tiles, start, len);
        let placements = Board.legalPlacements(state.board, state.direction, selection);

        placements |> List.iter((((x, y), tiles)) => {
          open Reprocessing;
          Draw.noFill(env);
          Draw.stroke(Constants.black, env);
          Draw.strokeWeight(3, env);
          Draw.rect(~pos=(wx(x), wy(y)), ~width=size, ~height=size, env);

          if (mx >= wx(x) && mx < wx(x) + size && my >= wy(y) && my <= wy(y) + size) {

          tiles |> List.iter((((x,y), tile)) => {
            drawTile(tile, (wx(x), wy(y)), size, 4, env)
          })
          }
        });
      }
    };

    let numTiles = List.length(state.tiles);
    state.tiles |> List.iteri((i, tile) => {
      let pos = (Env.height(env) - size, i * size);
      drawTile(tile, pos, size, 2, env);
      if (mx > Env.width(env) - size && my < numTiles * size) {
        let final = my / size;
      if (i == final) {
          Draw.noFill(env);
          Draw.stroke(withAlpha(Constants.black, 0.5), env);
          Draw.rect(~pos=pos, ~width=size, ~height=size, env)
      };
      };
    });

    switch (state.selection) {
      | None => ()
      | Some((start, len)) => {
        for (i in start to start + len - 1) {
          let x = Env.height(env) - size;
          let y = size * i;
          Draw.noFill(env);
          Draw.stroke(Constants.black, env);
          Draw.rect(~pos=(x, y), ~width=size, ~height=size, env)
        }
      }
    };

    state
  },
  ()
);
