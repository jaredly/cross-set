
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

let withAlpha = ({Reprocessing_Common.r, g, b, a}, alpha) => {Reprocessing_Common.r, g, b, a: a *. alpha};


open Reprocessing;
let drawTile = (spriteSheet, {Board.shape, color}, (x, y), size, margin, env) => {
  let texX = Board.shapeIndex(shape) * 45;
  let texY = Board.colorIndex(color) * 45;
  Draw.subImage(
    spriteSheet,
    ~pos=(x + margin, y + margin),
    ~width=size - margin * 2, ~height=size - margin*2,
    ~texPos=(texX, texY),
    ~texWidth=45,
    ~texHeight=45,
    env
  );
  /* Draw.noStroke(env);
  Draw.fill(colorColor(color), env);
  Draw.rect(~pos=(x + margin, y + margin), ~width=size - margin * 2, ~height=size - margin * 2, env);
  Draw.fill(Constants.white, env);
  showShape(shape, x + margin, y + margin, size - margin * 2, env); */
};

type state = {
  spriteSheet: Reprocessing.imageT,
  board: Board.board,
  tiles: list(Board.tile),
  selection: option((int, int)),
  direction: Board.direction,
  dragPos: option(((int, int), (int, int))),
  offset: (int, int),
};

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

let swapTiles = (tiles, x0, x1) => {
  let arr = Array.of_list(tiles);
  let tile = arr[x1];
  arr[x1] = arr[x0];
  arr[x0] = tile;
  Array.to_list(arr)
};

let togglePos = env => (Env.width(env) - size - size, 0);
let hitsToggle = env => {
  let (mx, my) = Env.mouse(env);
  let (x, y) = togglePos(env);
  mx >= x && mx <= x + size && my >= y && my <= y + size
};

Reprocessing.run(
  ~setup=env => {
    Reprocessing.Env.size(~width=400, ~height=400, env);
    {
      board: Board.PosMap.empty |> b => Board.setTile(b, (0, 0), Board.random()),
      tiles: [Board.random(), Board.random(), Board.random(), Board.random(), Board.random(), Board.random()],
      selection: Some((0, 2)),
      spriteSheet: Reprocessing.Draw.loadImage(~filename="Tiles.png", env),
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
    if (hitsToggle(env)) {
      {...state, direction: state.direction == Board.Down ? Board.Right : Board.Down}
    } else if (mx > Env.width(env) - size && my < numTiles * size) {
      if (Env.key(Reprocessing.Events.LeftShift, env) || Env.key(Reprocessing.Events.RightShift, env)) {
        let final = my / size;
        switch (state.selection) {
          | None => {...state, selection: Some((final, 1))}
          | Some((start, len)) => {...state, selection: start < final ? Some((start, final - start + 1)) : Some((final, start - final + 1))}
        }
      } else {
        switch (state.selection) {
          | Some((start, 1)) when Env.key(Events.LeftOsKey, env) || Env.key(Events.RightOsKey, env) => {
            {
            ...state,
            tiles: swapTiles(state.tiles, start, my / size),
            selection: Some((my / size, 1))
          }
          }
          | _ => {...state, selection: Some((my / size, 1))}
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
              | [] => {...state, dragPos: Some(((mx,my), state.offset))}
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
              drawTile(state.spriteSheet, tile, (wx(x), wy(y)), size, 0, env)
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
            drawTile(state.spriteSheet, tile, (wx(x), wy(y)), size, 4, env)
          })
          }
        });
      }
    };

    let numTiles = List.length(state.tiles);
    state.tiles |> List.iteri((i, tile) => {
      let pos = (Env.height(env) - size, i * size);
      drawTile(state.spriteSheet, tile, pos, size, 2, env);
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

    Draw.noStroke(env);
    Draw.fill(Utils.color(~r=50, ~g=250, ~b=250, ~a=255), env);
    Draw.rect(~pos=togglePos(env), ~width=size, ~height=size, env);
    Draw.fill(Utils.color(~r=0, ~g=150, ~b=150, ~a=255), env);
    let (x, y) = togglePos(env);
    if (state.direction == Board.Down) {
      Draw.rect(~pos=(x + size / 3, y), ~width=size/3, ~height=size, env);
    } else {
      Draw.rect(~pos=(x, y + size / 3), ~width=size, ~height=size/3, env);

    };

    state
  },
  ()
);
