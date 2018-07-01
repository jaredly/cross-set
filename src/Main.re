
open Reprocessing;

let withAlpha = ({Reprocessing_Common.r, g, b, a}, alpha) => {Reprocessing_Common.r, g, b, a: a *. alpha};

let colorColor = c => Board.(switch c {
  | Board.C1 => Constants.red
  | C2 => Constants.green
  | C3 => Constants.blue
  | C4 => Utils.color(~r=200, ~g=50, ~b=250, ~a=255)
  | C5 => Utils.color(~r=200, ~g=200, ~b=50, ~a=255)
  | C6 => Constants.black
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
  Draw.rect(~pos=(x + a, y + b), ~width=w, ~height=w, env)
};


let drawTile = (spriteSheet, {Board.shape, color}, (x, y), size, margin, env) => {
  let texX = Board.shapeIndex(shape) * 90;
  let texY = Board.colorIndex(color) * 90;
  Draw.subImage(
    spriteSheet,
    ~pos=(x + margin, y + margin),
    ~width=size - margin * 2, ~height=size - margin*2,
    ~texPos=(texX, texY),
    ~texWidth=90,
    ~texHeight=90,
    env
  );
};

type state = {
  spriteSheet: Reprocessing.imageT,
  /* board: Board.board,
  tiles: list(Board.tile), */
  game: Game.state,
  selection: option((int, int)),
  direction: Board.direction,
  dragPos: option(((int, int), (int, int))),
  offset: (int, int),
};

let size = 45;

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
let tradePos = env => (Env.width(env) - size - size - size, 0);
let hitsTrade = env => {
  let (mx, my) = Env.mouse(env);
  let (x, y) = tradePos(env);
  mx >= x && mx <= x + size && my >= y && my <= y + size
};

Reprocessing.run(
  ~setup=env => {
    Reprocessing.Env.size(~width=600, ~height=600, env);
    {
      game: Game.init(),
      /* board: Board.PosMap.empty |> b => Board.setTile(b, (0, 0), Board.random()),
      tiles: [Board.random(), Board.random(), Board.random(), Board.random(), Board.random(), Board.random()], */
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
    let numTiles = List.length(state.game.tiles);
    if (hitsTrade(env)) {
      switch (state.selection) {
        | None => {
          state
        }
        | Some((start, len)) => {
          {...state, game: Game.trade(state.game, start, len)}
        }
      }
    } else if (hitsToggle(env)) {
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
            game: {
              ...state.game,
              tiles: swapTiles(state.game.tiles, start, my / size)
            },
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
          let selection = Game.subList(state.game.tiles, start, len);
          let placements = Board.legalPlacements(state.game.board, state.direction, selection);
          let rec loop = placements => {
            switch placements {
              | [] => {...state, dragPos: Some(((mx,my), state.offset))}
              | [(pos, tiles), ...rest] => {
                if (mouseInTile(state.offset, (mx, my), pos, env)) {
                  {
                    ...state,
                    game: Game.placeTiles(state.game, tiles, start, len),
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

    let (x0, y0, x1, y1) = Board.currentBounds(state.game.board);
    for (x in x0 to x1) {
      for (y in y0 to y1) {
          switch (Board.getTile(state.game.board, (x, y))) {
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
        let selection = Game.subList(state.game.tiles, start, len);
        let placements = Board.legalPlacements(state.game.board, state.direction, selection);

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

    let numTiles = List.length(state.game.tiles);
    state.game.tiles |> List.iteri((i, tile) => {
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

    Draw.subImage(state.spriteSheet, ~pos=togglePos(env),
      ~width=size,
      ~height=size,
      ~texPos=((state.direction == Board.Down ? 0 : 90), 90 * 6),
      ~texWidth=90,
      ~texHeight=90,
      env
    );

    Draw.subImage(state.spriteSheet, ~pos=tradePos(env),
      ~width=size,
      ~height=size,
      ~texPos=(90 * 2, 90 * 6),
      ~texWidth=90,
      ~texHeight=90,
      env
    );

    state
  },
  ()
);
