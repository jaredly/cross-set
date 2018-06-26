
type color = C1|C2|C3|C4|C5|C6;
type shape = S1|S2|S3|S4|S5|S6;
type tile = {color, shape};
/* type board = array(array(option(tile))); */

module PosMap = Map.Make({
  type t = (int, int);
  let compare = compare;
});

type board = PosMap.t(tile);

/* check place
 * currently only checking shapes
*/
let colors = [C1,C2,C3,C4,C5,C6];
let shapes = [S1,S2,S3,S4,S5,S6];

let random = () => {color: Util.choose(colors), shape: Util.choose(shapes)};

type result = Shapes(list(shape)) | Colors(list(color)) | Nope | Any;

type direction = Up | Down | Right | Left;

let dd = d => switch d {
  | Up => (0, -1)
  | Down => (0, 1)
  | Left => (-1, 0)
  | Right => (1, 0)
};

let addPos = ((a, b), (c, d)) => (a + c, b + d);

/* let getTile = (board, (x, y)) => board[x][y]; */
let getTile = (board, pos) => if (PosMap.mem(pos, board)) { Some(PosMap.find(pos, board)) } else { None };
let setTile = (board, pos, tile) => PosMap.add(pos, tile, board);

let canPlaceTile = (board, pos, tile) => {
  let rec loop = (curPos, dpos, prev) => {
    switch (prev, getTile(board, curPos)) {
      | (_, None) => prev
      | (Shapes(shapes), Some({shape, color})) when shape == tile.shape => Nope
      | (Colors(colors), Some({shape, color})) when color == tile.color => Nope
      | (Any, Some({shape, color})) when color == tile.color && shape != tile.shape => {
          loop(addPos(curPos, dpos), dpos, Shapes([shape, tile.shape]))
      }
      | (Shapes(shapes), Some({shape, color})) when color == tile.color => {
        if (List.mem(shape, shapes)) {
          Nope
        } else {
          loop(addPos(curPos, dpos), dpos, Shapes([shape, ...shapes]))
        }
      }
      | (Any, Some({shape, color})) when shape == tile.shape && color != tile.color => {
          loop(addPos(curPos, dpos), dpos, Colors([color, tile.color]))
      }
      | (Colors(colors), Some({shape, color})) when shape == tile.shape => {
        if (List.mem(color, colors)) {
          Nope
        } else {
          loop(addPos(curPos, dpos), dpos, Colors([color, ...colors]))
        }
      }
      | _ => Nope
    }
  };
  let loop = (dpos) => {
    loop(addPos(pos, dpos), dpos)
  };
  let horiz = Any
  |> loop((1, 0)) /* right */
  |> loop((-1, 0)) /* left */
  ;
  let vert = Any
  |> loop((0, -1)) /* top */
  |> loop((0, 1)); /* bottom */
  (
    horiz != Nope
    && vert != Nope
    /* horiz != Any || vert != Any */
    )
};

let isAdjacent = (board, tiles, pos, dpos) => {
  let rec loop = (tiles, pos) => {
    switch tiles {
      | [] => false
      | [_, ...rest] => {
      switch (getTile(board, pos)) {
        | Some(_) => true
        | None => getTile(board, addPos(pos, (-1, 0))) != None
        || getTile(board, addPos(pos, (1, 0))) != None
        || getTile(board, addPos(pos, (0, 1))) != None
        || getTile(board, addPos(pos, (0, -1))) != None
        || loop(rest, addPos(pos, dpos))
      }
      }
    }
  };
  loop(tiles, pos)
};

let canPlaceTiles = (board, pos, direction, tiles) => {
  let dpos = dd(direction);

  let rec loop = (board, tiles, pos) => {
    switch tiles {
      | [] => Some([])
      | [tile, ...rest] => {
        switch (getTile(board, pos)) {
          | Some(_) => loop(board, tiles, addPos(pos, dpos))
          | None => {
            let canPlace = canPlaceTile(board, pos, tile);
            if (canPlace) {
 switch (loop(setTile(board, pos, tile), rest, addPos(pos, dpos))) {
   | None => None
   | Some(placements) => Some([(pos, tile), ...placements])
 }
            } else {
              None
            }
          }
        }
    }
  };
  };

  switch (getTile(board, pos)) {
    | Some(_) => None
    | None => {
      let places = loop(board, tiles, pos);
      switch (places) {
        | None => None
        | Some(places) => isAdjacent(board, tiles, pos, dpos) ? Some(places) : None
      }
    }
  }
};

let currentBounds = (board) => {
  PosMap.fold(((x, y), tile, (l, t, r, b)) => {
    (min(l, x), min(t, y), max(r, x), max(b, y))
  }, board, (0, 0, 0, 0))
};

let legalPlacements = (board, direction, tiles) => {
  let (x0, y0, x1, y1) = currentBounds(board);
  let placements = ref([]);
  let num = List.length(tiles);
  for (x in x0 - num to x1 + num) {
    for (y in y0 - num to y1 + num) {
      switch (canPlaceTiles(board, (x, y), direction, tiles)) {
        | None => ()
        | Some(places) => placements := [((x, y), places), ...placements^]
      }
    }
  };
  placements^
};

let legalTilePlacements = (board, tile) => {
  let (x0, y0, x1, y1) = currentBounds(board);
  let placements = ref([]);
  for (x in x0 - 1 to x1 + 1) {
    for (y in y0 - 1 to y1 + 1) {
      switch (getTile(board, (x, y))) {
      | Some(_) => ()
      | None =>
        let isFree = canPlaceTile(board, (x, y), tile);
        if (isFree) {
          placements := [(x, y), ...placements^]
        }
      }
    }
  };
  placements^
};