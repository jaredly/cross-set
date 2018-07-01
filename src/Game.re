
type state = {
  board: Board.board,
  tiles: list(Board.tile),
};

let init = () => {
  board: Board.PosMap.empty |> b => Board.setTile(b, (0, 0), Board.random()),
  tiles: [Board.random(), Board.random(), Board.random(), Board.random(), Board.random(), Board.random()],
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

let rec refreshTiles = (items, start, num) =>
  start <= 0 ?
    num == 0 ?
      items :
      (
        switch items {
        | [] => []
        | [first, ...rest] => [Board.random(), ...refreshTiles(rest, start, num - 1)]
        }
      ) :
    (
      switch items {
      | [] => []
      | [first, ...rest] => [first, ...refreshTiles(rest, start - 1, num)]
      }
    );

let trade = ({tiles, board}, start, len) => {board, tiles: refreshTiles(tiles, start, len)};
let placeTiles = (state, tiles, start, len) => {
  board: List.fold_left((board, (pos, tile)) => Board.setTile(board, pos, tile), state.board, tiles),
  tiles: refreshTiles(state.tiles, start, len),
};