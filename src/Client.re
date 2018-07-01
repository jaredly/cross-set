type state = {
  board: Board.board,
  tiles: list(option(Board.tile)),
  myTurn: bool,
};

type remote;

/* let checkRemote: remote => option((Board.board, list(Board.tile))) = remote => {
  /* Unix.select */
}; */

let trade = ({tiles, board, myTurn}, start, len) => {
  {board, tiles: Game.refreshTiles(tiles, start, len), myTurn};
};

/* let placeTiles = (state, tiles, start, len) => {
     board:
       List.fold_left((board, (pos, tile)) => Board.setTile(board, pos, tile), state.board, tiles),
     tiles: Game.refreshTiles(state.tiles, start, len)
   }; */
let placeTiles = (tiles, pos, direction) => ();
let tradeTiles = (tiles) => ();

/* let init = () => {
  let {Game.board, hands} = Game.init();
  {board, tiles: hands[0] |> List.map((t) => Some(t)), myTurn: false}
}; */