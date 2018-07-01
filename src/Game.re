type gameState = {
  board: Board.board,
  currentPlayer: int,
  hands: array(list(Board.tile)),
  allTiles: list(Board.tile)
};

let drawTile = allTiles => {
  let nth = Random.int(List.length(allTiles));
  let (_, tile, tiles) = List.fold_left(
    ((n, picked, tiles), tile) => n == nth ? (n + 1, Some(tile), tiles) : (n + 1, picked, [tile, ...tiles]),
    (0, None, []),
    allTiles
  );
  (tile, tiles)
};
open Infix;

let drawHand = allTiles => {
  let rec loop = (i, hand, tiles) => {
    i == 0 ? (hand, tiles) : {
      let (tile, tiles) = drawTile(tiles);
      let tile = tile |! "Unable to draw hand";
      loop(i - 1, [tile, ...hand], tiles)
    }
  };
  loop(6, [], allTiles)
};

let rec drawHands = (hands, allTiles) => hands <= 0 ? ([], allTiles) : {
  let (hand, allTiles) = drawHand(allTiles);
  let (hands, allTiles) = drawHands(hands - 1, allTiles);
  ([hand, ...hands], allTiles)
};

let refillHand = (hand, tiles, allTiles) => {
  let rec loop = (hand, tiles, allTiles) => switch tiles {
    | [] => (hand, allTiles)
    | [{Board.id}, ...tiles] => {
      let (tile, allTiles) = drawTile(allTiles);
      let tile = tile |! "Cannot draw";
      loop(hand |> List.map(t => t.Board.id == id ? tile : t), tiles, allTiles)
    }
  };
  loop(hand, tiles, allTiles)
};

let init = () => {
  let (tile, allTiles) = drawTile(Board.allTiles());
  let tile = tile |! "Unable to find a tile to start";
  let (hands, allTiles) = drawHands(4, allTiles);
  {
    board: Board.PosMap.empty |> ((b) => Board.setTile(b, (0, 0), tile)),
    currentPlayer: 0,
    hands: Array.of_list(hands),
    allTiles,
  }
};

let rec subList = (items, start, num) =>
  start <= 0 ?
    num == 0 ?
      [] :
      (
        switch items {
        | [] => []
        | [first, ...rest] => [first, ...subList(rest, start, num - 1)]
        }
      ) :
    (
      switch items {
      | [] => []
      | [_, ...rest] => subList(rest, start - 1, num)
      }
    );

let rec refreshTiles = (items, start, num) =>
  start <= 0 ?
    num == 0 ?
      items :
      (
        switch items {
        | [] => []
        | [first, ...rest] => [None, ...refreshTiles(rest, start, num - 1)]
        }
      ) :
    (
      switch items {
      | [] => []
      | [first, ...rest] => [first, ...refreshTiles(rest, start - 1, num)]
      }
    );
