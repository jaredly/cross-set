
let localState = ref(Game.init());

let pending = ref(true);

let check = (number) => {
  let r = pending^;
  pending := false;
  if (r) {
    Some((localState^.board, localState^.hands[number], localState^.currentPlayer == number))
  } else {
    None
  }
};

let tradeTiles = (player, tiles) => {
  pending := true;
  let (hand, allTiles) = Game.refillHand(localState^.hands[player], tiles, localState^.allTiles);
  let allTiles = tiles @ allTiles;
  let hands = Array.copy(localState^.hands);
  hands[player] = hand;
  localState := {
    ...localState^,
    allTiles,
    hands
  };
};

let placeTiles = (player, tiles, pos, direction) => {
  pending := true;
  switch (Board.canPlaceTiles(localState^.board, pos, direction, tiles)) {
    | None => ()
    | Some(places) => {
      let (hand, allTiles) = Game.refillHand(localState^.hands[player], tiles, localState^.allTiles);
      let hands = Array.copy(localState^.hands);
      hands[player] = hand;
      localState := {
        ...localState^,
        board: List.fold_left((board, (pos, tile)) => Board.setTile(board, pos, tile), localState^.board, places),
        allTiles,
        hands
      };
    }
  }
};