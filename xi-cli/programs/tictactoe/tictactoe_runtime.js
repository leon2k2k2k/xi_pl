import { tictactoe } from "./tictactoe.ts";

export const play_result_invalid = { message: "invalid" };

// export function playResultMove(pos, square, won) {
//   return { message: "invalid", square: square, position: pos, won: won };
// }

export function play_result_move(board) {
  return (square) =>
    (won) => ({
      message: "move",
      board: board,
      square: square,
      won: won,
    });
}

export function tictactoe1(play) {
  return (init_board) =>
    (init_square) =>
      async () => {
        return await tictactoe(play, init_board, init_square);
      };
}

export const Object = "Object";
