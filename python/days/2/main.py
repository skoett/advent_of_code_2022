from pathlib import Path
from typing import Tuple
from dataclasses import dataclass


class UnknownHandError(Exception):
    pass


@dataclass()
class Score:
    win: int = 6
    draw: int = 3
    lose: int = 0
    game_a: int = 0
    game_b: int = 0

    def get_scores(self) -> Tuple[int, int]:
        return self.game_a, self.game_b


@dataclass()
class Hand:
    rock: int = 1
    paper: int = 2
    scissor: int = 3
    score: Score = Score()

    def parse_hand(self, hand: str):
        """
        Parses the given hand to a pre-determined integer value.
        Args:
            hand: The hand. Either rock, paper or scissor.

        Returns:
            The given integer value encapsulated in a dataclass variable.
        """
        match hand:
            case "A" | "X":
                return self.rock
            case "B" | "Y":
                return self.paper
            case "C" | "Z":
                return self.scissor
            case _:
                raise UnknownHandError("Hand should be in 'A', 'B', 'C', 'X', 'Y' or 'Z'")

    def lose(self, hand: int) -> int:
        return self.scissor if hand == self.rock else hand - 1

    def win(self, hand: int) -> int:
        return self.rock if hand == self.scissor else hand + 1

    def play(self, opp: str, you: str) -> int:
        """
        Plays a game of rock, paper scissors with two inputs.
        Args:
            opp: The opponents input in the form of "A", "B" or "C".
            you: The players input in the form of "X", "Y" or "Z".

        Returns:
            An integer value of the game simulation.
        """
        you = self.parse_hand(you)
        opp = self.parse_hand(opp)
        if you - opp % 3 == 1:
            return self.score.win + you
        return self.score.draw + you if you == opp else self.score.lose + you

    def tactics(self, opp: str, you: str) -> int:
        """
        Determines how to play in second part of the task.
        Args:
            opp: The rule we follow. I.e. rock = lose, paper = draw and scissor = win.
            you: Your hand. Either rock, paper or scissor.

        Returns:
            The integer value of the game following the tactic.
        """
        you = self.parse_hand(you)
        opp = self.parse_hand(opp)
        if you == self.rock:  # You need to lose
            return self.score.lose + self.lose(opp)
        if you == self.paper:  # We need a draw
            return self.score.draw + opp
        if you == self.scissor:  # We need to win
            return self.score.win + self.win(opp)
        raise UnknownHandError("Hand is not valid.")


def main(p: Path) -> Tuple[int, int]:
    hand = Hand()
    with open(p) as f:
        for line in f:
            line = line.rstrip('\n').split(" ")
            hand.score.game_a += hand.play(*line)
            hand.score.game_b += hand.tactics(*line)
    return hand.score.get_scores()


if __name__ == '__main__':
    data = Path("input.txt")
    result = main(data)
    print(f"Task 1: {result[0]}")
    print(f"Task 2: {result[1]}")
