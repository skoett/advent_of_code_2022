from typing import Tuple


def main(input_file: str) -> Tuple[int, int]:
    max_cal, current = 0, 0
    top_3: list[int] = [0] * 3
    with open(input_file) as f:
        for line in f:
            line = line.rstrip('\n')
            if line:
                current += int(line)
            if current > max_cal:
                max_cal = current
            if not line:
                if min(top_3) < current:
                    top_3[top_3.index(min(top_3))] = current
                current = 0
    return max_cal, sum(top_3)


if __name__ == '__main__':
    data = "input.txt"
    task_1, task_2 = main(data)
    print(f"Task 1: {task_1}\nTask 2: {task_2}")
