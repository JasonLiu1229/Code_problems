from pathlib import Path


def solve(input: Path):
    def parse_line(line):
        line = line.strip()
        return line[0], int(line[1:])

    dial = 50
    count = 0
    with open(input, "r") as f:
        for line in f:
            direction, number = parse_line(line)
            if direction == "L":
                dial -= number
            else:
                dial += number

            dial = dial % 100
            if dial == 0:
                count += 1
    return count


if __name__ == "__main__":
    print(solve("input.txt"))
