test_data = 'dabAcCaCBAcCcaDA'


def reduce(inp: str) -> str:
    i: int = 0
    while i < len(inp)-1:

        if inp[i] == inp[i+1].swapcase():
            inp = inp[:i] + inp[i+2:]

            # backup one position to look for new reductions
            #
            # dabAcCaCBAcCcaDA
            #     ^
            # dabAaCBAcCcaDA
            #    ^
            # dabCBAcCcaDA
            #   ^

            i = max(0, i-1)

        else:
            i = i + 1

    return inp


def main():

    with open('input5') as f:
        inp: str = f.read().rstrip()

    # part 1
    red_inp = reduce(inp)
    print('Final string prefix:', red_inp[:10], '...', red_inp[-10:])
    print('Final string length:', len(red_inp))

    # part 2

    # ['a', ..., 'z']
    units = [chr(97+x) for x in range(26)]

    res = []
    for u in units:
        print('Removing', u)
        inp1 = list(filter(lambda c: c.lower() != u, inp))
        red1 = reduce(inp1)
        print('  len', len(red1))
        res.append((u, len(red1)))

    best = min(res, key=lambda x: x[1])
    print('Best unit to remove:', best)


if __name__ == '__main__':
    main()
