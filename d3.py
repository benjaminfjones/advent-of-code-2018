import re
from typing import List, Tuple, Dict

Claim = Tuple[int, int, int, int, int]

def parse_claim(s: str) -> Claim:
    r = re.search(r'#(\d+) @ (\d+),(\d+): (\d+)x(\d+)', s)
    return (int(r.group(1)),
            int(r.group(2)),
            int(r.group(3)),
            int(r.group(4)),
            int(r.group(5)))

def parse_input(fn: str) -> List[Claim]:
    with open(fn) as f:
        res = [ parse_claim(l) for l in f ]
    return res

# histogram: map from position to frequency
Histogram = Dict[Tuple[int,int], int]

def histo(cs: List[Claim]) -> Histogram:
    res = {}
    for c in cs:
        base_pos = (c[1], c[2])
        for x in range(c[3]):
            for y in range(c[4]):
                pos = (base_pos[0] + x, base_pos[1] + y)
                if pos in res:
                    res[pos] = res[pos] + 1
                else:
                    res[pos] = 1
    return res

def check_claim(c: Claim, h: Histogram) -> bool:
    base_pos = (c[1], c[2])
    for x in range(c[3]):
        for y in range(c[4]):
            pos = (base_pos[0] + x, base_pos[1] + y)
            if h[pos] != 1:
                return False
    return True


# test data
test_list: List[str] =  ['#1 @ 1,3: 4x4',
                         '#2 @ 3,1: 4x4',
                         '#3 @ 5,5: 2x2']
test_input: List[Claim] = [ parse_claim(l) for l in test_list ]


if __name__ == '__main__':
    inp: List[Claim] = parse_input('input3')
    h: Histogram = histo(inp)
    t: int = 0
    for p in h:
        if h[p] > 1:
            t = t + 1
    print('Area of overlaps: ', t)

    no_overlaps = [ c for c in inp if check_claim(c,h) ]
    print('No overlap claims: ', no_overlaps)
