from fractions import Fraction
from typing import Dict, List, Optional, Set, Tuple


Point = Tuple[int, int]
test_data: List[Point] = [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
"""
aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf

0000.22
0033422
0333422
.333442
1.34444
11.4444
11.4445
11.4455
"""


class Line:
    """
    Represents a line, either vertical, or passing through self.intercept
    with slope self.slope[1]/self.slope[0].

    y = m x + b
    p[1] = self.slope[1]/self.slope[0] * p[0] + b
    b = p[1] - self.slope[1]/self.slope[0] * p[0]
    """

    is_vert: bool
    xintercept: int

    slope: Fraction
    b: Fraction

    def __init__(self, p1, p2):
        if p1[0] == p2[0]:
            self.is_vert = True
            self.xintercept = p1[0]
            self.slope = 0  # dummy
            self.b = 0      # dummy
        else:
            self.is_vert = False
            self.xintercept = 0  # dummy
            self.slope = Fraction(p2[1]-p1[1], p2[0]-p1[0])
            self.b = Fraction(p1[1]) - self.slope*Fraction(p1[0])

    def is_above(self, p):
        if self.is_vert:
            return p[0] == self.xintercept
        else:
            return Fraction(p[1]) >= self.slope*p[0] + self.b

    def is_below(self, p):
        if self.is_vert:
            return p[0] == self.xintercept
        else:
            return Fraction(p[1]) <= self.slope*p[0] + self.b

    def separates(self, xs: List[Point]) -> bool:
        """
        Does the given set of points fall completely to one side or the other?
        """
        return (all([self.is_above(x) for x in xs]) or
                all([self.is_below(x) for x in xs]))


def on_convex_hull(p: Point, xs: List[Point]) -> bool:
    """
    Is the given point on the convex hull of the set of points?
    """
    for q in xs:
        line = Line(p, q)
        if line.separates(xs):
            return True
    return False


def l1(p1: Point, p2: Point) -> int:
    """
    L_1 distance on the 2d integer lattice
    """
    return abs(p2[0]-p1[0]) + abs(p2[1]-p1[1])


def make_grid(ll: Point, ur: Point) -> List[Point]:
    """
    Return the 2d integer lattice contained in the rectangle with given lower
    left and upper right corners.
    """
    return [(ll[0]+i, ll[1]+j)
            for i in range(ur[0]-ll[0]+1)
            for j in range(ur[1]-ll[1]+1)]


def belongs(p: Point, xs: List[Point]) -> Optional[int]:
    """
    Determine which point in the input list the given point is closest too.
    Returns 'None' if there is a tie.
    """
    if not xs:
        raise Exception('belongs: no points given')
    elif len(xs) == 1:
        return 0  # index of unique point in the list
    else:
        dists = [(i, l1(p, x)) for i, x in enumerate(xs)]
        sdists = sorted(dists, key=lambda x: x[1])
        if sdists[0][1] == sdists[1][1]:
            # tie!
            return None
        else:
            return sdists[0][0]


def compute_cells(xs: List[Point], buff: int = 0, draw: bool = False) ->\
                  Dict[int, int]:
    """
    Compute the lattice area of each Voronoi cell after intersecting with a
    tight bounding rectangle.
    """
    minx = min([x[0] for x in xs])
    miny = min([x[1] for x in xs])
    maxx = max([x[0] for x in xs])
    maxy = max([x[1] for x in xs])
    ll_corner = (minx-buff, miny-buff)
    width = maxx - minx + 2*buff
    height = maxy - miny + 2*buff

    # map from point's index to area of cell
    res: Dict[int, int] = {}
    for y in range(height+1):
        # if draw:
        #     pline = ''

        for x in range(width+1):
            p = (ll_corner[0]+x, ll_corner[1]+y)
            b = belongs(p, xs)

            if b is None:
                # if draw:
                #     pline = pline + '.'
                pass
            else:
                res[b] = 1 if b not in res else res[b] + 1
                # if draw:
                #     pline = pline + f'{b}'
        # if draw:
        #     print(pline)
    return res


def compute_safe_area(xs: List[Point], region: Set[Point]
                      ) -> int:
    """
    Count the number of points in 'region' that are within 10000 summed
    L_1 distance over all the given points.
    """
    count: int = 0
    for p in region:
        tdist = sum([l1(p, x) for x in xs])
        if tdist < 10000:
            count = count + 1
    return count


def main(part1: bool = True, part2: bool = True) -> None:

    #
    # Parse the input
    #

    # data = test_data
    data: List[Tuple[int, int]] = []
    with open('input6') as f:
        inp: List[str] = f.readlines()
        for l in inp:
            ls = l.split(',')
            x = int(ls[0].strip())
            y = int(ls[1].strip())
            data.append((x, y))

    minx = min([x[0] for x in data])
    miny = min([x[1] for x in data])
    maxx = max([x[0] for x in data])
    maxy = max([x[1] for x in data])
    ll_corner = (minx, miny)
    ur_corner = (maxx, maxy)
    width = maxx - minx
    height = maxy - miny
    print(f'Bounding rectangle @ {ll_corner} {width}x{height}')

    # convex_map = {p: on_convex_hull(p, data) for p in data}
    # better to use itertools here

    # on_hull = {p: t for p, t in convex_map.items() if t}
    # ind_on_hull: Set[int] = {data.index(p) for p, _ in on_hull.items()}

    # in_hull = {p: t for p, t in convex_map.items() if not t}
    # num_on = len(on_hull)
    # num_in = len(in_hull)
    # print(f'Number of points on convex hull: {num_on}')
    # print(f'Points on hull: {ind_on_hull}')
    # print(f'Number of points **in** convex hull: {num_in}')

    # res = list(compute_cells(data, buff=buff, draw=False).items())
    # res_in_hull = [c for c in res if data[c[0]] in in_hull]
    # sres = sorted(res_in_hull, key=lambda x: x[1], reverse=True)
    # print('sres:', sres[:6])
    # area = sres[0][1]
    # print(f'Largest finite cell area: {area}')

    #
    # Part 1
    #

    if part1:
        res0 = compute_cells(data, buff=9, draw=False)

        for d in range(10, 11):
            res1 = compute_cells(data, buff=d, draw=False)
            delta = {i: (a1-res0[i])
                     for i, a1 in res1.items()
                     if a1 != res0[i]}
            res0 = res1

            # print('')
            # print(f'Delta {d}:')
            # for i, a in delta.items():
            #     if i in ind_on_hull:
            #         print(f'Point (*) {i:3} increased by {a:5}')
            #     else:
            #         print(f'Point     {i:3} increased by {a:5}')

        print('Asymptotically increasing points: ( = on hull? )')
        new_on_hull = set([i for i, _ in delta.items()])
        print(new_on_hull)
        print('')

        res_in_hull = [(i, a) for i, a in res1.items() if i not in new_on_hull]
        sres = sorted(res_in_hull, key=lambda x: x[1], reverse=True)
        print('sres:', sres[:6])
        area = sres[0][1]
        print(f'Largest finite cell area: {area}')

    #
    # Part 2
    #

    if part2:

        # compute the safe area inside a tight bpounding box first, then
        # expand by computing safe area along adjoining edges

        bounding_box = set(make_grid(ll_corner, ur_corner))
        total_area = compute_safe_area(data, bounding_box)
        print(f'Safe area inside bbox: {total_area}')
        for d in range(1, 5):
            # new lower edge
            le = make_grid((ll_corner[0]-d, ll_corner[1]-d),
                           (ur_corner[0]+d, ll_corner[1]-d))
            # new upper edge
            ue = make_grid((ll_corner[0]-d, ur_corner[1]+d),
                           (ur_corner[0]+d, ur_corner[1]+d))
            # new left edge
            lfe = make_grid((ll_corner[0]-d, ll_corner[1]-d),
                            (ll_corner[0]-d, ur_corner[1]+d))
            # new right edge
            rte = make_grid((ur_corner[0]+d, ll_corner[1]-d),
                            (ur_corner[0]+d, ur_corner[1]+d))
            new_edges = set(le).union(set(ue), set(lfe), set(rte))
            new_area = compute_safe_area(data, new_edges)
            total_area = total_area + new_area
            print(f'd {d}  new pts {len(new_edges)}  new area {new_area:6}'
                  f'  total {total_area:6}')


if __name__ == '__main__':
    main()
