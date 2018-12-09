import datetime
import re
from typing import Any, List, Dict, Tuple, Union


# test data
test_data_s: str = """[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"""

test_data: List[str] = test_data_s.split('\n')

#
# Handy Types
#

EventTime = datetime.datetime  # incl. ymdhm
Day = datetime.datetime        # only  ymd
# 0 = asleep, 1 = awake
GuardState = int
GuardID = int
# action strings -> either int or bool (depending on key)
ParsedAction = Dict[str, Union[GuardID, bool]]
# date/time -> action that happened starting then
ParsedEvent = Tuple[EventTime, ParsedAction]


def parse_line(inp: str) -> ParsedEvent:
    """ Returns (year,month,day,hour,min,action) """

    datestr, _, rest = inp.partition(']')
    dt = datetime.datetime.strptime(datestr, '[%Y-%m-%d %H:%M')
    act = rest.lstrip()

    i = re.match(r'Guard #(\d+) begins shift', act)
    if i:
        p = {'Guard': int(i.group(1))}
    elif re.match(r'falls asleep', act):
        p = {'falls asleep': True}
    elif re.match(r'wakes up', act):
        p = {'wakes up': True}
    else:
        raise Exception('foobar')

    return (dt, p)


def organize_days(xs: List[ParsedEvent]) -> \
                  Dict[Day, List[ParsedEvent]]:
    """
    Takes a list of events and organizes them by day, fixing the entries
    which occur in hour 23 of a previous day to appear that they took place at
    00:00 on the next.
    """
    res: Dict[Day, List[ParsedEvent]] = {}
    delta_day = datetime.timedelta(1)
    for x in xs:

        # fix hour 23 guard events to start at next day 00:00
        dt = x[0]
        if dt.hour == 23:
            dt = dt + delta_day
            dt = dt.replace(hour=0, minute=0)

        # keep only the day
        dt = datetime.datetime(dt.year, dt.month, dt.day)

        if dt not in res:
            res[dt] = [x]
        else:
            res[dt].append(x)

    return res


def process_events(xs: List[ParsedEvent]) -> \
                   Tuple[GuardID, Dict[int, GuardState]]:
    """
    Interpret a list of events from each day, returning the guard ID of the
    guard on duty as well as a map from minutes to conscious state.
    """
    if len(xs) == 0:
        raise Exception('No events to process!')
    xs_sorted = sorted(xs, key=lambda x: x[0])

    gtuple = xs_sorted[0]
    gtime, gact = gtuple

    ##########################
    # Event processing state
    guard_log: Dict[int, GuardState] = {}
    last_time: int = -1
    last_state: GuardState = -1
    ##########################

    if 'Guard' in gact:
        gid: int = gact['Guard']
        last_time = gtime.minute
        last_state = 1  # awake!
        for m in range(last_time):
            guard_log[m] = -1  # asleep or absent
    else:
        raise Exception('No guard on duty!')

    # process rest of events
    for evt_time, evt in xs_sorted[1:]:
        evt_min = evt_time.minute

        if 'falls asleep' in evt:
            if last_state != 1:
                raise Exception('Guard fell asleep while absent or asleep!')
            for m in range(last_time, evt_min):
                guard_log[m] = 1  # awake
            last_time = evt_min
            last_state = 0  # asleep

        elif 'wakes up' in evt:
            if last_state != 0:
                raise Exception('Guard woke up while awake!')
            for m in range(last_time, evt_min):
                guard_log[m] = 0  # asleep
            last_time = evt_min
            last_state = 1  # awake

        else:
            raise Exception('Invalid action while guard on duty:', evt)

    # fill the remainder of the log
    for m in range(last_time, 60):
        guard_log[m] = last_state

    return (gid, guard_log)


def find_max_value(d: Dict[Any, int]) -> Tuple[Any, int]:
    ditems = list(d.items())
    return max(*ditems, key=lambda x: x[1])


def main():
    with open('input4') as f:
        inp: str = f.readlines()
    xs = map(parse_line, inp)
    ds = organize_days(xs)

    gl = {}
    for day, ls in ds.items():
        gid, log = process_events(ls)
        # accumulate guard logs
        if gid not in gl:
            gl[gid] = [log]
        else:
            gl[gid].append(log)

    # count sleeping minutes
    sleepy_mins: Dict[GuardID, int] = {}
    sleepiest_min: Dict[GuardID, int] = {}
    for gid, lgs in gl.items():
        asleep_histo: Dict[int, int] = {}
        for l in lgs:
            for m, state in l.items():
                if state == 0:
                    if m in asleep_histo:
                        asleep_histo[m] = asleep_histo[m] + 1
                    else:
                        asleep_histo[m] = 1

        # sum minutes asleep
        s: int = 0
        for _, v in asleep_histo.items():
            s = s + v
        sleepy_mins[gid] = s

        # find minute that  guard is most often asleep
        if asleep_histo:  # a guard might never fall asleep!
            sleepiest_min[gid] = find_max_value(asleep_histo)

    # Solution for part 1
    gid, amt = find_max_value(sleepy_mins)
    print('Guard', gid, 'asleep longest for', amt, 'mins')
    print('Guard', gid, 'asleep at min', sleepiest_min[gid][0], 'most often')

    # Solution for part 2
    sgm = list(sleepiest_min.items())
    sgm_max = max(*sgm, key=lambda x: x[1][1])
    print('Guard', sgm_max[0], 'asleep at min', sgm_max[1][0], 'most at',
          sgm_max[1][1], 'minutes')


if __name__ == '__main__':
    main()
